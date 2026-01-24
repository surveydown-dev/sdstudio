# Main server function
studio_server <- function(gssencmode = "prefer") {
  function(input, output, session) {
    # Reactive values for modify content state
    modify_form_trigger <- shiny::reactiveVal(NULL)
    modify_content_info <- shiny::reactiveVal(NULL)
    add_content_page_id <- shiny::reactiveVal(NULL)
    add_form_trigger <- shiny::reactiveVal(NULL)
    
    # Flag to prevent automatic button updates during user-initiated mode switch
    suppress_auto_button_update <- shiny::reactiveVal(FALSE)

    # Dashboard reactive values - Load .env if exists
    if (file.exists(".env")) {
      dotenv::load_dot_env(".env")
    }
    
    # Initialize database connection inputs with .env values on startup
    shiny::observeEvent(shiny::reactiveVal(TRUE)(), {
      # Only run this once on startup
      if (file.exists(".env")) {
        # Update input values with current environment variables
        shiny::updateTextInput(session, "host", value = Sys.getenv("SD_HOST", ""))
        shiny::updateTextInput(session, "port", value = Sys.getenv("SD_PORT", ""))
        shiny::updateTextInput(session, "dbname", value = Sys.getenv("SD_DBNAME", ""))
        shiny::updateTextInput(session, "user", value = Sys.getenv("SD_USER", ""))
        shiny::updateTextInput(session, "password", value = Sys.getenv("SD_PASSWORD", ""))
        shiny::updateTextInput(session, "default_table", value = Sys.getenv("SD_TABLE", ""))
      }
    }, once = TRUE)
    
    # Reactive values for dashboard connection status and database
    rv <- shiny::reactiveValues(
      connection_status = FALSE,
      current_db = NULL,
      initial_connection = TRUE,  # Track if this is initial connection vs test connection
      gssapi_enabled = FALSE,     # Track if GSSAPI is currently enabled
      connection_attempted = FALSE, # Track if connection has been attempted
      current_mode = "live",      # Track current mode: "live" or "local"
      database_tables = c(),      # Store database tables
      csv_files = c(),            # Store CSV files
      placeholder_table = NULL    # Store current placeholder table name
    )

    # Function to update connection state indicator
    update_connection_indicator <- function(status, gssapi_enabled = FALSE, attempted = TRUE) {
      rv$connection_attempted <- attempted
      
      if (status && gssapi_enabled) {
        # Green: successful connection with GSSAPI on
        icon_html <- '<i class="fas fa-circle fa-xs text-success"></i>'
        text <- "Connected (GSSAPI enabled)"
        text_class <- "text-success small"
      } else if (status && !gssapi_enabled) {
        # Yellow: successful connection with GSSAPI off
        icon_html <- '<i class="fas fa-circle fa-xs text-warning"></i>'
        text <- "Connected (GSSAPI disabled)"
        text_class <- "text-warning small"
      } else if (attempted && !status) {
        # Red: connection attempted and failed
        icon_html <- '<i class="fas fa-circle fa-xs text-danger"></i>'
        text <- "Connection failed"
        text_class <- "text-danger small"
      } else {
        # Gray: connection not started
        icon_html <- '<i class="fas fa-circle fa-xs text-secondary"></i>'
        text <- "Not connected"
        text_class <- "text-muted small"
      }
      
      # Update the UI elements
      session$sendCustomMessage("updateConnectionIndicator", list(
        icon = icon_html,
        text = text,
        textClass = text_class
      ))
    }

    # Dashboard connection management
    attempt_connection <- function(config = NULL, return_details = FALSE, gss_mode = gssencmode) {
      # Helper function to check if error is GSSAPI-related or connection failure
      is_gssapi_error <- function(error_msg) {
        grepl("invalid response to GSSAPI negotiation|gssapi|authentication failed|connection failed", error_msg, ignore.case = TRUE)
      }
      
      # Helper function to try connection with specific gssencmode
      try_connection <- function(gss_mode) {
        if (is.null(config)) {
          # Use default connection from .env with the specified gssencmode
          surveydown::sd_db_connect(gssencmode = gss_mode)
        } else {
          # Use provided config with the specified gssencmode
          pool <- pool::dbPool(
            RPostgres::Postgres(),
            host = config$host,
            dbname = config$dbname,
            port = config$port,
            user = config$user,
            password = config$password,
            gssencmode = gss_mode
          )
          list(db = pool)
        }
      }
      
      # Handle auto mode by trying prefer first, then disable
      if (gss_mode == "auto") {
        # Try prefer first
        prefer_result <- tryCatch({
          db <- try_connection("prefer")
          
          if (!is.null(db)) {
            rv$connection_status <- TRUE
            rv$current_db <- db
            rv$gssapi_enabled <- TRUE
            update_connection_indicator(TRUE, gssapi_enabled = TRUE)
            if (return_details) {
              return(list(success = TRUE, fallback_used = FALSE, message = "Connection successful"))
            } else {
              return(TRUE)
            }
          }
          NULL  # Return NULL if db is null
        }, error = function(e) {
          "error"  # Return "error" to indicate prefer mode failed
        })
        
        # If prefer mode succeeded, we already returned above
        if (is.null(prefer_result) || prefer_result == "error") {
          message("Connection failed with gssencmode='prefer', trying 'disable'...")
          
          # Try disable as fallback
          disable_result <- tryCatch({
            db <- try_connection("disable")
            
            if (!is.null(db)) {
              rv$connection_status <- TRUE
              rv$current_db <- db
              rv$gssapi_enabled <- FALSE
              update_connection_indicator(TRUE, gssapi_enabled = FALSE)
              message("Connection successful with gssencmode='disable'")
              if (return_details) {
                return(list(success = TRUE, fallback_used = TRUE, 
                           message = "Connection successful (GSSAPI disabled due to connection error)"))
              } else {
                return(TRUE)
              }
            }
            NULL  # Return NULL if db is null
          }, error = function(e2) {
            "error"  # Return "error" to indicate disable mode also failed
          })
          
          # If we reach here and disable didn't succeed, both attempts failed
          if (is.null(disable_result) || disable_result == "error") {
            rv$connection_status <- FALSE
            rv$current_db <- NULL
            rv$gssapi_enabled <- FALSE
            update_connection_indicator(FALSE, gssapi_enabled = FALSE)
            warning("Connection failed with both gssencmode='prefer' and 'disable'")
            if (return_details) {
              return(list(success = FALSE, fallback_used = TRUE, 
                         message = "Connection failed with both prefer and disable modes"))
            } else {
              return(FALSE)
            }
          }
        }
      } else {
        # For non-auto modes, use original logic
        tryCatch({
          db <- try_connection(gss_mode)
          
          if (!is.null(db)) {
            rv$connection_status <- TRUE
            rv$current_db <- db
            rv$gssapi_enabled <- (gss_mode == "prefer")
            update_connection_indicator(TRUE, gssapi_enabled = (gss_mode == "prefer"))
            if (return_details) {
              return(list(success = TRUE, fallback_used = FALSE, message = "Connection successful"))
            } else {
              return(TRUE)
            }
          }
          rv$connection_status <- FALSE
          rv$current_db <- NULL
          rv$gssapi_enabled <- FALSE
          update_connection_indicator(FALSE, gssapi_enabled = FALSE)
          if (return_details) {
            return(list(success = FALSE, fallback_used = FALSE, message = "Connection failed"))
          } else {
            return(FALSE)
          }
        }, error = function(e) {
          error_msg <- as.character(e$message)
          
          # If this is a GSSAPI error and we're using "prefer", try with "disable"
          if (is_gssapi_error(error_msg) && gss_mode == "prefer") {
            message("GSSAPI negotiation failed, retrying with gssencmode='disable'...")
            
            tryCatch({
              db <- try_connection("disable")
              
              if (!is.null(db)) {
                rv$connection_status <- TRUE
                rv$current_db <- db
                rv$gssapi_enabled <- FALSE
                update_connection_indicator(TRUE, gssapi_enabled = FALSE)
                message("Connection successful with gssencmode='disable'")
                if (return_details) {
                  return(list(success = TRUE, fallback_used = TRUE, 
                             message = "Connection successful (GSSAPI disabled due to negotiation error)"))
                } else {
                  return(TRUE)
                }
              }
              rv$connection_status <- FALSE
              rv$current_db <- NULL
              rv$gssapi_enabled <- FALSE
              update_connection_indicator(FALSE, gssapi_enabled = FALSE)
              if (return_details) {
                return(list(success = FALSE, fallback_used = TRUE, 
                           message = "Connection failed with both GSSAPI modes"))
              } else {
                return(FALSE)
              }
            }, error = function(e2) {
              # Both attempts failed
              rv$connection_status <- FALSE
              rv$current_db <- NULL
              rv$gssapi_enabled <- FALSE
              update_connection_indicator(FALSE, gssapi_enabled = FALSE)
              warning("Connection failed with both gssencmode='prefer' and 'disable': ", e2$message)
              if (return_details) {
                return(list(success = FALSE, fallback_used = TRUE, 
                           message = paste("Connection failed with both GSSAPI modes:", e2$message)))
              } else {
                return(FALSE)
              }
            })
          } else {
            # Not a GSSAPI error or already using "disable", just fail normally
            rv$connection_status <- FALSE
            rv$current_db <- NULL
            rv$gssapi_enabled <- FALSE
            update_connection_indicator(FALSE, gssapi_enabled = FALSE)
            if (return_details) {
              return(list(success = FALSE, fallback_used = FALSE, message = error_msg))
            } else {
              return(FALSE)
            }
          }
        })
      }
    }

    # Functions for local CSV file handling
    update_csv_files <- function() {
      csv_files <- list.files(pattern = "\\.csv$", full.names = FALSE)
      if (length(csv_files) == 0) {
        rv$csv_files <- c("No CSV files found" = "")
      } else {
        # Put preview_data.csv first if it exists
        if ("preview_data.csv" %in% csv_files) {
          csv_files <- c("preview_data.csv", setdiff(csv_files, "preview_data.csv"))
        }
        rv$csv_files <- csv_files
      }
    }
    
    # Function to refresh response data based on current mode
    refresh_response_data <- function() {
      if (rv$current_mode == "local") {
        update_csv_files()
        update_table_dropdown()
        
        # Force refresh of survey data by invalidating the reactive
        # This is done by briefly changing the table selection to trigger data refresh
        current_selection <- input$table_select
        if (!is.null(current_selection) && current_selection != "") {
          # Temporarily change to empty and back to force reactive update
          shiny::updateSelectInput(session, "table_select", selected = "")
          later::later(function() {
            shiny::updateSelectInput(session, "table_select", selected = current_selection)
          }, 0.1)
        }
      } else {
        # Refresh database tables and data
        if (rv$connection_status && !is.null(rv$current_db)) {
          # Force refresh of database tables (same as Test Connection does)
          update_database_tables()
          update_table_dropdown()
          
          # Update connection indicator to show current status
          update_connection_indicator(rv$connection_status, rv$gssapi_enabled, TRUE)
          
          # Also invalidate the survey_data reactive to force data reload
          # This is done by briefly changing the table selection to trigger data refresh
          current_selection <- input$table_select
          if (!is.null(current_selection) && current_selection != "") {
            # Temporarily change to empty and back to force reactive update
            shiny::updateSelectInput(session, "table_select", selected = "")
            later::later(function() {
              shiny::updateSelectInput(session, "table_select", selected = current_selection)
            }, 0.1)
          }
        } else {
          message("No database connection available for refresh")
          # Attempt to reconnect if we have environment variables
          if (file.exists(".env")) {
            dotenv::load_dot_env(".env")
          }
          
          host <- Sys.getenv("SD_HOST", "")
          dbname <- Sys.getenv("SD_DBNAME", "")
          user <- Sys.getenv("SD_USER", "")
          
          if (host != "" && dbname != "" && user != "") {
            message("Attempting to reconnect...")
            result <- attempt_connection(config = NULL, return_details = TRUE, gss_mode = "auto")
            if (result$success) {
              message("Reconnection successful, updating data...")
              update_database_tables()
              update_table_dropdown()
              # Update connection indicator after successful reconnection
              update_connection_indicator(rv$connection_status, rv$gssapi_enabled, TRUE)
            } else {
              # Update connection indicator to show failed status
              update_connection_indicator(FALSE, FALSE, TRUE)
            }
          } else {
            # Update connection indicator to show not connected status
            update_connection_indicator(FALSE, FALSE, TRUE)
          }
        }
      }
    }
    
    read_local_csv <- function(filename) {
      if (is.null(filename) || filename == "" || filename == "No CSV files found") {
        return(NULL)
      }
      
      # Check if file exists before trying to read it
      if (!file.exists(filename)) {
        return(NULL)
      }
      
      tryCatch({
        utils::read.csv(filename, stringsAsFactors = FALSE)
      }, error = function(e) {
        return(NULL)
      })
    }
    
    
    # Initialize connection indicator to gray state and ensure proper mode UI
    shiny::observe({
      update_connection_indicator(FALSE, gssapi_enabled = FALSE, attempted = FALSE)
      
      # Ensure UI is in the correct state on startup (live mode by default)
      session$sendCustomMessage("updateModeUI", list(mode = rv$current_mode))
    }, priority = 1000)
    
    # Conditional file watcher - only active when needed
    file_watcher_active <- shiny::reactiveVal(FALSE)
    
    # Create file watcher that only runs when activated
    app_file_watcher <- shiny::reactive({
      if (!file_watcher_active() || !file.exists("app.R")) return(NULL)
      
      shiny::reactivePoll(
        intervalMillis = 1000,  # Check every 1 second when active
        session = session,
        checkFunc = function() {
          if (!file.exists("app.R")) return(Sys.time())
          file.mtime("app.R")
        },
        valueFunc = function() {
          if (!file.exists("app.R")) return(NULL)
          readLines("app.R", warn = FALSE)
        }
      )()
    })
    
    # React to app.R changes only when file actually changes and watcher is active
    shiny::observe({
      if (!file_watcher_active()) return()
      
      app_content <- app_file_watcher()
      
      if (survey_exists() && !is.null(app_content) && !suppress_auto_button_update()) {
        detected_mode <- detect_app_mode()
        
        # Update internal mode state if it differs from detected mode
        if (rv$current_mode != detected_mode) {
          rv$current_mode <- detected_mode
          
          # Update button states via JavaScript
          session$sendCustomMessage("updateCodeModeButtons", list(
            mode = detected_mode
          ))
        }
      }
    })
    
    # Activate file watcher ONLY when on Build tab AND actively editing app.R
    shiny::observe({
      # Monitor only if: Build tab + app.R tab active
      should_be_active <- !is.null(input$tabset) && 
                         input$tabset == "Build" && 
                         !is.null(input$code_tabs) && 
                         input$code_tabs == "app.R"
      
      file_watcher_active(should_be_active)
    })

    # Initial connection attempt (only if .env file exists)
    shiny::observe({
      if (file.exists(".env")) {
        # Set placeholder table from .env file if it exists
        env_table <- Sys.getenv("SD_TABLE", "")
        if (env_table != "") {
          rv$placeholder_table <- env_table
        }
        
        message(paste("Attempting initial database connection with gssencmode=", gssencmode, "..."))
        attempt_connection(config = NULL, return_details = FALSE, gss_mode = gssencmode)
      } else {
        message("No .env file found. Skipping initial database connection attempt.")
        # Set connection indicator to show not connected status
        update_connection_indicator(FALSE, gssapi_enabled = FALSE, attempted = FALSE)
      }
    })

    # Update database tables when connection is established  
    update_database_tables <- function() {
      if (rv$connection_status && !is.null(rv$current_db)) {
        tryCatch({
          tables <- pool::poolWithTransaction(rv$current_db$db, function(conn) {
            all_tables <- DBI::dbListTables(conn)
            all_tables[!grepl("^pg_", all_tables)]
          })

          # Start with existing tables
          all_choices <- tables
          
          # Add placeholder table if it exists and is not already in the list
          if (!is.null(rv$placeholder_table) && rv$placeholder_table != "" && 
              !rv$placeholder_table %in% tables) {
            all_choices <- c(rv$placeholder_table, tables)
          }

          rv$database_tables <- if (length(all_choices) > 0) all_choices else c("No tables found" = "")
        }, error = function(e) {
          rv$database_tables <- c("Connection error" = "")
        })
      } else {
        rv$database_tables <- c("No connection" = "")
      }
    }
    
    # Update table selection dropdown based on current mode
    update_table_dropdown <- function() {
      if (rv$current_mode == "local") {
        # Local mode: show CSV files
        selected_file <- if ("preview_data.csv" %in% rv$csv_files) {
          "preview_data.csv"
        } else if (length(rv$csv_files) > 0 && !is.null(names(rv$csv_files)) && names(rv$csv_files)[1] != "No CSV files found") {
          rv$csv_files[1]
        } else if (length(rv$csv_files) > 0 && is.null(names(rv$csv_files))) {
          rv$csv_files[1]
        } else {
          NULL
        }
        
        shiny::updateSelectizeInput(session, "table_select",
                                    choices = rv$csv_files,
                                    selected = selected_file
        )
      } else {
        # Live mode: show database tables
        # Prioritize placeholder table, then default from env, then first table
        selected_table <- if (!is.null(rv$placeholder_table) && rv$placeholder_table != "" && 
                             rv$placeholder_table %in% rv$database_tables) {
          rv$placeholder_table
        } else {
          default_table <- Sys.getenv("SD_TABLE", "")
          if (default_table != "" && default_table %in% rv$database_tables) {
            default_table
          } else if (length(rv$database_tables) > 0 && !is.null(names(rv$database_tables)) && names(rv$database_tables)[1] != "No tables found") {
            rv$database_tables[1]
          } else if (length(rv$database_tables) > 0 && is.null(names(rv$database_tables))) {
            rv$database_tables[1]
          } else {
            NULL
          }
        }
        
        shiny::updateSelectizeInput(session, "table_select",
                                    choices = rv$database_tables,
                                    selected = selected_table
        )
      }
    }
    
    # Observer to update CSV files and refresh dropdown
    shiny::observe({
      update_csv_files()
      if (rv$current_mode == "local") {
        update_table_dropdown()
      }
    })
    
    # Observer to update database tables when connection changes (only for initial connection)
    shiny::observe({
      if (!rv$initial_connection) {
        return()
      }
      
      update_database_tables()
      if (rv$current_mode == "live") {
        update_table_dropdown()
      }
      
      # After initial connection, mark as no longer initial
      if (rv$connection_status && !is.null(rv$current_db)) {
        rv$initial_connection <- FALSE
      }
    })

    # Handle test connection button
    shiny::observeEvent(input$test_connection, {
      # Close existing connection
      if (!is.null(rv$current_db) && !is.null(rv$current_db$db)) {
        tryCatch({
          pool::poolClose(rv$current_db$db)
          rv$current_db <- NULL
          rv$connection_status <- FALSE
        }, error = function(e) {
          warning("Error closing connection: ", e$message)
        })
      }

      # Test new connection
      config <- list(
        host = input$host,
        port = input$port,
        dbname = input$dbname,
        user = input$user,
        password = input$password
      )

      result <- attempt_connection(config, return_details = TRUE)

      if (result$success) {
        # Save to .env file
        env_content <- paste(
          "# Database connection settings for surveydown",
          sprintf("SD_HOST=%s", input$host),
          sprintf("SD_PORT=%s", input$port),
          sprintf("SD_DBNAME=%s", input$dbname),
          sprintf("SD_USER=%s", input$user),
          sprintf("SD_PASSWORD=%s", input$password),
          sprintf("SD_TABLE=%s", input$default_table),
          sep = "\n"
        )
        writeLines(env_content, ".env")

        # Update .gitignore
        if (file.exists(".gitignore")) {
          gitignore_content <- readLines(".gitignore")
          if (!".env" %in% gitignore_content) {
            write("\n.env", ".gitignore", append = TRUE)
          }
        } else {
          write(".env", ".gitignore")
        }

        # Set placeholder table from the default_table input
        if (input$default_table != "") {
          rv$placeholder_table <- input$default_table
        }
        
        # Update database tables after successful test connection
        if (rv$connection_status && !is.null(rv$current_db)) {
          update_database_tables()
          if (rv$current_mode == "live") {
            update_table_dropdown()
          }
        }
        
        # Provide detailed feedback about the connection
        status_message <- paste("Connection successful & Parameters saved to .env file.")
        if (result$fallback_used) {
          status_message <- paste(status_message, "\nNote: GSSAPI was automatically disabled due to negotiation error.")
        }
        
        output$connection_status <- shiny::renderText(status_message)
      } else {
        output$connection_status <- shiny::renderText(
          paste("Connection failed:", result$message)
        )
      }
    }, ignoreInit = TRUE)


    # Reactive survey data with error handling for both local and live modes
    survey_data <- shiny::reactive({
      shiny::req(input$table_select)
      
      if (rv$current_mode == "local") {
        # Local mode: read CSV file
        if (input$table_select == "" || input$table_select == "No CSV files found") {
          return(NULL)
        }
        return(read_local_csv(input$table_select))
      } else {
        # Live mode: read from database
        shiny::req(rv$connection_status)
        shiny::req(rv$current_db)

        tryCatch({
          data <- pool::poolWithTransaction(rv$current_db$db, function(conn) {
            DBI::dbGetQuery(conn, sprintf('SELECT * FROM "%s"', input$table_select))
          })
          return(data)
        }, error = function(e) {
          # Just return NULL for any error - this will leave dashboard empty
          return(NULL)
        })
      }
    })

    # Dashboard outputs
    # Downloadable CSV of survey data
    output$download_survey_data <- shiny::downloadHandler(
      filename = function() {
        paste0(input$table_select, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- survey_data()
        utils::write.csv(data, file, row.names = FALSE)
      }
    )

    # Value Boxes
    output$total_responses <- shiny::renderText({
      shiny::req(survey_data())
      nrow(survey_data())
    })

    output$daily_average <- shiny::renderText({
      shiny::req(survey_data())
      data <- survey_data()
      start_times <- as.POSIXct(data$time_start, format="%Y-%m-%d %H:%M:%S")
      first_response <- min(start_times, na.rm = TRUE)
      last_response <- max(start_times, na.rm = TRUE)
      duration_days <- max(as.numeric(difftime(last_response, first_response, units = "days")), 1)
      round(nrow(data) / duration_days, 1)
    })

    output$completion_rate <- shiny::renderText({
      shiny::req(survey_data())
      data <- survey_data()
      total_responses <- nrow(data)
      completed_responses <- sum(!is.na(data$time_end) & data$time_end != "", na.rm = TRUE)
      if(total_responses > 0) {
        sprintf("%.1f%%", (completed_responses / total_responses) * 100)
      } else {
        "0.0%"
      }
    })

    # Response Trend Plots
    output$cumulative_trend <- shiny::renderPlot({
      data <- survey_data()
      # Use default theme colors
      bg_color <- "#ffffff"
      text_color <- "#1a2226"
      grid_color <- "gray80"
      line_color <- "#0062cc"

      if (is.null(data) || nrow(data) == 0 || !("time_start" %in% names(data))) {
        graphics::par(bg = bg_color, fg = text_color)
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No data available to display", col = text_color, cex = 1.2)
        return()
      }

      dates <- try(as.Date(data$time_start))
      if (inherits(dates, "try-error") || length(dates) == 0 || all(is.na(dates))) {
        graphics::par(bg = bg_color, fg = text_color)
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Unable to process date data", col = text_color, cex = 1.2)
        return()
      }

      dates <- dates[!is.na(dates)]
      daily_counts <- table(dates)
      date_range <- seq(min(dates), max(dates), by = "day")
      all_counts <- integer(length(date_range))
      names(all_counts) <- date_range
      all_counts[names(daily_counts)] <- daily_counts
      cumulative_responses <- cumsum(all_counts)

      # Plot setup with theme
      oldpar <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(oldpar))
      graphics::par(bg = bg_color, fg = text_color, col.axis = text_color, col.lab = text_color,
          mar = c(4, 4, 2, 2))

      # Add grid first
      graphics::plot(date_range, cumulative_responses, type = "n",
           xlab = "Date", ylab = "Cumulative Responses")
      graphics::grid(col = grid_color, lty = "dotted")

      # Add line and points
      graphics::lines(date_range, cumulative_responses, col = line_color, lwd = 2)
      graphics::points(date_range, cumulative_responses, col = line_color, pch = 16)
    }, bg = "transparent")

    output$daily_trend <- shiny::renderPlot({
      data <- survey_data()
      # Use default theme colors
      bg_color <- "#ffffff"
      text_color <- "#1a2226"
      grid_color <- "gray80"
      bar_color <- "#0062cc"
      bar_border <- "#00008B"

      if (is.null(data) || nrow(data) == 0 || !("time_start" %in% names(data))) {
        graphics::par(bg = bg_color, fg = text_color)
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No data available to display", col = text_color, cex = 1.2)
        return()
      }

      dates <- try(as.Date(data$time_start))
      if (inherits(dates, "try-error") || length(dates) == 0 || all(is.na(dates))) {
        graphics::par(bg = bg_color, fg = text_color)
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Unable to process date data", col = text_color, cex = 1.2)
        return()
      }

      dates <- dates[!is.na(dates)]
      daily_counts <- table(dates)
      date_range <- seq(min(dates), max(dates), by = "day")
      all_counts <- integer(length(date_range))
      names(all_counts) <- date_range
      all_counts[names(daily_counts)] <- daily_counts

      # Plot setup with theme
      oldpar <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(oldpar))
      graphics::par(bg = bg_color, fg = text_color, col.axis = text_color, col.lab = text_color,
          mar = c(5, 4, 2, 2))  # Increased bottom margin for date labels

      # Create barplot
      bp <- graphics::barplot(all_counts,
                    col = bar_color,
                    border = bar_border,
                    xlab = "Date",
                    ylab = "Daily Responses",
                    xaxt = "n",
                    space = 0.2
      )

      # Add gridlines
      graphics::grid(col = grid_color, lty = "dotted")

      # Add x-axis with rotated labels
      graphics::axis(1,
           at = bp,
           labels = format(date_range, "%b %d"),
           las = 2,
           col.axis = text_color,
           cex.axis = 0.9
      )
    }, bg = "transparent")

    # Survey Data Table
    output$survey_data_table <- DT::renderDataTable({
      shiny::req(survey_data())
      data <- survey_data()
      DT::datatable(
        data,
        extensions = 'Scroller',
        options = list(
          dom = 'Bfrtip',
          scrollX = TRUE,
          scrollY = '400px',
          scroller = TRUE,
          pageLength = 50
        ),
        class = 'cell-border stripe'
      )
    })

    # Reactive value to track if survey exists
    survey_exists <- shiny::reactiveVal(FALSE)
    
    # Check if survey files exist on startup
    shiny::observe({
      survey_exists(file.exists("survey.qmd") && file.exists("app.R"))
    })
    
    # Render build tab content conditionally
    output$build_tab_content <- shiny::renderUI({
      if (survey_exists()) {
        ui_normal_build()
      } else {
        ui_template_selection()
      }
    })
    
    # Initialize path input on startup
    shiny::observe({
      current_dir <- getwd()
      shiny::updateTextInput(session, "path_input", value = current_dir)
    }, priority = 1000)
    
    # Handle directory button click
    shiny::observeEvent(input$path_display_btn, {
      # Update the modal input with current path
      shiny::updateTextInput(session, "path_edit_input", value = input$path_input)
      # Show the modal
      session$sendCustomMessage("showModal", "edit-directory-modal")
    })
    
    # Handle directory path confirmation
    shiny::observeEvent(input$confirm_path_edit, {
      new_path <- input$path_edit_input
      
      # Validate the path
      if (is.null(new_path) || trimws(new_path) == "") {
        shiny::showNotification("Please enter a valid directory path", type = "error")
        return()
      }
      
      # Expand tilde and normalize path
      expanded_path <- path.expand(trimws(new_path))
      normalized_path <- normalizePath(expanded_path, mustWork = FALSE)
      
      # Check if parent directory exists
      parent_dir <- dirname(normalized_path)
      if (!dir.exists(parent_dir)) {
        shiny::showNotification("Parent directory does not exist!", type = "error")
        return()
      }
      
      # Update the hidden input and button display
      shiny::updateTextInput(session, "path_input", value = normalized_path)
      session$sendCustomMessage("updatePathButton", list(
        path = normalized_path,
        display = basename(normalized_path)
      ))
      
      # Hide the modal
      session$sendCustomMessage("hideModal", "edit-directory-modal")
      
      # Show success notification
      shiny::showNotification("Directory path updated successfully", type = "message")
    })

    # Handle create survey button
    shiny::observeEvent(input$create_survey_btn, {
      req(input$template_select, input$path_input)

      # Validate path
      if (!dir.exists(dirname(input$path_input))) {
        shiny::showNotification("Parent directory does not exist!", type = "error")
        return()
      }

      # Create the directory if it doesn't exist
      if (!dir.exists(input$path_input)) {
        tryCatch({
          dir.create(input$path_input, recursive = TRUE)
        }, error = function(e) {
          shiny::showNotification(paste("Failed to create directory:", e$message), type = "error")
          return()
        })
      }

      # Create the survey with full path first
      tryCatch({
        surveydown::sd_create_survey(
          template = input$template_select,
          path = input$path_input,
          ask = FALSE
        )

        # Change to the target directory after creation
        # Note: This working directory change is scoped to the Shiny session
        # and does not affect the user's R environment outside the app
        setwd(input$path_input)
        survey_exists(TRUE)
      }, error = function(e) {
        shiny::showNotification(paste("Failed to create survey:", e$message), type = "error")
      })
    })

    # Setup survey.qmd editor
    output$survey_editor_ui <- shiny::renderUI({
      if (!survey_exists()) return(NULL)
      survey_content <- paste(readLines("survey.qmd", warn = FALSE), collapse = "\n")
      
      shinyAce::aceEditor(
        outputId = "survey_editor",
        value = survey_content,
        mode = "markdown",
        theme = "textmate",
        height = "calc(100vh - 193px)",
        fontSize = 14,
        wordWrap = TRUE
      )
    })
    
    # Force this output to render even when tab is hidden (eliminates lazy loading)
    shiny::outputOptions(output, "survey_editor_ui", suspendWhenHidden = FALSE)

    # Setup app.R editor
    output$app_editor_ui <- shiny::renderUI({
      if (!survey_exists()) return(NULL)
      app_content <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
      
      shinyAce::aceEditor(
        outputId = "app_editor",
        value = app_content,
        mode = "r",
        theme = "chrome",
        height = "calc(100vh - 193px)",
        fontSize = 14,
        wordWrap = TRUE
      )
    })
    
    # Force this output to render even when tab is hidden (eliminates lazy loading)
    shiny::outputOptions(output, "app_editor_ui", suspendWhenHidden = FALSE)
    
    # Function to detect current mode from app.R content
    detect_app_mode <- function(content = NULL) {
      if (is.null(content)) {
        if (!file.exists("app.R")) return("live")
        content <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
      }
      
      # Look for sd_db_connect patterns
      if (grepl("sd_db_connect\\s*\\(\\s*ignore\\s*=\\s*TRUE", content)) {
        return("local")
      } else if (grepl("sd_db_connect\\s*\\(", content)) {
        return("live")
      }
      
      # No sd_db_connect found - default to live mode (will be added)
      return("live")
    }
    
    # Function to update app.R content based on mode
    update_app_mode <- function(target_mode, current_content = NULL) {
      if (!file.exists("app.R")) return(NULL)
      
      if (is.null(current_content)) {
        current_content <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
      }
      
      # Check if sd_db_connect exists
      has_sd_db_connect <- grepl("sd_db_connect\\s*\\(", current_content)
      
      if (!has_sd_db_connect) {
        # Add sd_db_connect after the last library() call
        lines <- strsplit(current_content, "\n")[[1]]
        
        # Find the last library() call
        library_indices <- grep("^\\s*library\\s*\\(", lines)
        
        if (length(library_indices) > 0) {
          last_library_index <- max(library_indices)
          
          # Prepare the db connection line based on target mode
          if (target_mode == "local") {
            db_line <- "db <- sd_db_connect(ignore = TRUE)"
          } else {
            db_line <- "db <- sd_db_connect()"
          }
          
          # Insert the db connection after the last library call
          new_lines <- c(
            lines[1:last_library_index],
            "",
            db_line,
            "",
            lines[(last_library_index + 1):length(lines)]
          )
          
          return(paste(new_lines, collapse = "\n"))
        } else {
          # No library calls found, add at the beginning
          if (target_mode == "local") {
            db_line <- "db <- sd_db_connect(ignore = TRUE)"
          } else {
            db_line <- "db <- sd_db_connect()"
          }
          
          new_lines <- c(db_line, "", lines)
          return(paste(new_lines, collapse = "\n"))
        }
      } else {
        # sd_db_connect exists, modify it
        if (target_mode == "local") {
          # Change to local mode
          updated_content <- gsub(
            "sd_db_connect\\s*\\([^)]*\\)",
            "sd_db_connect(ignore = TRUE)",
            current_content
          )
        } else {
          # Change to live mode
          updated_content <- gsub(
            "sd_db_connect\\s*\\([^)]*\\)",
            "sd_db_connect()",
            current_content
          )
        }
        
        return(updated_content)
      }
    }
    
    # Initialize button states once when survey is ready (no continuous polling)
    initialization_done <- shiny::reactiveVal(FALSE)
    
    shiny::observe({
      if (survey_exists() && file.exists("app.R") && !initialization_done() && !suppress_auto_button_update()) {
        current_mode <- detect_app_mode()
        
        # Update internal mode state if it differs from detected mode
        if (rv$current_mode != current_mode) {
          rv$current_mode <- current_mode
        }
        
        # Update button states via JavaScript
        session$sendCustomMessage("updateCodeModeButtons", list(
          mode = current_mode
        ))
        
        # Mark initialization as complete to prevent continuous running
        initialization_done(TRUE)
      }
    })
    
    # Monitor app.R editor changes for real-time mode detection
    shiny::observeEvent(input$app_editor, {
      if (survey_exists() && !is.null(input$app_editor) && !suppress_auto_button_update()) {
        current_mode <- detect_app_mode(input$app_editor)
        
        # Update internal mode state if it differs from detected mode
        if (rv$current_mode != current_mode) {
          rv$current_mode <- current_mode
        }
        
        # Update button states via JavaScript
        session$sendCustomMessage("updateCodeModeButtons", list(
          mode = current_mode
        ))
      }
    }, ignoreInit = TRUE)
    
    # Also update when user switches to app.R tab
    shiny::observeEvent(input$code_tabs, {
      if (input$code_tabs == "app.R" && survey_exists() && file.exists("app.R") && !suppress_auto_button_update()) {
        # Small delay to ensure editor is ready
        shiny::invalidateLater(200)
        current_mode <- detect_app_mode()
        
        # Update internal mode state if it differs from detected mode
        if (rv$current_mode != current_mode) {
          rv$current_mode <- current_mode
        }
        
        # Update button states via JavaScript
        session$sendCustomMessage("updateCodeModeButtons", list(
          mode = current_mode
        ))
      }
    })
    
    # Observer to handle mode-dependent UI changes
    shiny::observe({
      current_mode <- rv$current_mode
      # Send message to JavaScript to show/hide database connection section
      session$sendCustomMessage("updateModeUI", list(mode = current_mode))
      # Update table dropdown when mode changes
      update_table_dropdown()
      # Update connection indicator when switching to DB mode
      if (current_mode == "live") {
        update_connection_indicator(rv$connection_status, rv$gssapi_enabled, rv$connection_attempted)
      }
    })
    
    # Handle code mode switching
    shiny::observeEvent(input$code_mode_switch, {
      target_mode <- input$code_mode_switch$mode
      
      # Suppress automatic button updates FIRST to prevent bouncing
      suppress_auto_button_update(TRUE)
      later::later(function() {
        suppress_auto_button_update(FALSE)
      }, 5000)  # Increase to 5 seconds
      
      # Update current mode tracking
      rv$current_mode <- target_mode
      
      if (!survey_exists() || !file.exists("app.R")) {
        # Even if no survey exists, update mode for Responses tab
        return()
      }
      
      current_content <- input$app_editor
      
      # Show overlay and start monitoring if user is on Preview tab
      if (!is.null(input$tabset) && input$tabset == "Preview") {
        session$sendCustomMessage("showRenderingMessage", list())
        # Activate monitoring to detect when rendering is complete
        if (!monitoring_active()) {
          monitoring_active(TRUE)
          survey_html_exists(file.exists("surveydown.lua"))
        }
      }
      
      # Update the content
      updated_content <- update_app_mode(target_mode, current_content)
      
      if (!is.null(updated_content) && updated_content != current_content) {
        # Update the ACE editor
        shinyAce::updateAceEditor(session, "app_editor", value = updated_content)
        
        # Show notification
        mode_text <- if (target_mode == "local") "Local" else "Live"
      }
    })

    # Setup modify content form modal
    output$modify_content_form <- shiny::renderUI({
      form_info <- modify_form_trigger()
      
      if (is.null(form_info)) {
        return(shiny::div("Select content to modify..."))
      }
      
      if (form_info$type == "question") {
        current_item <- form_info$item
        current_type <- if("type" %in% names(current_item) && !is.null(current_item$type)) current_item$type else "mc"
        current_id <- if("id" %in% names(current_item) && !is.null(current_item$id)) current_item$id else ""
        current_label <- if("label" %in% names(current_item) && !is.null(current_item$label)) current_item$label else ""
        
        # Extract current options if it's a choice question
        current_options <- ""
        if (current_type %in% c("mc", "mc_buttons", "mc_multiple", "mc_multiple_buttons", "select", "slider")) {
          if ("raw" %in% names(current_item)) {
            current_options <- extract_options_from_raw(current_item$raw)
          }
        }
        
        # Extract slider_numeric values if it's a slider_numeric question
        current_min <- 0
        current_max <- 10
        current_range <- FALSE
        if (current_type == "slider_numeric" && "raw" %in% names(current_item)) {
          # Parse the raw code to extract seq() parameters
          raw_text <- current_item$raw
          if (grepl("seq\\s*\\(", raw_text)) {
            # Extract seq parameters
            seq_match <- regexpr("seq\\s*\\(([^)]+)\\)", raw_text, perl = TRUE)
            if (seq_match > 0) {
              seq_params <- regmatches(raw_text, seq_match)
              seq_params <- gsub("seq\\s*\\(|\\)", "", seq_params)
              params <- strsplit(seq_params, ",")[[1]]
              if (length(params) >= 2) {
                min_parsed <- suppressWarnings(as.numeric(trimws(params[1])))
                max_parsed <- suppressWarnings(as.numeric(trimws(params[2])))
                if (!is.na(min_parsed) && !is.na(max_parsed)) {
                  current_min <- min_parsed
                  current_max <- max_parsed
                }
              }
            }
          }
          # Check if there's a default parameter for range mode
          if (grepl("default\\s*=\\s*c\\s*\\(", raw_text)) {
            current_range <- TRUE
          }
        }
        
        form_elements <- list(
          shiny::selectInput("modify_question_type", "Question Type:", 
                    choices = c(
                      "Multiple Choice" = "mc",
                      "Text Input" = "text",
                      "Textarea" = "textarea",
                      "Numeric Input" = "numeric",
                      "Multiple Choice Buttons" = "mc_buttons",
                      "Multiple Choice Multiple" = "mc_multiple",
                      "Multiple Choice Multiple Buttons" = "mc_multiple_buttons",
                      "Select Dropdown" = "select",
                      "Slider" = "slider",
                      "Slider Numeric" = "slider_numeric",
                      "Date" = "date",
                      "Date Range" = "daterange"
                    ),
                    selected = current_type),
          shiny::textInput("modify_question_id", "Question ID:", value = current_id),
          shiny::textInput("modify_question_label", "Question Label:", value = current_label)
        )
        
        # Add options input for choice questions
        if (current_type %in% c("mc", "mc_buttons", "mc_multiple", "mc_multiple_buttons", "select", "slider")) {
          form_elements <- append(form_elements, list(
            shiny::div(
              style = "margin-top: 10px;",
              shiny::textAreaInput("modify_question_options", 
                                  "Options:", 
                                  value = current_options,
                                  rows = 3,
                                  placeholder = "Apple, Banana, Cherry")
            )
          ))
        }
        
        # Add slider_numeric specific inputs
        if (current_type == "slider_numeric") {
          form_elements <- append(form_elements, list(
            shiny::div(
              style = "margin-top: 10px;",
              shiny::fluidRow(
                shiny::column(6,
                  shiny::numericInput("modify_slider_min", "Minimum Value:", value = current_min, min = -1000, max = 1000, step = 1)
                ),
                shiny::column(6,
                  shiny::numericInput("modify_slider_max", "Maximum Value:", value = current_max, min = -1000, max = 1000, step = 1)
                )
              )
            ),
            shiny::div(
              style = "margin-top: 10px;",
              shiny::checkboxInput("modify_slider_range", "Range Mode (two handles)", value = current_range)
            )
          ))
        }
        
        form_elements <- append(form_elements, list(
          shiny::div(style = "font-size: 0.8em; color: #666; margin-top: 10px;",
            paste0("Editing question \"", form_info$content_id, "\" on page \"", form_info$page_id, "\".")
          )
        ))
        
        return(shiny::div(form_elements))
        
      } else if (form_info$type == "text") {
        current_item <- form_info$item
        current_content <- if("content" %in% names(current_item) && !is.null(current_item$content)) current_item$content else ""
        
        return(shiny::div(
          shiny::textAreaInput("modify_text_content", "Text:", rows = 3, value = current_content),
          shiny::div(style = "font-size: 0.8em; color: #666; margin-top: 10px;",
            paste0("Editing text on page \"", form_info$page_id, "\".")
          )
        ))
      }
      
      # Fallback
      return(shiny::div("Unknown content type"))
    })

    # Add content form modal
    output$add_content_form <- shiny::renderUI({
      form_info <- add_form_trigger()
      
      if (is.null(form_info)) {
        return(shiny::div("Select content type..."))
      }
      
      content_type <- form_info$content_type
      
      if (content_type == "text") {
        return(shiny::div(
          shiny::textAreaInput("add_text_content", "Text:", rows = 3, 
                              placeholder = "Enter markdown text to add to the page")
        ))
      } else if (content_type == "question") {
        form_elements <- list(
          shiny::selectInput("add_question_type", "Question Type:", 
                            choices = c(
                              "Multiple Choice" = "mc",
                              "Text Input" = "text",
                              "Textarea" = "textarea",
                              "Numeric Input" = "numeric",
                              "Multiple Choice Buttons" = "mc_buttons",
                              "Multiple Choice Multiple" = "mc_multiple",
                              "Multiple Choice Multiple Buttons" = "mc_multiple_buttons",
                              "Select Dropdown" = "select",
                              "Slider" = "slider",
                              "Slider Numeric" = "slider_numeric",
                              "Date" = "date",
                              "Date Range" = "daterange"
                            )),
          shiny::textInput("add_question_id", "Question ID:", placeholder = "Enter unique question ID"),
          shiny::textInput("add_question_label", "Question Label:", placeholder = "Enter question text"),
          shiny::div(
            id = "add_question_options_div",
            style = "display: none; margin-top: 10px;",
            shiny::textAreaInput("add_question_options", 
                                "Options:", 
                                rows = 3,
                                placeholder = "Apple, Banana, Cherry")
          ),
          shiny::div(
            id = "add_slider_numeric_div",
            style = "display: none; margin-top: 10px;",
            shiny::fluidRow(
              shiny::column(6,
                shiny::numericInput("add_slider_min", "Minimum Value:", value = 0, min = -1000, max = 1000, step = 1)
              ),
              shiny::column(6,
                shiny::numericInput("add_slider_max", "Maximum Value:", value = 10, min = -1000, max = 1000, step = 1)
              )
            ),
            shiny::div(
              style = "margin-top: 10px;",
              shiny::checkboxInput("add_slider_range", "Range Mode (two handles)", value = FALSE)
            )
          )
        )
        
        return(shiny::div(form_elements))
      }
      
      return(shiny::div("Unknown content type"))
    })

    # Add page position UI
    output$add_page_position_ui <- shiny::renderUI({
      survey_structure <- parse_survey_structure()
      if (is.null(survey_structure) || !is.null(survey_structure$error) || 
          is.null(survey_structure$page_ids) || length(survey_structure$page_ids) == 0) {
        return(NULL)
      }
      last_page <- survey_structure$page_ids[length(survey_structure$page_ids)]
      
      shiny::selectInput(
        "add_page_below", 
        "Below Page:", 
        choices = setNames(survey_structure$page_ids, survey_structure$page_ids),
        selected = last_page
      )
    })

    # Initialize structure and preview handlers
    survey_structure <- server_structure_handlers(input, output, session, survey_exists)
    preview_handlers <- server_preview_handlers(input, output, session, survey_exists)
    
    # Handle direct auto-refresh trigger
    shiny::observeEvent(input$auto_refresh_trigger, {
      preview_handlers$refresh_preview()
    })

    # Handle manual refresh button
    shiny::observeEvent(input$preview_refresh_btn, {
      preview_handlers$refresh_preview()
    })
    
    # Handle responses refresh button
    shiny::observeEvent(input$responses_refresh_btn, {
      refresh_response_data()
    })

    # Launch preview on startup
    shiny::observe({
      preview_handlers$render_preview()
    }, priority = 1000)
    
    # Auto-refresh preview when survey rendering is complete
    survey_html_exists <- shiny::reactiveVal(NULL)
    monitoring_active <- shiny::reactiveVal(FALSE)
    
    # Start monitoring when user visits Preview tab AND survey is rendering
    shiny::observeEvent(input$tabset, {
      if (input$tabset == "Preview" && survey_exists()) {
        current_lua_exists <- file.exists("surveydown.lua")
        if (!monitoring_active() && current_lua_exists) {
          monitoring_active(TRUE)
          survey_html_exists(current_lua_exists)
          # Show loading message while rendering
          session$sendCustomMessage("showRenderingMessage", list())
        }
      } else if (input$tabset == "Responses") {
        # Auto-refresh data when switching to Responses tab
        refresh_response_data()
      } else {
        if (monitoring_active()) {
          monitoring_active(FALSE)
        }
      }
    })
    
    # Monitor surveydown.lua file only when actively monitoring
    shiny::observe({
      if (monitoring_active() && survey_exists()) {
        current_exists <- file.exists("surveydown.lua")
        previous_exists <- survey_html_exists()
        
        # Update the reactive value
        survey_html_exists(current_exists)
        
        # If surveydown.lua just disappeared (rendering finished), auto-refresh and stop monitoring
        if (!is.null(previous_exists) && previous_exists && !current_exists) {
          session$sendCustomMessage("triggerAutoRefresh", list(delay = 500))
          # Stop monitoring after successful completion
          monitoring_active(FALSE)
        }
        
        # Only continue monitoring if still active
        if (monitoring_active()) {
          shiny::invalidateLater(500)
        }
      }
    })
    
    # Handle R chunk separation for manual edits
    shiny::observeEvent(input$survey_editor, {
      shiny::invalidateLater(1000)
      current_content <- input$survey_editor

      # Separate R chunks if needed
      separated_content <- r_chunk_separation(current_content)

      if (!identical(current_content, separated_content)) {
        shinyAce::updateAceEditor(session, "survey_editor", value = separated_content)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle add page button
    shiny::observeEvent(input$add_page_btn, {
      shiny::updateTextInput(session, "add_page_id_input", value = "")
      session$sendCustomMessage("showModal", "add-page-modal")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle add page confirmation
    shiny::observeEvent(input$add_page_confirm, {
      page_id <- input$add_page_id_input
      
      if (is.null(page_id) || trimws(page_id) == "") {
        shiny::showNotification("Please enter a page ID", type = "error")
        return()
      }
      
      current_content <- input$survey_editor
      current_content <- r_chunk_separation(current_content)
      below_page <- input$add_page_below
      
      # Insert the new page
      if (is.null(below_page)) {
        updated_content <- insert_page_into_survey(page_id, current_content)
      } else {
        updated_content <- insert_page_below_specific_page(page_id, below_page, current_content)
      }
      
      if (!is.null(updated_content)) {
        shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
        shiny::showNotification(paste("Page", page_id, "added successfully!"), type = "message")
        survey_structure$refresh()
        session$sendCustomMessage("hideModal", "add-page-modal")
      } else {
        shiny::showNotification("Failed to add page. Please try again.", type = "error")
      }
    })

    # Handle add text button
    shiny::observeEvent(input$add_text_btn, {
      content_data <- input$add_text_btn
      page_id <- content_data$pageId
      
      if (!is.null(page_id) && page_id != "") {
        add_content_page_id(page_id)
        session$sendCustomMessage("updateModalTitle", list(
          modalId = "add-content-modal-title",
          title = paste0("Add text to page \"", page_id, "\"")
        ))
        
        add_form_trigger(list(
          page_id = page_id,
          content_type = "text",
          timestamp = as.numeric(Sys.time()) * 1000 + sample(1:1000, 1)
        ))
        session$sendCustomMessage("showModal", "add-content-modal")
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle add question button
    shiny::observeEvent(input$add_question_btn, {
      content_data <- input$add_question_btn
      page_id <- content_data$pageId
      
      if (!is.null(page_id) && page_id != "") {
        add_content_page_id(page_id)
        session$sendCustomMessage("updateModalTitle", list(
          modalId = "add-content-modal-title",
          title = paste0("Add question to page \"", page_id, "\"")
        ))
        
        add_form_trigger(list(
          page_id = page_id,
          content_type = "question",
          timestamp = as.numeric(Sys.time()) * 1000 + sample(1:1000, 1)
        ))
        session$sendCustomMessage("showModal", "add-content-modal")
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle add content confirmation
    shiny::observeEvent(input$add_content_confirm, {
      page_id <- add_content_page_id()
      form_info <- add_form_trigger()
      
      if (!is.null(page_id) && !is.null(form_info)) {
        current_content <- input$survey_editor
        current_content <- r_chunk_separation(current_content)
        updated_content <- NULL
        
        content_type <- form_info$content_type
        
        if (content_type == "text") {
          if (is.null(input$add_text_content) || trimws(input$add_text_content) == "") {
            shiny::showNotification("Please enter some text content", type = "error")
            return()
          }
          updated_content <- insert_text_into_survey(
            page_id,
            input$add_text_content,
            current_content
          )
          
          if (!is.null(updated_content)) {
            shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
            shiny::showNotification(paste("Text added to page", page_id), type = "message")
            survey_structure$refresh()
            session$sendCustomMessage("hideModal", "add-content-modal")
          } else {
            shiny::showNotification("Failed to add text. Check page ID and try again.", type = "error")
          }
          
        } else if (content_type == "question") {
          if (is.null(input$add_question_id) || input$add_question_id == "" ||
              is.null(input$add_question_label) || input$add_question_label == "") {
            shiny::showNotification("Please fill in all question fields", type = "error")
            return()
          }
          
          # Get options input for choice questions
          options_text <- NULL
          if (input$add_question_type %in% c("mc", "mc_buttons", "mc_multiple", "mc_multiple_buttons", "select", "slider")) {
            options_text <- input$add_question_options
          }
          
          # Get slider_numeric specific parameters
          min_val <- NULL
          max_val <- NULL
          is_range <- FALSE
          if (input$add_question_type == "slider_numeric") {
            min_val <- input$add_slider_min
            max_val <- input$add_slider_max
            is_range <- input$add_slider_range
            
            # Validate min/max values
            if (is.null(min_val) || is.null(max_val) || is.na(min_val) || is.na(max_val)) {
              shiny::showNotification("Please specify valid numeric minimum and maximum values for slider", type = "error")
              return()
            }
            if (!is.numeric(min_val) || !is.numeric(max_val)) {
              shiny::showNotification("Minimum and maximum values must be numeric", type = "error")
              return()
            }
            if (min_val != round(min_val) || max_val != round(max_val)) {
              shiny::showNotification("Minimum and maximum values must be integers", type = "error")
              return()
            }
            if (min_val >= max_val) {
              shiny::showNotification("Minimum value must be less than maximum value", type = "error")
              return()
            }
          }
          
          updated_content <- insert_question_into_survey(
            page_id,
            input$add_question_type,
            input$add_question_id,
            input$add_question_label,
            current_content,
            options_text,  # Pass options text
            min_val,       # Pass min value
            max_val,       # Pass max value
            is_range       # Pass range flag
          )
          
          if (!is.null(updated_content)) {
            shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
            shiny::showNotification(paste("Question", input$add_question_id, "added to page", page_id), type = "message")
            survey_structure$refresh()
            session$sendCustomMessage("hideModal", "add-content-modal")
          } else {
            shiny::showNotification("Failed to add question. Check page ID and try again.", type = "error")
          }
        }
      }
    })

    # Handle modify page button
    shiny::observeEvent(input$modify_page_btn, {
      page_data <- input$modify_page_btn
      page_id <- page_data$pageId
      
      if (!is.null(page_id) && page_id != "") {
        shiny::updateTextInput(session, "modify_page_id_input", value = page_id)
        session$userData$modify_page_original_id <- page_id
        session$sendCustomMessage("showModal", "modify-page-modal")
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle modify page confirmation
    shiny::observeEvent(input$modify_page_confirm, {
      new_page_id <- input$modify_page_id_input
      original_page_id <- session$userData$modify_page_original_id
      
      if (!is.null(new_page_id) && !is.null(original_page_id) && 
          new_page_id != "" && new_page_id != original_page_id) {
        current_content <- input$survey_editor
        current_content <- r_chunk_separation(current_content)
        updated_content <- modify_page_id(original_page_id, new_page_id, current_content)
        
        if (!is.null(updated_content)) {
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          shiny::showNotification(paste0("Page ID changed from \"", original_page_id, "\" to \"", new_page_id, "\""), type = "message")
          survey_structure$refresh()
          session$sendCustomMessage("hideModal", "modify-page-modal")
        } else {
          shiny::showNotification("Failed to modify page ID", type = "error")
        }
      } else if (new_page_id == original_page_id) {
        session$sendCustomMessage("hideModal", "modify-page-modal")
      } else {
        shiny::showNotification("Please enter a valid page ID", type = "error")
      }
    })

    # Handle modify content button
    shiny::observeEvent(input$modify_content_btn, {
      content_data <- input$modify_content_btn
      # Extract from the data structure
      page_id <- content_data$pageId
      content_id <- content_data$contentId
      content_type <- content_data$contentType
      
      if (!is.null(page_id) && !is.null(content_id) && !is.null(content_type)) {
        modify_content_info(content_data)
        survey_structure <- parse_survey_structure()
        if (!is.null(survey_structure) && page_id %in% names(survey_structure$pages) &&
            content_id %in% names(survey_structure$pages[[page_id]])) {
          current_item <- survey_structure$pages[[page_id]][[content_id]]
          
          if (content_type == "question") {
            session$sendCustomMessage("updateModalTitle", list(
              modalId = "modify-content-modal-title",
              title = paste("Modify Question:", content_id)
            ))
          } else {
            session$sendCustomMessage("updateModalTitle", list(
              modalId = "modify-content-modal-title", 
              title = paste("Modify Text:", content_id)
            ))
          }
          modify_form_trigger(list(
            type = content_type,
            item = current_item,
            page_id = page_id,
            content_id = content_id,
            timestamp = as.numeric(Sys.time()) * 1000 + sample(1:1000, 1)
          ))
          session$sendCustomMessage("showModal", "modify-content-modal")
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle modify content confirmation
    shiny::observeEvent(input$modify_content_confirm, {
      content_info <- modify_content_info()
      
      if (!is.null(content_info)) {
        page_id <- content_info$pageId
        content_id <- content_info$contentId
        content_type <- content_info$contentType
        current_content <- input$survey_editor
        current_content <- r_chunk_separation(current_content)
        updated_content <- NULL
        
        if (content_type == "question") {
          new_type <- input$modify_question_type
          new_id <- input$modify_question_id
          new_label <- input$modify_question_label
          new_options <- input$modify_question_options  # Get options input
          
          # Get slider_numeric specific parameters
          min_val <- NULL
          max_val <- NULL
          is_range <- FALSE
          if (new_type == "slider_numeric") {
            min_val <- input$modify_slider_min
            max_val <- input$modify_slider_max
            is_range <- input$modify_slider_range
            
            # Validate min/max values
            if (is.null(min_val) || is.null(max_val) || is.na(min_val) || is.na(max_val)) {
              shiny::showNotification("Please specify valid numeric minimum and maximum values for slider", type = "error")
              return()
            }
            if (!is.numeric(min_val) || !is.numeric(max_val)) {
              shiny::showNotification("Minimum and maximum values must be numeric", type = "error")
              return()
            }
            if (min_val != round(min_val) || max_val != round(max_val)) {
              shiny::showNotification("Minimum and maximum values must be integers", type = "error")
              return()
            }
            if (min_val >= max_val) {
              shiny::showNotification("Minimum value must be less than maximum value", type = "error")
              return()
            }
          }
          
          if (!is.null(new_type) && !is.null(new_id) && !is.null(new_label) && 
              new_id != "" && new_label != "") {
            updated_content <- modify_question_content(
              page_id, content_id, new_type, new_id, new_label, current_content, new_options, min_val, max_val, is_range
            )
          } else {
            shiny::showNotification("Please fill in all question fields", type = "error")
            return()
          }
        } else if (content_type == "text") {
          new_text <- input$modify_text_content
          
          if (!is.null(new_text) && trimws(new_text) != "") {
            updated_content <- modify_text_content(
              page_id, content_id, new_text, current_content
            )
          } else {
            shiny::showNotification("Please enter some text content", type = "error")
            return()
          }
        }
        
        if (!is.null(updated_content)) {
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          shiny::showNotification(paste0(toupper(substr(content_type, 1, 1)), substr(content_type, 2, nchar(content_type)), 
                                    " modified successfully!"), type = "message")
          survey_structure$refresh()
          session$sendCustomMessage("hideModal", "modify-content-modal")
        } else {
          shiny::showNotification(paste("Failed to modify", content_type), type = "error")
        }
      }
    })

    # Handle delete page button
    shiny::observeEvent(input$delete_page_btn, {
      page_id <- input$delete_page_btn
      
      if (!is.null(page_id) && page_id != "") {
        current_content <- input$survey_editor
        current_content <- r_chunk_separation(current_content)
        updated_content <- delete_page_from_survey(page_id, current_content)
        
        if (!is.null(updated_content)) {
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          shiny::showNotification(paste0("Page \"", page_id, "\" deleted successfully!"), type = "message")
          survey_structure$refresh()
        } else {
          shiny::showNotification(paste("Failed to delete page", page_id), type = "error")
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle delete content button
    shiny::observeEvent(input$delete_content_btn, {
      req(input$delete_content_btn, input$survey_editor)
      content_info <- input$delete_content_btn
      page_id <- content_info$pageId
      content_id <- content_info$contentId
      content_type <- content_info$contentType
      
      if (!is.null(page_id) && !is.null(content_id)) {
        current_content <- input$survey_editor
        current_content <- r_chunk_separation(current_content)
        updated_content <- delete_content_from_survey(page_id, content_id, content_type, current_content)
        
        if (!is.null(updated_content)) {
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          shiny::showNotification(paste0(toupper(substr(content_type, 1, 1)), substr(content_type, 2, nchar(content_type)), 
                                    " \"", content_id, "\" deleted successfully!"), type = "message")
          survey_structure$refresh()
        } else {
          shiny::showNotification(paste("Failed to delete", content_type, content_id), type = "error")
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle Undo button
    shiny::observeEvent(input$undo_btn, {
      if (input$code_tabs == "survey.qmd") {
        session$sendCustomMessage("aceUndo", "survey_editor")
      } else if (input$code_tabs == "app.R") {
        session$sendCustomMessage("aceUndo", "app_editor")
      }
    })

    # Handle Redo button
    shiny::observeEvent(input$redo_btn, {
      if (input$code_tabs == "survey.qmd") {
        session$sendCustomMessage("aceRedo", "survey_editor")
      } else if (input$code_tabs == "app.R") {
        session$sendCustomMessage("aceRedo", "app_editor")
      }
    })

    # Handle page drag and drop reordering
    shiny::observeEvent(input$page_drag_completed, {
      current_content <- input$survey_editor
      separated_content <- r_chunk_separation(current_content)
      
      if (!identical(current_content, separated_content)) {
        shinyAce::updateAceEditor(session, "survey_editor", value = separated_content)
        current_content <- separated_content
      } else {
        current_content <- input$survey_editor
      }
      
      if (length(input$page_drag_completed$order) > 0) {
        updated_content <- reorder_pages(input$page_drag_completed$order, current_content)
        
        if (!is.null(updated_content)) {
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
        } else {
          shiny::showNotification("Failed to reorder pages", type = "error")
          survey_structure$refresh()
        }
      }
    }, ignoreInit = TRUE)

    # Handle content drag and drop reordering
    shiny::observeEvent(input$content_drag_completed, {
      from_page_id <- input$content_drag_completed$fromPageId
      to_page_id <- input$content_drag_completed$toPageId
      flat_order <- input$content_drag_completed$order
      is_cross_page <- input$content_drag_completed$isCrossPage
      
      current_content <- input$survey_editor
      separated_content <- r_chunk_separation(current_content)
      
      if (!identical(current_content, separated_content)) {
        shinyAce::updateAceEditor(session, "survey_editor", value = separated_content)
        current_content <- separated_content
      } else {
        current_content <- input$survey_editor
      }
      
      content_list <- process_content_order(flat_order)
      
      if (length(content_list) == 0) {
        shiny::showNotification("No valid content items found", type = "error")
        return(NULL)
      }
      
      tryCatch({
        if (is_cross_page) {
          # Handle cross-page move
          updated_content <- handle_cross_page_content_move(
            from_page_id, to_page_id, content_list, current_content
          )
          
          if (!is.null(updated_content)) {
            updated_content <- r_chunk_separation(updated_content)
            shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          } else {
            shiny::showNotification("Failed to move content between pages", type = "error")
            survey_structure$refresh()
          }
        } else {
          # Handle within-page reordering
          check_and_separate_content(to_page_id, content_list, current_content, session)
          updated_content <- reorder_page_content(to_page_id, content_list, current_content)
          
          if (!is.null(updated_content)) {
            updated_content <- r_chunk_separation(updated_content)
            shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          } else {
            shiny::showNotification(paste("Failed to reorder content in page", to_page_id), type = "error")
            survey_structure$refresh()
          }
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
        survey_structure$refresh()
      })
    }, ignoreInit = TRUE)
    
    # Clean up when session ends
    session$onSessionEnded(function() {
      tryCatch({
        process <- shiny::isolate(preview_handlers$preview_process())
        if (!is.null(process)) {
          try(tools::pskill(process), silent = TRUE)
        }
      }, error = function(e) {
        # Silently ignore reactive context errors during cleanup
      })
    })
  }
}

# Handler for survey structure management
server_structure_handlers <- function(input, output, session, survey_exists) {
  structure_trigger <- shiny::reactiveVal(0)
  
  # Store page toggle states
  page_toggle_states <- shiny::reactiveVal(list())
  
  output$survey_structure <- shiny::renderUI({
    if (!survey_exists()) return(NULL)
    structure_trigger()
    survey_structure <- parse_survey_structure()
    
    if (!is.null(survey_structure$error)) {
      return(shiny::div(
        style = "color: red;",
        shiny::h4("Error"),
        shiny::p(survey_structure$error)
      ))
    }
    
    # Use isolate() to prevent reactive dependency on page_toggle_states
    current_states <- shiny::isolate(page_toggle_states())
    if (length(current_states) == 0 && !is.null(survey_structure$page_ids)) {
      # Initialize all pages as expanded (TRUE) on first launch
      initial_states <- setNames(
        rep(TRUE, length(survey_structure$page_ids)), 
        survey_structure$page_ids
      )
      page_toggle_states(initial_states)
      current_states <- initial_states
    }
    
    render_survey_structure(survey_structure, current_states)
  })
  
  # Handle page toggle events from JavaScript
  shiny::observeEvent(input$page_toggled, {
    if (!is.null(input$page_toggled$pageId)) {
      current_states <- page_toggle_states()
      current_states[[input$page_toggled$pageId]] <- input$page_toggled$isExpanded
      page_toggle_states(current_states)
    }
  }, ignoreInit = TRUE)
  
  refresh_structure <- function() {
    structure_trigger(structure_trigger() + 1)
    shiny::invalidateLater(200)
  }
  
  # Function to get page IDs for dropdowns
  get_page_ids <- function() {
    survey_structure <- parse_survey_structure()
    if (!is.null(survey_structure$error)) {
      return(NULL)
    }
    return(survey_structure$page_ids)
  }
  
  # Monitor editor changes with debounce
  last_update_time <- shiny::reactiveVal(Sys.time())
  
  shiny::observeEvent(input$survey_editor, {
    current_time <- Sys.time()
    if (difftime(current_time, last_update_time(), units = "secs") > 1) {
      refresh_structure()
      last_update_time(current_time)
    } else {
      shiny::invalidateLater(1000)
    }
  }, ignoreInit = TRUE)
  
  # Return functions for external use
  list(
    refresh = refresh_structure,
    get_page_ids = get_page_ids
  )
}

# Handler for survey preview functionality
server_preview_handlers <- function(input, output, session, survey_exists) {
  preview_process <- shiny::reactiveVal(NULL)
  preview_port <- stats::runif(1, 3000, 8000) |> floor()
  render_preview <- function() {
    # Check if survey exists first
    if (!survey_exists()) {
      return()
    }
    
    # Get current process
    current_process <- NULL
    shiny::isolate({
      current_process <- preview_process()
    })
    
    if (!is.null(current_process)) {
      try(tools::pskill(current_process), silent = TRUE)
      preview_process(NULL)
    }
    
    if (!file.exists("survey.qmd") || !file.exists("app.R")) {
      shiny::showNotification("Error: survey.qmd or app.R file not found!", type = "error")
      return()
    }
    
    if (exists("input") && !is.null(input$survey_editor)) {
      writeLines(input$survey_editor, "survey.qmd")
    }
    
    if (exists("input") && !is.null(input$app_editor)) {
      writeLines(input$app_editor, "app.R")
    }
    
    # Launch preview server
    new_process <- launch_preview_server(preview_port)
    
    preview_process(new_process)
    preview_url <- paste0("http://127.0.0.1:", preview_port)

    # Update single preview frame
    output$preview_frame <- shiny::renderUI({
      shiny::tags$iframe(
        src = preview_url,
        width = "100%",
        height = "100%",
        style = "border: none; display: block;"
      )
    })
  }
  
  # Initial preview frame output
  output$preview_frame <- shiny::renderUI({
    if (!survey_exists()) {
      shiny::div(
        style = "display: flex; align-items: center; justify-content: center; height: 100%; text-align: center; background-color: #f8f9fa;",
        shiny::div(
          shiny::h4("No Survey Available", style = "color: #666; margin-bottom: 15px;"),
          shiny::p("Create a survey from the Build tab to see the preview here.", style = "color: #888;")
        )
      )
    } else {
      # This will be updated by refresh_preview when called
      shiny::div(
        style = "display: flex; align-items: center; justify-content: center; height: 100%; text-align: center; background-color: #f8f9fa;",
        shiny::div(
          shiny::h4("Survey Preview", style = "color: #666; margin-bottom: 15px;"),
          shiny::p("Preview will load when survey is rendered.", style = "color: #888;")
        )
      )
    }
  })
  
  # Refresh only function - refreshes iframe with cache-busting timestamp
  refresh_preview <- function() {
    preview_url <- paste0("http://127.0.0.1:", preview_port, "?refresh=", as.numeric(Sys.time()))
    
    # Add a small delay to ensure preview server is ready
    later::later(function() {
      output$preview_frame <- shiny::renderUI({
        shiny::tags$iframe(
          src = preview_url,
          width = "100%",
          height = "100%",
          style = "border: none; display: block;"
        )
      })
    }, delay = 0.5)  # 500ms delay
  }
  
  # Return the refresh functions and process for cleanup
  list(
    render_preview = render_preview,
    refresh_preview = refresh_preview,
    preview_process = preview_process
  )
}
