#' Launch surveydown Studio
#'
#' This function serves two purposes:
#' 1. It generates a new survey project from a template
#' 2. It launches a Shiny app to preview and edit the survey
#'
#' @param path A character string specifying the directory where the survey
#'   template should be created. Defaults to the current working directory.
#' @param template A character string specifying the template to use.
#'   Default is "default" which uses the built-in package template.
#'   See `surveydown::sd_create_survey()` for other available templates.
#'
#' @return No return value, called for its side effects of creating a survey
#'   project and launching a Shiny app.
#' @importFrom stats runif setNames
#' @importFrom utils head
#' @importFrom shiny req
#' @importFrom surveydown sd_create_survey
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Launch studio with default template
#'   sd_studio()
#'
#'   # Launch studio with a specific template and path
#'   sd_studio(path = "my_survey", template = "question_types")
#' }
sd_studio <- function(path = getwd(), template = "default") {
  template_explicitly_provided <- !missing(template)
  survey_exists <- file.exists(file.path(path, "survey.qmd"))
  app_exists <- file.exists(file.path(path, "app.R"))

  if (template_explicitly_provided || !survey_exists || !app_exists) {
    surveydown::sd_create_survey(path = path, template = template)
  }
  
  original_dir <- getwd()
  on.exit(setwd(original_dir), add = TRUE)
  setwd(path)
  
  shiny::shinyApp(ui = studio_ui(), server = studio_server())
}

# UI ----

# Main UI framework
studio_ui <- function() {
  shiny::navbarPage(
    title = "surveydown Studio",
    id = "tabset",
    theme = bslib::bs_theme(version = 5),
    ui_construction_tab(),
    ui_preview_tab()
  )
}

# Construction tab UI
ui_construction_tab <- function() {
  shiny::tabPanel(
    "Construction",

    shiny::tags$head(
      shiny::tags$style(shiny::HTML(get_studio_css())),
      shiny::tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.14.0/Sortable.min.js"),
      shiny::tags$script(shiny::HTML(get_studio_js())),

      shiny::div(
        id = "modify-page-modal",
        class = "modal fade",
        tabindex = "-1",
        shiny::div(
          class = "modal-dialog",
          shiny::div(
            class = "modal-content",
            shiny::div(
              class = "modal-header",
              shiny::h5("Modify Page ID", class = "modal-title"),
              shiny::tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal")
            ),
            shiny::div(
              class = "modal-body",
              shiny::textInput("modify_page_id_input", "Page ID:", value = "")
            ),
            shiny::div(
              class = "modal-footer",
              shiny::actionButton("modify_page_confirm", "Save Changes", class = "btn btn-primary"),
              shiny::tags$button("Cancel", type = "button", class = "btn btn-secondary", `data-bs-dismiss` = "modal")
            )
          )
        )
      ),

      shiny::div(
        id = "modify-content-modal",
        class = "modal fade",
        tabindex = "-1",
        shiny::div(
          class = "modal-dialog",
          shiny::div(
            class = "modal-content",
            shiny::div(
              class = "modal-header",
              shiny::h5(id = "modify-content-modal-title", "Modify Content", class = "modal-title"),
              shiny::tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal")
            ),
            shiny::div(
              class = "modal-body",
              shiny::uiOutput("modify_content_form")
            ),
            shiny::div(
              class = "modal-footer",
              shiny::actionButton("modify_content_confirm", "Save Changes", class = "btn btn-primary"),
              shiny::tags$button("Cancel", type = "button", class = "btn btn-secondary", `data-bs-dismiss` = "modal")
            )
          )
        )
      ),

      shiny::div(
        id = "add-content-modal",
        class = "modal fade",
        tabindex = "-1",
        shiny::div(
          class = "modal-dialog",
          shiny::div(
            class = "modal-content",
            shiny::div(
              class = "modal-header",
              shiny::h5(id = "add-content-modal-title", "Add Content", class = "modal-title"),
              shiny::tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal")
            ),
            shiny::div(
              class = "modal-body",
              shiny::uiOutput("add_content_form")
            ),
            shiny::div(
              class = "modal-footer",
              shiny::actionButton("add_content_confirm", "Add Content", class = "btn btn-primary"),
              shiny::tags$button("Cancel", type = "button", class = "btn btn-secondary", `data-bs-dismiss` = "modal")
            )
          )
        )
      ),

      shiny::div(
        id = "add-page-modal",
        class = "modal fade",
        tabindex = "-1",
        shiny::div(
          class = "modal-dialog",
          shiny::div(
            class = "modal-content",
            shiny::div(
              class = "modal-header",
              shiny::h5("Add A New Page", class = "modal-title"),
              shiny::tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal")
            ),
            shiny::div(
              class = "modal-body",
              shiny::textInput("add_page_id_input", "Page ID:", 
                              placeholder = "Enter page ID"),
              shiny::uiOutput("add_page_position_ui")
            ),
            shiny::div(
              class = "modal-footer",
              shiny::actionButton("add_page_confirm", "Add Page", class = "btn btn-success"),
              shiny::tags$button("Cancel", type = "button", class = "btn btn-secondary", `data-bs-dismiss` = "modal")
            )
          )
        )
      )
    ),

    shiny::fluidRow(      
      # Left - Structure Panel
      shiny::column(
        width = 5,
        style = "border-right: 1px solid #ddd;",
        shiny::div(          
          # Structure header with undo/redo buttons
          shiny::div(
            style = "display: flex; justify-content: space-between; align-items: center; background-color: #cce5ff; padding: 6px; margin-bottom: 10px; border-radius: 4px;",
            
            # Undo button (left)
            shiny::actionButton(
              "undo_btn",
              NULL,
              icon = shiny::icon("undo"),
              class = "btn-sm",
              style = "background-color: #cce5ff; border-color: #007bff; color: #007bff; padding: 2px 5px; font-size: 0.8rem;"
            ),
            
            # Structure title (center)
            shiny::h5("Structure", style = "margin: 0; text-align: center; flex-grow: 1;"),
            
            # Redo button (right)
            shiny::actionButton(
              "redo_btn",
              NULL,
              icon = shiny::icon("redo"),
              class = "btn-sm",
              style = "background-color: #cce5ff; border-color: #007bff; color: #007bff; padding: 2px 5px; font-size: 0.8rem;"
            )
          ),
          
          # Structure content panel
          shiny::wellPanel(
            style = "background-color: #ffffff; border-color: #cce5ff; padding: 0.5rem;",
            
            shiny::div(
              style = "overflow-y: auto; height: calc(100vh - 191px);",
              shiny::uiOutput("survey_structure")
            ),
            
            # Add A New Page button
            shiny::div(
              style = "margin-top: 10px;",
              shiny::actionButton(
                "add_page_btn",
                "Add A New Page",
                class = "btn-success",
                style = "width: 100%; padding: 8px; font-weight: bold;"
              )
            )
          )
        )
      ),
      
      # Right - Code Panel
      shiny::column(
        width = 7,
        style = "border-right: 1px solid #ddd;",
        shiny::div(
          # Code header
          shiny::div(
            style = "display: flex; justify-content: space-between; align-items: center; background-color: #d4edda; padding: 6px; margin-bottom: 10px; border-radius: 4px;",
            shiny::h5("Code", style = "margin: 0; text-align: center; flex-grow: 1;")
          ),
          
          # Code panel with tabs
          shiny::wellPanel(
            style = "background-color: #f0fff0; border-color: #d4edda; padding: 0.5rem;",
            shiny::tabsetPanel(
              id = "code_tabs",
              shiny::tabPanel(
                "survey.qmd",
                shiny::div(
                  style = "margin-top: 10px",
                  shiny::uiOutput("survey_editor_ui")
                )
              ),
              shiny::tabPanel(
                "app.R",
                shiny::div(
                  style = "margin-top: 10px",
                  shiny::uiOutput("app_editor_ui")
                )
              )
            )
          )
        )
      )
    )
  )
}

# Preview tab UI
ui_preview_tab <- function() {
  shiny::tabPanel(
    "Preview",
    shiny::div(
      style = "display: flex; flex-direction: column; align-items: center; height: calc(100vh - 79px);",
      
      # Refresh button container
      shiny::div(
        style = "margin-bottom: 10px; text-align: center;",
        shiny::actionButton("refresh_preview_btn", "Refresh Preview", 
                           class = "btn-success btn-sm",
                           icon = shiny::icon("sync"))
      ),
      
      # Preview iframe with adjusted height
      shiny::div(
        style = "width: 100%; height: calc(100vh - 115px); border: none;",
        shiny::uiOutput("preview_frame")
      )
    )
  )
}

# Server ----

# Main server function
studio_server <- function() {
  function(input, output, session) {
    # Reactive values for modify content state
    modify_form_trigger <- shiny::reactiveVal(NULL)
    modify_content_info <- shiny::reactiveVal(NULL)
    add_content_page_id <- shiny::reactiveVal(NULL)
    add_form_trigger <- shiny::reactiveVal(NULL)

    # Setup survey.qmd editor
    output$survey_editor_ui <- shiny::renderUI({
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

    # Setup app.R editor
    output$app_editor_ui <- shiny::renderUI({
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
    survey_structure <- server_structure_handlers(input, output, session)
    preview_handlers <- server_preview_handlers(input, output, session)
    
    # Connect refresh button to preview function
    shiny::observeEvent(input$refresh_preview_btn, {
      preview_handlers$refresh_preview()
    })

    # Launch preview on startup
    shiny::observe({
      preview_handlers$refresh_preview()
    }, priority = 1000)
    
    # Handle R chunk separation for manual edits
    shiny::observeEvent(input$survey_editor, {
      shiny::invalidateLater(1000)
      current_content <- input$survey_editor
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
          
          updated_content <- insert_question_into_survey(
            page_id,
            input$add_question_type,
            input$add_question_id,
            input$add_question_label,
            current_content,
            options_text  # Pass options text
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
          
          if (!is.null(new_type) && !is.null(new_id) && !is.null(new_label) && 
              new_id != "" && new_label != "") {
            updated_content <- modify_question_content(
              page_id, content_id, new_type, new_id, new_label, current_content, new_options
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
      process <- preview_handlers$preview_process()
      if (!is.null(process)) {
        try(tools::pskill(process), silent = TRUE)
      }
    })
  }
}

# Handler for survey structure management
server_structure_handlers <- function(input, output, session) {
  structure_trigger <- shiny::reactiveVal(0)
  
  # Store page toggle states
  page_toggle_states <- shiny::reactiveVal(list())
  
  output$survey_structure <- shiny::renderUI({
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
server_preview_handlers <- function(input, output, session) {
  preview_process <- shiny::reactiveVal(NULL)
  preview_port <- stats::runif(1, 3000, 8000) |> floor()
  refresh_preview <- function() {
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

    output$preview_frame <- shiny::renderUI({
      shiny::tags$iframe(
        src = preview_url,
        width = "100%",
        height = "100%",
        style = "border: 1px solid #ddd; border-radius: 5px; display: block;"
      )
    })
  }
  
  # Return the refresh function and process for cleanup
  list(
    refresh_preview = refresh_preview,
    preview_process = preview_process
  )
}

# Content Editing Functions ----

# Reorganize library calls to the beginning of the file
reorganize_libraries <- function(editor_content) {
  if (is.null(editor_content)) {
    return(NULL)
  }
  
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Extract all library calls
  library_calls <- extract_library_calls(editor_content)
  
  # If no library calls found, return original content
  if (length(library_calls) == 0) {
    return(paste(editor_content, collapse = "\n"))
  }
  
  # Remove library calls from their current locations
  cleaned_content <- remove_library_calls_from_content(editor_content)
  
  # Add consolidated library chunk at the beginning
  final_content <- add_library_chunk_to_beginning(cleaned_content, library_calls)
  
  return(paste(final_content, collapse = "\n"))
}

# Extract all library() calls from the content
extract_library_calls <- function(editor_content) {
  library_calls <- character(0)
  in_r_chunk <- FALSE
  
  for (i in seq_along(editor_content)) {
    line <- editor_content[i]
    
    if (grepl("^```\\{r\\}", line)) {
      in_r_chunk <- TRUE
    } else if (in_r_chunk && grepl("^```$", line)) {
      in_r_chunk <- FALSE
    } else if (in_r_chunk) {
      # Check for library calls in this line
      if (grepl("^\\s*library\\s*\\(", line, perl = TRUE)) {
        library_calls <- c(library_calls, trimws(line))
      }
    }
  }
  
  # Remove duplicates while preserving order
  return(unique(library_calls))
}

# Remove library calls from their current locations
remove_library_calls_from_content <- function(editor_content) {
  result <- character(0)
  in_r_chunk <- FALSE
  current_chunk_lines <- character(0)
  chunk_start_line <- ""
  
  i <- 1
  while (i <= length(editor_content)) {
    line <- editor_content[i]
    
    if (grepl("^```\\{r\\}", line)) {
      # Start of R chunk
      in_r_chunk <- TRUE
      current_chunk_lines <- character(0)
      chunk_start_line <- line
    } else if (in_r_chunk && grepl("^```$", line)) {
      # End of R chunk - process the accumulated lines
      in_r_chunk <- FALSE
      
      # Filter out library calls from chunk content
      non_library_lines <- character(0)
      for (chunk_line in current_chunk_lines) {
        if (!grepl("^\\s*library\\s*\\(", chunk_line, perl = TRUE)) {
          non_library_lines <- c(non_library_lines, chunk_line)
        }
      }
      
      # Only add the chunk if it has non-library content
      if (length(non_library_lines) > 0) {
        # Check if all remaining lines are just whitespace
        non_empty_lines <- non_library_lines[trimws(non_library_lines) != ""]
        if (length(non_empty_lines) > 0) {
          result <- c(result, chunk_start_line, non_library_lines, line)
        }
      }
      
      current_chunk_lines <- character(0)
    } else if (in_r_chunk) {
      # Inside R chunk - collect lines
      current_chunk_lines <- c(current_chunk_lines, line)
    } else {
      # Outside R chunk - add line as-is
      result <- c(result, line)
    }
    
    i <- i + 1
  }
  
  return(result)
}

# Add consolidated library chunk to the beginning
add_library_chunk_to_beginning <- function(content_lines, library_calls) {
  if (length(library_calls) == 0) {
    return(content_lines)
  }
  
  # Find where to insert the library chunk (after YAML header)
  yaml_end_line <- 0
  in_yaml <- FALSE
  
  for (i in seq_along(content_lines)) {
    line <- content_lines[i]
    
    if (i == 1 && grepl("^---\\s*$", line)) {
      in_yaml <- TRUE
    } else if (in_yaml && grepl("^---\\s*$", line)) {
      yaml_end_line <- i
      break
    }
  }
  
  # Create the library chunk
  library_chunk <- c(
    "```{r}",
    library_calls,
    "```"
  )
  
  # Insert after YAML header or at the very beginning
  if (yaml_end_line > 0) {
    result <- c(
      content_lines[1:yaml_end_line],
      "",
      library_chunk,
      content_lines[(yaml_end_line + 1):length(content_lines)]
    )
  } else {
    result <- c(
      library_chunk,
      content_lines
    )
  }
  
  return(result)
}

# Insert a new page into the survey.qmd file
insert_page_into_survey <- function(page_id, editor_content) {
  if (is.null(editor_content)) {
    return(NULL)
  }

  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  last_page_end <- max(which(grepl(":::", editor_content, fixed = TRUE)), 0)
  
  if (last_page_end == 0) {
    last_page_end <- length(editor_content)
  }
  
  page_template <- generate_page_template(page_id)
  result <- c(
    editor_content[1:last_page_end],
    page_template,
    if(last_page_end < length(editor_content)) editor_content[(last_page_end+1):length(editor_content)] else NULL
  )
  
  return(paste(result, collapse = "\n"))
}

# Insert a new page below a specific existing page
insert_page_below_specific_page <- function(new_page_id, below_page_id, editor_content) {
  if (is.null(editor_content) || is.null(new_page_id) || is.null(below_page_id)) {
    return(NULL)
  }
  
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", below_page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(insert_page_into_survey(new_page_id, editor_content))
  }
  
  page_start_line <- page_start_lines[1]
  page_end_line <- NULL

  for (i in page_start_line:length(editor_content)) {
    if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) {
    return(insert_page_into_survey(new_page_id, editor_content))
  }
  
  page_template <- generate_page_template(new_page_id)
  
  result <- c(
    editor_content[1:page_end_line],
    page_template,
    if(page_end_line < length(editor_content)) editor_content[(page_end_line+1):length(editor_content)] else NULL
  )
  
  return(paste(result, collapse = "\n"))
}

# Generate a page template based on page ID
generate_page_template <- function(page_id) {
  if (page_id == "end") {
    return(c(
      "::: {.sd_page id=end}",
      "",
      "## Thanks for taking our survey!",
      "",
      "```{r}",
      "# Close button",
      "sd_close()",
      "```",
      "",
      ":::",
      ""
    ))
  } else {
    return(c(
      paste0("::: {.sd_page id=", page_id, "}"),
      "",
      "```{r}",
      "sd_next()",
      "```",
      "",
      ":::",
      ""
    ))
  }
}

# Insert a question into a specific page
insert_question_into_survey <- function(page_id, question_type, question_id, question_label, editor_content, options_text = NULL) {
  if (is.null(editor_content) || is.null(page_id)) {
    return(NULL)
  }
  
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(NULL)
  }
  
  page_start_line <- page_start_lines[1]
  page_end_line <- NULL

  for (i in page_start_line:length(editor_content)) {
    if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) {
    return(NULL)
  }
  
  insertion_point <- find_insertion_point(editor_content, page_start_line, page_end_line)
  question_code <- generate_question_code(question_type, question_id, question_label, options_text)
  question_chunk <- c(
    "```{r}",
    question_code,
    "```"
  )
  
  result <- c(
    editor_content[1:(insertion_point-1)],
    question_chunk,
    editor_content[insertion_point:length(editor_content)]
  )
  
  return(paste(result, collapse = "\n"))
}

# Insert text into a specific page
insert_text_into_survey <- function(page_id, text_content, editor_content) {
  if (is.null(editor_content) || is.null(page_id)) {
    return(NULL)
  }
  
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(NULL)
  }
  
  page_start_line <- page_start_lines[1]
  page_end_line <- NULL

  for (i in page_start_line:length(editor_content)) {
    if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) {
    return(NULL)
  }
  
  insertion_point <- find_insertion_point(editor_content, page_start_line, page_end_line)
  formatted_text <- strsplit(text_content, "\n")[[1]]
  result <- c(
    editor_content[1:(insertion_point-1)],
    formatted_text,
    "",
    editor_content[insertion_point:length(editor_content)]
  )
  
  return(paste(result, collapse = "\n"))
}

# Find the best insertion point for a new question
find_insertion_point <- function(editor_content, page_start_line, page_end_line) {

  for (i in page_start_line:page_end_line) {
    if (grepl("^```\\{r\\}", editor_content[i])) {
      chunk_start <- i
      chunk_end <- NULL
      
      for (j in (i+1):page_end_line) {
        if (grepl("^```$", editor_content[j])) {
          chunk_end <- j
          break
        }
      }
      
      if (!is.null(chunk_end)) {
        chunk_content <- editor_content[(chunk_start+1):(chunk_end-1)]
        # Updated pattern to include sd_close() and sd_prev() as navigation functions
        if (any(grepl("sd_next\\(|sd_prev\\(|sd_close\\(", chunk_content, perl = TRUE))) {
          return(chunk_start)
        }
      }
    }
  }
  
  return(page_end_line)
}

# Reorder pages in the survey file
reorder_pages <- function(new_order, editor_content) {
  if (is.null(editor_content) || length(new_order) == 0) {
    return(NULL)
  }
  
  # Ensure new_order is a character vector
  if (is.list(new_order)) {
    new_order <- unlist(new_order)
  }
  new_order <- as.character(new_order)
  
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  page_blocks <- list()
  
  for (page_id in new_order) {
    page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
    page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
    
    if (length(page_start_lines) == 0) {
      next
    }
    
    page_start_line <- page_start_lines[1]
    page_end_line <- NULL
    
    for (i in page_start_line:length(editor_content)) {
      if (i > page_start_line && grepl("^:::$", editor_content[i])) {
        page_end_line <- i
        break
      }
    }
    
    if (!is.null(page_end_line)) {
      page_blocks[[page_id]] <- list(
        start = page_start_line,
        end = page_end_line,
        content = editor_content[page_start_line:page_end_line]
      )
    }
  }
  
  if (length(page_blocks) != length(new_order)) {
    return(NULL)
  }
  
  first_page_start <- min(sapply(page_blocks, function(block) block$start))
  last_page_end <- max(sapply(page_blocks, function(block) block$end))
  
  # Build page content with blank lines between pages
  page_content_list <- list()
  for (i in seq_along(new_order)) {
    page_id <- new_order[i]
    page_content_list[[length(page_content_list) + 1]] <- page_blocks[[page_id]]$content
    
    # Add blank line after each page except the last one
    if (i < length(new_order)) {
      page_content_list[[length(page_content_list) + 1]] <- ""
    }
  }
  
  result <- c(
    editor_content[1:(first_page_start-1)],
    unlist(page_content_list),
    if(last_page_end < length(editor_content)) editor_content[(last_page_end+1):length(editor_content)] else NULL
  )
  
  return(paste(result, collapse = "\n"))
}

# Reorder content within a page
reorder_page_content <- function(page_id, new_content_order, editor_content) {
  if (is.null(editor_content) || is.null(page_id) || length(new_content_order) == 0) {
    return(NULL)
  }
  
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(NULL)
  }
  
  page_start_line <- page_start_lines[1]
  page_end_line <- NULL

  for (i in page_start_line:length(editor_content)) {
    if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) {
    return(NULL)
  }
  
  original_page_content <- editor_content[page_start_line:page_end_line]
  navigation_chunks <- extract_navigation_chunks(original_page_content)
  survey_structure <- parse_survey_structure()
  
  if (is.null(survey_structure) || !("pages" %in% names(survey_structure)) ||
      !(page_id %in% names(survey_structure$pages))) {
    return(NULL)
  }
  
  page_items <- survey_structure$pages[[page_id]]
  new_page_content <- c(
    editor_content[page_start_line],
    ""
  )
  
  for (item in new_content_order) {
    if (!is.list(item) || !all(c("type", "id") %in% names(item)) ||
        !(item$id %in% names(page_items))) {
      next
    }
    
    current_item <- page_items[[item$id]]
    
    if (!is.list(current_item) || !("is_question" %in% names(current_item))) {
      next
    }
    
    if (current_item$is_question) {
      if ("raw" %in% names(current_item)) {
        r_chunk_lines <- strsplit(current_item$raw, "\n")[[1]]
        new_page_content <- c(new_page_content, r_chunk_lines, "")
      }
    } else {
      if ("content" %in% names(current_item)) {
        text_lines <- strsplit(current_item$content, "\n")[[1]]
        new_page_content <- c(new_page_content, text_lines, "")
      }
    }
  }
  
  if (length(navigation_chunks) > 0) {
    for (chunk in navigation_chunks) {
      new_page_content <- c(new_page_content, chunk$lines, "")
    }
  }
  
  new_page_content <- c(new_page_content, ":::")
  
  result <- c(
    editor_content[1:(page_start_line-1)],
    new_page_content,
    if (page_end_line < length(editor_content)) editor_content[(page_end_line+1):length(editor_content)] else NULL
  )
  
  return(paste(result, collapse = "\n"))
}

# Extract navigation chunks from page content
extract_navigation_chunks <- function(page_content) {
  navigation_chunks <- list()
  in_chunk <- FALSE
  chunk_start <- NULL
  
  for (i in seq_along(page_content)) {
    line <- page_content[i]
    
    if (grepl("^```\\{r\\}", line)) {
      in_chunk <- TRUE
      chunk_start <- i
    } else if (in_chunk && grepl("^```$", line)) {
      in_chunk <- FALSE
      chunk_content <- page_content[(chunk_start+1):(i-1)]

      if (any(grepl("sd_next\\(|sd_prev\\(|sd_close\\(", chunk_content))) {
        navigation_chunks[[length(navigation_chunks) + 1]] <- list(
          start = chunk_start,
          end = i,
          lines = page_content[chunk_start:i]
        )
      }
    }
  }
  
  return(navigation_chunks)
}

# Separate multiple sd_* function calls into individual R chunks AND clean up content
r_chunk_separation <- function(editor_content) {
  if (is.null(editor_content)) {
    return(NULL)
  }
  
  if (length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  result <- character(0)
  i <- 1
  
  while (i <= length(editor_content)) {
    if (grepl("^```\\{r\\}", editor_content[i])) {
      chunk_start <- i
      chunk_end <- NULL
      
      for (j in (i+1):length(editor_content)) {
        if (grepl("^```$", editor_content[j])) {
          chunk_end <- j
          break
        }
      }
      
      if (!is.null(chunk_end)) {
        chunk_content <- editor_content[(chunk_start+1):(chunk_end-1)]
        
        # Check for sd_ function calls
        sd_start_indices <- grep("\\bsd_[a-zA-Z0-9_]+\\s*\\(", chunk_content)
        
        # If multiple sd_ calls, split the chunk
        if (length(sd_start_indices) > 1) {
          for (start_idx in sd_start_indices) {
            call_end <- find_function_call_end(chunk_content, start_idx)
            
            # Add this call as a separate chunk
            result <- c(
              result,
              "```{r}",
              chunk_content[start_idx:call_end],
              "```",
              ""
            )
          }
          
          # Skip past the original chunk
          i <- chunk_end + 1
        } else {
          # No multiple sd_ calls, keep the chunk as is
          result <- c(
            result,
            editor_content[chunk_start:chunk_end]
          )
          i <- chunk_end + 1
        }
      } else {
        # If no end of chunk found, add this line and continue
        result <- c(result, editor_content[i])
        i <- i + 1
      }
    } else {
      # Not an R chunk start, add this line and continue
      result <- c(result, editor_content[i])
      i <- i + 1
    }
  }
  
  # Remove trailing empty line if present
  if (length(result) > 0 && result[length(result)] == "") {
    result <- result[-length(result)]
  }
  
  # Convert to string for processing
  content_string <- paste(result, collapse = "\n")
  
  # Apply library reorganization
  content_string <- reorganize_libraries(content_string)
  
  # Apply empty line cleanup
  content_string <- clean_multiple_empty_lines(content_string)
  
  return(content_string)
}

# Find the end of a function call by tracking parentheses
find_function_call_end <- function(chunk_content, start_idx) {
  call_end <- start_idx
  paren_count <- 0
  
  # Count initial opening parentheses in the first line
  opening <- gregexpr("\\(", chunk_content[start_idx])[[1]]
  if (opening[1] > 0) {
    paren_count <- length(opening)
  }
  
  # Count initial closing parentheses in the first line
  closing <- gregexpr("\\)", chunk_content[start_idx])[[1]]
  if (closing[1] > 0) {
    paren_count <- paren_count - length(closing)
  }
  
  # If parentheses not balanced in first line, find the end
  if (paren_count > 0) {
    for (k in (start_idx+1):length(chunk_content)) {
      # Update parenthesis count
      opening <- gregexpr("\\(", chunk_content[k])[[1]]
      if (opening[1] > 0) {
        paren_count <- paren_count + length(opening)
      }
      
      closing <- gregexpr("\\)", chunk_content[k])[[1]]
      if (closing[1] > 0) {
        paren_count <- paren_count - length(closing)
      }
      
      # If balanced, we found the end
      if (paren_count <= 0) {
        call_end <- k
        break
      }
    }
  }
  
  return(call_end)
}

# Generate question code based on type
generate_question_code <- function(type, id, label, options_text = NULL) {
  # Ensure we have valid inputs
  if (is.null(id) || id == "") id <- paste0(type, "_id")
  if (is.null(label) || label == "") label <- paste0(type, "_label")
  
  # Generate appropriate code based on question type
  if (type %in% c("mc", "mc_buttons", "mc_multiple", "mc_multiple_buttons", "select", "slider")) {
    # Parse options input
    options_vector <- parse_options_input(options_text)
    
    # Generate option lines
    option_lines <- character(0)
    for (i in seq_along(options_vector)) {
      option_label <- names(options_vector)[i]
      option_value <- options_vector[i]
      
      if (i == 1) {
        option_lines <- c(option_lines, paste0("    \"", option_label, "\" = \"", option_value, "\""))
      } else if (i == length(options_vector)) {
        option_lines <- c(option_lines, paste0("    \"", option_label, "\" = \"", option_value, "\""))
      } else {
        option_lines <- c(option_lines, paste0("    \"", option_label, "\" = \"", option_value, "\","))
      }
    }
    
    # Add comma to all but last option
    if (length(option_lines) > 1) {
      for (i in 1:(length(option_lines) - 1)) {
        if (!grepl(",$", option_lines[i])) {
          option_lines[i] <- paste0(option_lines[i], ",")
        }
      }
    }
    
    return(c(
      paste0("sd_question("),
      paste0("  type   = \"", type, "\","),
      paste0("  id     = \"", id, "\","),
      paste0("  label  = \"", label, "\","),
      paste0("  option = c("),
      option_lines,
      paste0("  )"),
      paste0(")")
    ))
  } else if (type == "slider_numeric") {
    return(c(
      paste0("sd_question("),
      paste0("  type   = \"", type, "\","),
      paste0("  id     = \"", id, "\","),
      paste0("  label  = \"", label, "\","),
      paste0("  option = seq(0, 10, 1)"),
      paste0(")")
    ))
  } else {
    # Simple questions (text, textarea, numeric, date, daterange)
    return(c(
      paste0("sd_question("),
      paste0("  type  = \"", type, "\","),
      paste0("  id    = \"", id, "\","),
      paste0("  label = \"", label, "\""),
      paste0(")")
    ))
  }
}

# Content Editing Helper Functions ----

# Clean up multiple consecutive empty lines - reduce to single empty line
clean_multiple_empty_lines <- function(content) {
  if (is.null(content)) {
    return(NULL)
  }
  
  if (is.character(content) && length(content) == 1) {
    content <- strsplit(content, "\n")[[1]]
  }
  
  result <- character(0)
  empty_line_count <- 0
  
  for (line in content) {
    if (trimws(line) == "") {
      empty_line_count <- empty_line_count + 1
      # Only add one empty line, regardless of how many consecutive we find
      if (empty_line_count == 1) {
        result <- c(result, line)
      }
    } else {
      # Non-empty line, reset counter
      empty_line_count <- 0
      result <- c(result, line)
    }
  }
  
  return(paste(result, collapse = "\n"))
}

# Extract options from raw question code
extract_options_from_raw <- function(raw_code) {
  if (is.null(raw_code) || raw_code == "") {
    return("")
  }
  
  # Look for option = c(...) pattern
  option_pattern <- "option\\s*=\\s*c\\s*\\(([^)]+)\\)"
  option_match <- regexpr(option_pattern, raw_code, perl = TRUE)
  
  if (option_match > 0) {
    option_content <- regmatches(raw_code, option_match)
    # Extract just the content inside c(...)
    inner_pattern <- "c\\s*\\(([^)]+)\\)"
    inner_match <- regexpr(inner_pattern, option_content, perl = TRUE)
    
    if (inner_match > 0) {
      start_pos <- attr(inner_match, "capture.start")[1]
      length_val <- attr(inner_match, "capture.length")[1]
      options_text <- substr(option_content, start_pos, start_pos + length_val - 1)
      
      # Parse the options to extract just the labels
      # Remove quotes and extract labels (before =)
      options_pairs <- strsplit(options_text, ",")[[1]]
      labels <- character(0)
      
      for (pair in options_pairs) {
        pair <- trimws(pair)
        if (grepl("=", pair)) {
          label_part <- strsplit(pair, "=")[[1]][1]
          label_part <- trimws(label_part)
          label_part <- gsub("^[\"']|[\"']$", "", label_part)  # Remove quotes
          labels <- c(labels, label_part)
        }
      }
      
      return(paste(labels, collapse = ", "))
    }
  }
  
  return("")
}

# Convert option label to snake case value
label_to_snake_case <- function(label) {
  # Convert to lowercase, replace spaces and special chars with underscores
  snake_case <- tolower(label)
  snake_case <- gsub("[^a-z0-9]+", "_", snake_case)
  # Remove leading/trailing underscores
  snake_case <- gsub("^_+|_+$", "", snake_case)
  # Replace multiple underscores with single
  snake_case <- gsub("_+", "_", snake_case)
  return(snake_case)
}

# Parse options input string into named vector
parse_options_input <- function(options_text) {
  if (is.null(options_text) || trimws(options_text) == "") {
    return(c("Option A" = "option_a", "Option B" = "option_b"))
  }
  
  # First, replace line breaks with commas to normalize separators
  # Handle different line break types (Windows: \r\n, Unix: \n, Mac: \r)
  options_text <- gsub("\r\n", "\n", options_text)  # Normalize Windows line breaks
  options_text <- gsub("\r", "\n", options_text)    # Normalize Mac line breaks
  
  # Replace line breaks with commas, but be careful not to double up commas
  options_text <- gsub("\n+", ",", options_text)    # Replace one or more line breaks with comma
  
  # Split by comma, handling various spacing
  options_raw <- strsplit(options_text, ",")[[1]]
  options_raw <- trimws(options_raw)
  options_raw <- options_raw[options_raw != ""]  # Remove empty entries
  
  if (length(options_raw) == 0) {
    return(c("Option A" = "option_a", "Option B" = "option_b"))
  }
  
  # Create named vector
  option_values <- sapply(options_raw, label_to_snake_case, USE.NAMES = FALSE)
  options_vector <- setNames(option_values, options_raw)
  
  return(options_vector)
}

# Parse survey structure from survey.qmd file
parse_survey_structure <- function() {
  # Read the survey content
  if (exists("input", envir = parent.frame()) && !is.null(get("input", envir = parent.frame())$survey_editor)) {
    survey_content <- get("input", envir = parent.frame())$survey_editor
    survey_content <- paste(survey_content, collapse = "\n")
  } else if (file.exists("survey.qmd")) {
    survey_content <- readLines("survey.qmd", warn = FALSE)
    survey_content <- paste(survey_content, collapse = "\n")
  } else {
    return(list(error = "survey.qmd file not found!"))
  }
  
  # Extract pages
  page_pattern_underscore <- ":::\\s*\\{\\s*\\.sd_page\\s+id\\s*=\\s*([a-zA-Z0-9_]+)\\s*\\}"
  page_pattern_dash <- ":::\\s*\\{\\s*\\.sd-page\\s+id\\s*=\\s*([a-zA-Z0-9_]+)\\s*\\}"
  
  # Check which pattern is used
  if (grepl(page_pattern_underscore, survey_content)) {
    page_pattern <- page_pattern_underscore
  } else if (grepl(page_pattern_dash, survey_content)) {
    page_pattern <- page_pattern_dash
  } else {
    return(list(error = "No pages found in survey.qmd!"))
  }
  
  # Find all pages
  page_matches <- gregexpr(page_pattern, survey_content, perl = TRUE)
  page_ids <- regmatches(survey_content, page_matches)
  
  if (length(page_ids) == 0 || length(page_ids[[1]]) == 0) {
    return(list(error = "No pages found in survey.qmd!"))
  }
  
  # Extract page IDs
  page_ids <- extract_page_ids(page_ids[[1]])
  
  # Split content by pages
  split_pattern <- ":::\\s*\\{\\s*\\.sd[_-]page"
  page_splits <- strsplit(survey_content, split_pattern, perl = TRUE)[[1]]
  
  if (length(page_splits) <= 1) {
    return(list(error = "Error parsing page content!"))
  }
  
  page_splits <- page_splits[-1]  # Remove content before first page
  
  # Process each page
  pages <- list()
  for (i in seq_along(page_ids)) {
    if (i <= length(page_splits)) {
      page_id <- page_ids[i]
      page_content <- extract_page_content(page_splits[i])
      pages[[page_id]] <- extract_page_items(page_content, page_id)
    }
  }
  
  return(list(pages = pages, page_ids = page_ids))
}

# Clean consecutive empty lines in a text string or vector
clean_consecutive_empty_lines <- function(content) {
  if (is.character(content) && length(content) == 1) {
    content <- strsplit(content, "\n")[[1]]
  }
  
  result <- character(0)
  prev_empty <- FALSE
  
  for (line in content) {
    current_empty <- trimws(line) == ""
    
    # Only add empty line if previous wasn't empty
    if (!(prev_empty && current_empty)) {
      result <- c(result, line)
    }
    
    prev_empty <- current_empty
  }
  
  return(paste(result, collapse = "\n"))
}

# Handle moving content between pages
handle_cross_page_content_move <- function(from_page_id, to_page_id, target_content_list, editor_content) {
  if (is.null(editor_content) || is.null(from_page_id) || is.null(to_page_id)) {
    return(NULL)
  }
  
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  survey_structure <- parse_survey_structure()
  if (is.null(survey_structure) || !("pages" %in% names(survey_structure))) {
    return(NULL)
  }
  
  # Get content items that need to be moved
  content_to_move <- list()
  for (item in target_content_list) {
    if (item$id %in% names(survey_structure$pages[[from_page_id]])) {
      content_to_move[[item$id]] <- survey_structure$pages[[from_page_id]][[item$id]]
    }
  }
  
  if (length(content_to_move) == 0) {
    return(NULL)
  }
  
  # Remove content from source page
  temp_content <- editor_content
  for (content_id in names(content_to_move)) {
    content_item <- content_to_move[[content_id]]
    content_type <- if (content_item$is_question) "question" else "text"
    temp_content <- delete_content_from_survey_lines(from_page_id, content_id, content_type, temp_content)
    if (is.null(temp_content)) {
      return(NULL)
    }
  }
  
  # Add content to target page in the specified order
  result_content <- add_content_to_target_page(to_page_id, content_to_move, target_content_list, temp_content)
  
  return(paste(result_content, collapse = "\n"))
}

# Delete content from survey (working with lines array)
delete_content_from_survey_lines <- function(page_id, content_id, content_type, editor_lines) {
  # Similar to delete_content_from_survey but works with lines array
  # Implementation needed based on your existing delete_content_from_survey function
  editor_content_str <- paste(editor_lines, collapse = "\n")
  result_str <- delete_content_from_survey(page_id, content_id, content_type, editor_content_str)
  if (is.null(result_str)) return(NULL)
  return(strsplit(result_str, "\n")[[1]])
}

# Add content to target page
add_content_to_target_page <- function(page_id, content_items, target_order, editor_lines) {
  # Find target page boundaries
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_lines, perl = TRUE)
  
  if (length(page_start_lines) == 0) return(NULL)
  
  page_start_line <- page_start_lines[1]
  page_end_line <- NULL
  
  for (i in page_start_line:length(editor_lines)) {
    if (grepl("^:::$", editor_lines[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) return(NULL)
  
  # Get current page structure to identify existing content and navigation
  original_page_content <- editor_lines[page_start_line:page_end_line]
  navigation_chunks <- extract_navigation_chunks(original_page_content)
  
  # Get existing content from target page (excluding moved items)
  survey_structure <- parse_survey_structure()
  if (is.null(survey_structure) || !("pages" %in% names(survey_structure))) {
    return(NULL)
  }
  
  existing_page_items <- survey_structure$pages[[page_id]]
  moved_item_ids <- names(content_items)
  
  # Filter out moved items from existing content
  remaining_existing_items <- existing_page_items[!names(existing_page_items) %in% moved_item_ids]
  
  # Rebuild page content in the exact order specified by target_order
  new_page_content <- c(
    editor_lines[page_start_line],  # Page opening
    ""
  )
  
  # Process each item in the target order
  for (item in target_order) {
    item_id <- item$id
    
    # Check if it's a moved item
    if (item_id %in% names(content_items)) {
      content_item <- content_items[[item_id]]
      if (content_item$is_question && "raw" %in% names(content_item)) {
        content_lines <- strsplit(content_item$raw, "\n")[[1]]
        new_page_content <- c(new_page_content, content_lines, "")
      } else if (!content_item$is_question && "content" %in% names(content_item)) {
        text_lines <- strsplit(content_item$content, "\n")[[1]]
        new_page_content <- c(new_page_content, text_lines, "")
      }
    }
    # Check if it's an existing item that stayed in the page
    else if (item_id %in% names(remaining_existing_items)) {
      existing_item <- remaining_existing_items[[item_id]]
      if (existing_item$is_question && "raw" %in% names(existing_item)) {
        content_lines <- strsplit(existing_item$raw, "\n")[[1]]
        new_page_content <- c(new_page_content, content_lines, "")
      } else if (!existing_item$is_question && "content" %in% names(existing_item)) {
        text_lines <- strsplit(existing_item$content, "\n")[[1]]
        new_page_content <- c(new_page_content, text_lines, "")
      }
    }
  }
  
  # Add navigation chunks at the end
  if (length(navigation_chunks) > 0) {
    for (chunk in navigation_chunks) {
      new_page_content <- c(new_page_content, chunk$lines, "")
    }
  }
  
  # Add page closing
  new_page_content <- c(new_page_content, ":::")
  
  # Rebuild the entire file
  result <- c(
    editor_lines[1:(page_start_line-1)],
    new_page_content,
    if (page_end_line < length(editor_lines)) editor_lines[(page_end_line+1):length(editor_lines)] else NULL
  )
  
  return(result)
}

# Function to render the survey structure UI
render_survey_structure <- function(survey_structure, page_states = NULL) {
  shiny::div(
    id = "pages-container",
    lapply(survey_structure$page_ids, function(page_id) {
      page_items <- survey_structure$pages[[page_id]]
      
      # Sort items by position
      if (length(page_items) > 0) {
        positions <- sapply(page_items, function(item) item$position)
        sorted_items <- page_items[order(positions)]
      } else {
        sorted_items <- list()
      }
      
      # Check if this page should be expanded or collapsed
      is_expanded <- TRUE  # default
      if (!is.null(page_states) && page_id %in% names(page_states)) {
        is_expanded <- page_states[[page_id]]
      }
      
      shiny::div(
        class = "page-wrapper",
        `data-page-id` = page_id,
        
        # Page header with toggle and drag handle
        shiny::div(
          class = "page-header",
          shiny::div(
            class = "page-drag-handle drag-handle",
            shiny::icon("grip-vertical")
          ),
          shiny::div(paste0("Page: ", page_id), style = "margin: 0; font-weight: bold;"),
          
          shiny::div(
            class = "page-actions",
            style = "display: flex; gap: 5px;",
            # Modify page button
            shiny::actionButton(
              inputId = "modify_page_btn_ui",
              label = NULL,
              icon = shiny::icon("edit"),
              class = "btn-sm btn-outline-primary modify-page-btn",
              title = "Modify page ID",
              `data-page-id` = page_id
            ),
            # Delete page button
            shiny::actionButton(
              inputId = "delete_page_btn_ui",
              label = NULL,
              icon = shiny::icon("trash-alt"),
              class = "btn-sm btn-outline-danger delete-page-btn",
              title = "Delete page",
              `data-page-id` = page_id
            )
          ),
          
          shiny::div(
            class = "toggle-icon",
            # Update icon based on state
            shiny::icon(if(is_expanded) "chevron-down" else "chevron-right")
          )
        ),
        
        # Content container - set display based on state
        shiny::div(
          class = "questions-container",
          id = paste0("page-", page_id, "-content"),
          `data-page-id` = page_id,
          style = if(is_expanded) "display: block;" else "display: none;",
          
          shiny::div(
            style = "margin-bottom: 10px; width: 98%; margin-left: auto; margin-right: auto; display: flex; gap: 5px;",
            shiny::actionButton(
              inputId = "add_text_btn_ui",
              label = "Add Text",
              class = "btn-success add-text-btn",
              style = "width: calc(50% - 2.5px); padding: 8px; font-weight: bold;",
              title = "Add text to this page",
              `data-page-id` = page_id
            ),
            shiny::actionButton(
              inputId = "add_question_btn_ui", 
              label = "Add Question",
              class = "btn-info add-question-btn",
              style = "width: calc(50% - 2.5px); padding: 8px; font-weight: bold;",
              title = "Add question to this page",
              `data-page-id` = page_id
            )
          ),
          
          # Add content items in their order
          if (length(sorted_items) > 0) {
            lapply(names(sorted_items), function(item_id) {
              render_content_item(sorted_items[[item_id]])
            })
          }
        )
      )
    })
  )
}

# Render a single content item (question or text)
render_content_item <- function(item) {
  if (item$is_question) {
    # Question item
    shiny::div(
      class = "question-item",
      `data-question-id` = item$id,
      `data-content-type` = "question",
      `data-position` = item$position,
      
      # Question drag handle
      shiny::div(
        class = "question-drag-handle drag-handle",
        shiny::icon("grip-lines")
      ),
      
      # Content wrapper - keeps the vertical layout
      shiny::div(
        class = "content-wrapper",
        style = "flex-grow: 1;",
        
        # Question content - each on its own line
        shiny::div(
          shiny::HTML(paste0("<strong>Question: ", item$id, "</strong>"))
        ),
        shiny::div(
          shiny::HTML(paste0("Type: ", item$type))
        ),
        shiny::div(
          shiny::HTML(paste0("Label: ", item$label))
        )
      ),
      
      # Content delete button
      shiny::div(
        class = "content-actions",
        style = "margin-left: auto; display: flex; gap: 5px;",
        # Modify question button
        shiny::actionButton(
          inputId = "modify_content_btn_ui",
          label = NULL,
          icon = shiny::icon("edit"),
          class = "btn-sm btn-outline-success modify-content-btn",
          title = "Modify question",
          `data-content-id` = item$id,
          `data-page-id` = item$page_id,
          `data-content-type` = "question"
        ),
        # Delete question button
        shiny::actionButton(
          inputId = "delete_content_btn_ui",
          label = NULL,
          icon = shiny::icon("trash-alt"),
          class = "btn-sm btn-outline-danger delete-content-btn",
          title = "Delete question",
          `data-content-id` = item$id,
          `data-page-id` = item$page_id
        )
      )
    )
  } else {
    # Text item
    shiny::div(
      class = "text-item",
      `data-text-id` = item$id,
      `data-content-type` = "text",
      `data-position` = item$position,
      
      # Text drag handle
      shiny::div(
        class = "text-drag-handle drag-handle",
        shiny::icon("grip-lines")
      ),
      
      # Text content wrapper - keeps vertical layout if needed
      shiny::div(
        class = "content-wrapper",
        style = "flex-grow: 1;",
        
        # Text content preview
        shiny::div(
          shiny::HTML(paste0("<strong>Text:</strong> ", item$preview))
        )
      ),
      
      # Content delete button
      shiny::div(
        class = "content-actions",
        style = "margin-left: auto; display: flex; gap: 5px;",
        # Modify text button
        shiny::actionButton(
          inputId = "modify_content_btn_ui",
          label = NULL,
          icon = shiny::icon("edit"),
          class = "btn-sm btn-outline-success modify-content-btn",
          title = "Modify text",
          `data-content-id` = item$id,
          `data-page-id` = item$page_id,
          `data-content-type` = "text"
        ),
        # Delete text button
        shiny::actionButton(
          inputId = "delete_content_btn_ui",
          label = NULL,
          icon = shiny::icon("trash-alt"),
          class = "btn-sm btn-outline-danger delete-content-btn",
          title = "Delete text",
          onclick = paste0("Shiny.setInputValue('delete_content_btn', { pageId: '", 
                        item$page_id, "', contentId: '", item$id, "', contentType: 'text' });"),
          `data-content-id` = item$id,
          `data-page-id` = item$page_id
        )
      )
    )
  }
}

# Extract page IDs from page matches
extract_page_ids <- function(page_matches) {
  extracted_ids <- vector("character", length(page_matches))
  
  for (i in seq_along(page_matches)) {
    id_match <- regexpr("id\\s*=\\s*([a-zA-Z0-9_]+)", page_matches[i], perl = TRUE)
    if (id_match > 0) {
      match_text <- regmatches(page_matches[i], list(id_match))[[1]]
      extracted_ids[i] <- gsub("id\\s*=\\s*", "", match_text)
    } else {
      extracted_ids[i] <- paste("page", i)
    }
  }
  
  return(extracted_ids)
}

# Extract cleaned page content
extract_page_content <- function(raw_page_content) {
  # Find the closing :::
  closing_idx <- regexpr(":::", raw_page_content, fixed = TRUE)
  if (closing_idx > 0) {
    raw_page_content <- substr(raw_page_content, 1, closing_idx - 1)
  }
  
  # Skip the id part
  id_end_idx <- regexpr("}", raw_page_content, fixed = TRUE)
  if (id_end_idx > 0) {
    return(substr(raw_page_content, id_end_idx + 1, nchar(raw_page_content)))
  }
  
  return(raw_page_content)
}

# Extract items (questions and text) from page content
extract_page_items <- function(page_content, page_id) {
  # Find all R code blocks
  r_block_pattern <- "```\\{r\\}([\\s\\S]*?)```"
  r_blocks <- gregexpr(r_block_pattern, page_content, perl = TRUE)
  
  content_items <- list()
  item_index <- 1
  
  # If no R blocks, add entire content as text
  if (r_blocks[[1]][1] == -1) {
    if (nchar(trimws(page_content)) > 0) {
      content_items[[paste0("text_", page_id, "_1")]] <- create_text_item(
        paste0("text_", page_id, "_1"),
        trimws(page_content),
        1,
        page_id
      )
    }
    return(content_items)
  }
  
  # Get positions and lengths of R blocks
  r_positions <- r_blocks[[1]]
  r_lengths <- attr(r_blocks[[1]], "match.length")
  r_matches <- regmatches(page_content, r_blocks)[[1]]
  
  # Create a list to track all content segments
  segments <- list()
  
  # Add text before first R block if exists
  if (r_positions[1] > 1) {
    text_content <- substr(page_content, 1, r_positions[1] - 1)
    if (nchar(trimws(text_content)) > 0) {
      segments[[length(segments) + 1]] <- list(
        type = "text",
        start = 1,
        end = r_positions[1] - 1,
        content = trimws(text_content)
      )
    }
  }
  
  # Process each R block and text between blocks
  for (j in seq_along(r_positions)) {
    # Add the R block
    block_start <- r_positions[j]
    block_end <- block_start + r_lengths[j] - 1
    r_block <- r_matches[j]
    
    # Extract code content
    code_content <- gsub("```\\{r\\}", "", r_block)
    code_content <- gsub("```$", "", code_content)
    code_content <- trimws(code_content)
    
    # Check if it's a question
    is_question <- grepl("sd_question\\s*\\(", code_content, perl = TRUE)
    
    segments[[length(segments) + 1]] <- list(
      type = if(is_question) "question" else "r_block",
      start = block_start,
      end = block_end,
      content = r_block,
      code = code_content
    )
    
    # Add text after this block if not the last block
    if (j < length(r_positions)) {
      next_start <- r_positions[j + 1]
      text_start <- block_end + 1
      text_end <- next_start - 1
      
      if (text_end >= text_start) {
        text_content <- substr(page_content, text_start, text_end)
        if (nchar(trimws(text_content)) > 0) {
          segments[[length(segments) + 1]] <- list(
            type = "text",
            start = text_start,
            end = text_end,
            content = trimws(text_content)
          )
        }
      }
    } else if (block_end < nchar(page_content)) {
      # Add text after last block
      text_content <- substr(page_content, block_end + 1, nchar(page_content))
      if (nchar(trimws(text_content)) > 0) {
        segments[[length(segments) + 1]] <- list(
          type = "text",
          start = block_end + 1,
          end = nchar(page_content),
          content = trimws(text_content)
        )
      }
    }
  }
  
  # Process segments into content items
  text_counter <- 1
  for (segment in segments) {
    if (segment$type == "text") {
      text_id <- paste0("text_", page_id, "_", text_counter)
      text_counter <- text_counter + 1
      
      content_items[[text_id]] <- create_text_item(
        text_id, 
        segment$content, 
        item_index,
        page_id
      )
      item_index <- item_index + 1
    } else if (segment$type == "question") {
      # Extract question parameters
      question_params <- extract_question_params(segment$code)
      
      if (question_params$id != "unknown") {
        content_items[[question_params$id]] <- create_question_item(
          question_params$type,
          question_params$id,
          question_params$label,
          segment$content,
          item_index,
          page_id
        )
        item_index <- item_index + 1
      }
    }
  }
  
  return(content_items)
}

# Create a question item
create_question_item <- function(type, id, label, raw_content, position, page_id) {
  list(
    type = type,
    id = id,
    label = label,
    raw = raw_content,
    position = position,
    is_question = TRUE,
    page_id = page_id
  )
}

# Create a text item
create_text_item <- function(id, content, position, page_id) {
  # Generate preview (first 5 words)
  words <- strsplit(content, "\\s+")[[1]]
  preview <- paste(head(words, 5), collapse = " ")
  if (length(words) > 5) preview <- paste0(preview, "...")
  
  list(
    id = id,
    preview = preview,
    content = content,
    position = position,
    is_question = FALSE,
    page_id = page_id
  )
}

# Modify page ID in the survey.qmd file
modify_page_id <- function(old_page_id, new_page_id, editor_content) {
  if (is.null(editor_content) || is.null(old_page_id) || is.null(new_page_id)) {
    return(NULL)
  }
  
  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find and replace the page ID in the opening tag
  page_pattern_underscore <- paste0("::: \\{.sd_page id=", old_page_id, "\\}")
  page_pattern_dash <- paste0("::: \\{.sd-page id=", old_page_id, "\\}")
  
  # Check which pattern exists and replace
  replaced <- FALSE
  for (i in seq_along(editor_content)) {
    if (grepl(page_pattern_underscore, editor_content[i], perl = TRUE)) {
      editor_content[i] <- gsub(page_pattern_underscore, 
                               paste0("::: {.sd_page id=", new_page_id, "}"), 
                               editor_content[i], perl = TRUE)
      replaced <- TRUE
    } else if (grepl(page_pattern_dash, editor_content[i], perl = TRUE)) {
      editor_content[i] <- gsub(page_pattern_dash, 
                               paste0("::: {.sd-page id=", new_page_id, "}"), 
                               editor_content[i], perl = TRUE)
      replaced <- TRUE
    }
  }
  
  if (!replaced) {
    return(NULL)
  }
  
  return(paste(editor_content, collapse = "\n"))
}

# Modify question content in the survey
modify_question_content <- function(page_id, old_question_id, new_type, new_id, new_label, editor_content, options_text = NULL) {
  if (is.null(editor_content) || is.null(page_id) || is.null(old_question_id)) {
    return(NULL)
  }
  
  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find the page
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(NULL)
  }
  
  page_start_line <- page_start_lines[1]
  
  # Find the page end
  page_end_line <- NULL
  for (i in page_start_line:length(editor_content)) {
    if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) {
    return(NULL)
  }
  
  # Find the R chunk containing the old question
  for (i in page_start_line:page_end_line) {
    if (grepl("^```\\{r\\}", editor_content[i])) {
      chunk_start <- i
      chunk_end <- NULL
      
      for (j in (i+1):page_end_line) {
        if (grepl("^```$", editor_content[j])) {
          chunk_end <- j
          break
        }
      }
      
      if (!is.null(chunk_end)) {
        chunk_content <- paste(editor_content[(chunk_start+1):(chunk_end-1)], collapse = "\n")
        
        # Check if this chunk contains the question we want to modify
        if (grepl(paste0('id\\s*=\\s*["\']', old_question_id, '["\']'), chunk_content, perl = TRUE)) {
          # Generate new question code
          new_question_code <- generate_question_code(new_type, new_id, new_label, options_text)
          
          # Replace the chunk content
          result <- c(
            editor_content[1:(chunk_start)],
            new_question_code,
            editor_content[chunk_end:length(editor_content)]
          )
          
          return(paste(result, collapse = "\n"))
        }
      }
    }
  }
  
  return(NULL)
}

# Modify text content in the survey
modify_text_content <- function(page_id, old_text_id, new_text, editor_content) {
  if (is.null(editor_content) || is.null(page_id) || is.null(old_text_id)) {
    return(NULL)
  }
  
  # Get the current survey structure
  survey_structure <- parse_survey_structure()
  
  if (is.null(survey_structure) || !("pages" %in% names(survey_structure)) ||
      !(page_id %in% names(survey_structure$pages)) ||
      !(old_text_id %in% names(survey_structure$pages[[page_id]]))) {
    return(NULL)
  }
  
  # Get the old content
  old_text_item <- survey_structure$pages[[page_id]][[old_text_id]]
  
  if (!("content" %in% names(old_text_item))) {
    return(NULL)
  }
  
  old_text <- old_text_item$content
  
  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find and replace the old text with new text
  editor_content_str <- paste(editor_content, collapse = "\n")
  
  # Escape special characters for regex
  escaped_old_text <- gsub("([\\$\\^\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|])", "\\\\\\1", old_text)
  
  # Replace the text
  updated_content_str <- gsub(escaped_old_text, new_text, editor_content_str, fixed = TRUE)
  
  # Check if replacement was successful
  if (identical(editor_content_str, updated_content_str)) {
    return(NULL)
  }
  
  return(updated_content_str)
}

# Delete a page from the survey.qmd file
delete_page_from_survey <- function(page_id, editor_content) {
  if (is.null(editor_content) || is.null(page_id)) {
    return(NULL)
  }
  
  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find the page
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(NULL)
  }
  
  # Use the first match
  page_start_line <- page_start_lines[1]
  
  # Find the end of the page
  page_end_line <- NULL
  for (i in page_start_line:length(editor_content)) {
    if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) {
    return(NULL)
  }
  
  # Remove the page
  result <- c(
    editor_content[1:(page_start_line-1)],
    if(page_end_line < length(editor_content)) editor_content[(page_end_line+1):length(editor_content)] else NULL
  )
  
  # Return the updated content
  return(paste(result, collapse = "\n"))
}

# Delete content from a page in the survey
delete_content_from_survey <- function(page_id, content_id, content_type, editor_content) {
  if (is.null(editor_content) || is.null(page_id) || is.null(content_id)) {
    return(NULL)
  }
  
  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find the page
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(NULL)
  }
  
  # Use the first match
  page_start_line <- page_start_lines[1]
  
  # Find the page end
  page_end_line <- NULL
  for (i in page_start_line:length(editor_content)) {
    if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) {
    return(NULL)
  }
  
  # Get the page content
  page_content <- editor_content[page_start_line:page_end_line]
  
  # Parse the structure to find the content
  survey_structure <- parse_survey_structure()
  
  if (is.null(survey_structure) || !("pages" %in% names(survey_structure)) ||
      !(page_id %in% names(survey_structure$pages)) ||
      !(content_id %in% names(survey_structure$pages[[page_id]]))) {
    return(NULL)
  }
  
  # Get the content item
  content_item <- survey_structure$pages[[page_id]][[content_id]]
  
  # Depending on content type, find and remove it
  if (content_type == "question" && content_item$is_question) {
    # Remove the R chunk containing the question
    r_block_pattern <- "```\\{r\\}([\\s\\S]*?)```"
    r_blocks <- gregexpr(r_block_pattern, paste(page_content, collapse = "\n"), perl = TRUE)
    r_matches <- regmatches(paste(page_content, collapse = "\n"), r_blocks)[[1]]
    
    for (r_block in r_matches) {
      # Look for the question ID in the block
      if (grepl(paste0('id\\s*=\\s*["\']', content_id, '["\']'), r_block, perl = TRUE)) {
        # Remove this block from page content
        updated_page_content <- gsub(gsub("([\\$\\^\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|])", "\\\\\\1", r_block), 
                                 "", paste(page_content, collapse = "\n"), fixed = TRUE)
        
        # Rebuild the document
        result <- c(
          editor_content[1:(page_start_line-1)],
          strsplit(updated_page_content, "\n")[[1]],
          if (page_end_line < length(editor_content)) editor_content[(page_end_line+1):length(editor_content)] else NULL
        )
        
        # Clean up any consecutive blank lines
        result <- clean_consecutive_blank_lines(result)
        
        return(paste(result, collapse = "\n"))
      }
    }
  } else if (content_type == "text" && !content_item$is_question) {
    # For text items, we need to find the exact text and remove it
    if ("content" %in% names(content_item)) {
      text_content <- content_item$content
      
      # Escape special characters for regex
      escaped_text <- gsub("([\\$\\^\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|])", "\\\\\\1", text_content)
      
      # Find and remove the text from page content
      updated_page_content <- gsub(escaped_text, "", paste(page_content, collapse = "\n"), fixed = TRUE)
      
      # Rebuild the document
      result <- c(
        editor_content[1:(page_start_line-1)],
        strsplit(updated_page_content, "\n")[[1]],
        if (page_end_line < length(editor_content)) editor_content[(page_end_line+1):length(editor_content)] else NULL
      )
      
      # Clean up any consecutive blank lines
      result <- clean_consecutive_blank_lines(result)
      
      return(paste(result, collapse = "\n"))
    }
  }
  
  # If we get here, deletion failed
  return(NULL)
}

# Helper function to clean up consecutive blank lines
clean_consecutive_blank_lines <- function(lines) {
  # Remove consecutive blank lines
  result <- character(0)
  prev_blank <- FALSE
  
  for (line in lines) {
    current_blank <- trimws(line) == ""
    
    if (!(prev_blank && current_blank)) {
      result <- c(result, line)
    }
    
    prev_blank <- current_blank
  }
  
  return(result)
}

# Extract question parameters
extract_question_params <- function(code_text) {
  extract_param <- function(param_name, text) {
    # Try single quotes
    pattern <- paste0(param_name, "\\s*=\\s*'([^']*)'")
    match <- regexpr(pattern, text, perl = TRUE)
    
    if (match > 0 && !is.null(attr(match, "capture.start")) && 
        attr(match, "capture.start")[1] > 0) {
      start_pos <- attr(match, "capture.start")[1]
      length_val <- attr(match, "capture.length")[1]
      return(substr(text, start_pos, start_pos + length_val - 1))
    }
    
    # Try double quotes
    pattern <- paste0(param_name, '\\s*=\\s*"([^"]*)"')
    match <- regexpr(pattern, text, perl = TRUE)
    
    if (match > 0 && !is.null(attr(match, "capture.start")) && 
        attr(match, "capture.start")[1] > 0) {
      start_pos <- attr(match, "capture.start")[1]
      length_val <- attr(match, "capture.length")[1]
      return(substr(text, start_pos, start_pos + length_val - 1))
    }
    
    return("unknown")
  }
  
  return(list(
    type = extract_param("type", code_text),
    id = extract_param("id", code_text),
    label = extract_param("label", code_text)
  ))
}

# Process content order array
process_content_order <- function(flat_order) {
  content_list <- list()
  
  if (is.vector(flat_order) || is.list(flat_order)) {
    # Ensure we have a character vector
    if (!is.character(flat_order)) {
      flat_order <- as.character(unlist(flat_order))
    }
    
    # Process pairs of type and id
    if (length(flat_order) >= 2) {
      for (i in seq(1, length(flat_order), by = 2)) {
        if (i + 1 <= length(flat_order)) {
          content_list[[length(content_list) + 1]] <- list(
            type = flat_order[i],
            id = flat_order[i + 1]
          )
        }
      }
    } else {
      shiny::showNotification("Order data too short. Needed pairs of type/id values.", type = "error")
      return(list())
    }
  } else {
    shiny::showNotification("Invalid content order format type", type = "error")
    return(list())
  }
  
  return(content_list)
}

# Check and separate content if necessary
check_and_separate_content <- function(page_id, content_list, current_content, session) {
  page_structure <- parse_survey_structure()
  
  if (!is.null(page_structure) && !is.null(page_structure$pages) && 
      page_id %in% names(page_structure$pages)) {
    
    # Check if any dragged item has multiple sd_* calls
    needs_separation <- FALSE
    for (item in content_list) {
      if (item$type == "question" && item$id %in% names(page_structure$pages[[page_id]])) {
        question_item <- page_structure$pages[[page_id]][[item$id]]
        if ("raw" %in% names(question_item)) {
          # Count sd_ function calls in this chunk
          raw_content <- question_item$raw
          sd_calls <- gregexpr("\\bsd_[a-zA-Z0-9_]+\\s*\\(", raw_content)
          if (sd_calls[[1]][1] > 0 && length(sd_calls[[1]]) > 1) {
            needs_separation <- TRUE
            break
          }
        }
      }
    }
    
    # If we need to separate content
    if (needs_separation) {
      current_content <- r_chunk_separation(current_content)
      shinyAce::updateAceEditor(session, "survey_editor", value = current_content)
      shiny::showNotification("Separated multiple functions in dragged content", 
                            type = "message", duration = 2)
    }
  }
}

# Launch preview server
launch_preview_server <- function(port) {
  # Create a temporary R script to run the app
  temp_script <- tempfile(fileext = ".R")
  writeLines(
    paste0(
      "library(shiny)\n",
      "port <- ", port, "\n",
      "setwd('", getwd(), "')\n",
      "source('app.R')\n",
      "options(shiny.port = port)\n",
      "options(shiny.host = '127.0.0.1')\n",
      "shiny::runApp(launch.browser = FALSE)\n"
    ), 
    temp_script
  )
  
  # Run the temp script in a separate R process
  r_path <- file.path(R.home("bin"), "R")
  system2(r_path, c("--vanilla", "-f", temp_script), wait = FALSE, stdout = NULL, stderr = NULL)
}

# CSS and JavaScript ----

# Function to get the custom CSS for the studio
get_studio_css <- function() {
  return("
    /* ===== GENERAL LAYOUT & NAVIGATION ===== */
    .navbar:not(.fixed-bottom):not(.navbar-fixed-bottom):not(.navbar-fixed-bottom)+div>.tab-content>.tab-pane {
      margin-top: 10px !important;
    }

    /* ===== DRAG & DROP EFFECTS ===== */
    .sortable-ghost {
      opacity: 0.4;
    }

    .sortable-placeholder {
      background-color: #f9f9f9;
      border: 1px dashed #ccc;
      margin: 5px 0;
    }

    /* Visual feedback during content dragging */
    .content-dragging .page-header {
      transition: background-color 0.2s ease;
    }

    .content-dragging .page-header.drag-hover {
      background-color: #b3d9ff !important;
      border: 2px dashed #007bff;
    }

    .content-dragging .questions-container {
      min-height: 50px;
      transition: min-height 0.2s ease;
    }

    /* Ensure expanded pages during drag have visible drop zones */
    .content-dragging .questions-container:empty::after {
      content: \"Drop content here\";
      display: block;
      text-align: center;
      color: #666;
      font-style: italic;
      padding: 20px;
      border: 2px dashed #ccc;
      border-radius: 5px;
      margin: 10px;
    }

    /* ===== TAB ENLARGEMENT ===== */
    .navbar-nav .nav-link {
      font-size: 1.1rem;
    }
    
    /* ===== CODE EDITOR STYLING ===== */
    .shiny-ace.ace_editor {
      margin-bottom: 0.1rem !important;
    }
    
    /* ===== UNDO/REDO BUTTON HOVER EFFECTS ===== */
    /* Structure section undo/redo buttons */
    .btn-sm[style*=\"background-color: #cce5ff\"]:hover {
      background-color: #99d6ff !important;
      border-color: #0056b3 !important;
      color: #0056b3 !important;
      transform: scale(1.05);
      transition: all 0.2s ease;
    }

    /* Add page button styling */
    #add_page_btn {
      background-color: #ffe0b2 !important;
      border-color: #ffcc80 !important;
      color: #f57c00 !important;
    }

    #add_page_btn:hover {
      background-color: #ffcc80 !important;
      border-color: #ffb74d !important;
      color: #bf360c !important;
      transform: translateY(-1px);
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }

    /* Add text button styling */
    .add-text-btn {
      background-color: #f0fff0 !important;
      border-color: #28a745 !important;
      color: #28a745 !important;
    }

    .add-text-btn:hover {
      background-color: #e8f5e8 !important;
      border-color: #1e7e34 !important;
      color: #1e7e34 !important;
      transform: translateY(-1px);
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }

    /* Add question button styling */
    .add-question-btn {
      background-color: #f0f8ff !important;
      border-color: #5bc0de !important;
      color: #5bc0de !important;
    }

    .add-question-btn:hover {
      background-color: #e6f3ff !important;
      border-color: #31b0d5 !important;
      color: #31b0d5 !important;
      transform: translateY(-1px);
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }

    /* Code section undo/redo buttons */
    .btn-sm[style*=\"background-color: #d4edda\"]:hover {
      background-color: #b8e6c1 !important;
      border-color: #1e7e34 !important;
      color: #1e7e34 !important;
      transform: scale(1.05);
      transition: all 0.2s ease;
    }

    /* Add smooth transition for all small buttons */
    .btn-sm {
      transition: all 0.2s ease;
    }

    /* ===== MODAL & FORM STYLING ===== */
    .modal-body textarea {
      height: 150px;
      font-family: Monaco, 'Lucida Console', monospace;
    }

    .modal-body input[type='text'] {
      font-family: Monaco, 'Lucida Console', monospace;
    }
    
    /* ===== PAGE STRUCTURE ===== */
    .page-header {
      background-color: #cce5ff; 
      padding: 10px; 
      border-radius: 5px; 
      margin-bottom: 10px;
      cursor: pointer;
      display: flex;
      justify-content: space-between;
      align-items: center;
      font-family: Monaco, 'Lucida Console', monospace;
    }
    
    .page-header:hover {
      background-color: #ADD8FF;
    }
    
    .page-header .toggle-icon {
      margin-left: 10px;
      min-width: 20px;
    }
    
    .page-actions {
      margin-left: auto;
    }
    
    /* ===== CONTENT ITEMS ===== */
    /* Base styling for content items */
    .question-item, 
    .text-item {
      margin: 0 auto 10px auto; 
      padding: 5px 5px 5px 20px; 
      width: 98%;
      position: relative;
      display: flex;
      align-items: center;
      font-family: Monaco, 'Lucida Console', monospace;
      font-size: 0.9rem;
    }
    
    /* Rounded corners for first and last content items */
    /* First content item */
    .questions-container > .question-item:nth-child(2),
    .questions-container > .text-item:nth-child(2) {
      border-top-left-radius: 5px;
      border-top-right-radius: 5px;
    }

    /* Last content item */
    .questions-container > .question-item:last-child,
    .questions-container > .text-item:last-child {
      border-bottom-left-radius: 5px;
      border-bottom-right-radius: 5px;
    }

    /* Question-specific styling */
    .question-item {
      border-left: 3px solid #5bc0de; 
      background-color: #f0f0f0;
    }
    
    /* Text-specific styling */
    .text-item {
      border-left: 3px solid #28a745; 
      background-color: #e3e3e3;
    }
    
    /* ===== INTERACTIVE ELEMENTS ===== */
    /* Drag handles - unified styling */
    .page-header .drag-handle,
    .question-item .drag-handle,
    .text-item .drag-handle {
      cursor: move;
      color: #777;
    }
    
    .page-header .drag-handle {
      margin-right: 10px;
    }
    
    .question-item .drag-handle,
    .text-item .drag-handle {
      position: absolute;
      left: 5px;
    }
    
    /* Drag handle hover effects */
    .page-header .drag-handle:hover,
    .question-item .drag-handle:hover,
    .text-item .drag-handle:hover {
      color: #333;
    }
    
    /* Action buttons */
    .content-actions {
      margin-right: 5px;
    }
    
    /* ===== DRAG & DROP EFFECTS ===== */
    .sortable-ghost {
      opacity: 0.4;
    }
    
    .sortable-placeholder {
      background-color: #f9f9f9;
      border: 1px dashed #ccc;
      margin: 5px 0;
    }
  ")
}

# Function to get the custom JavaScript for the studio
get_studio_js <- function() {
  return("
    $(document).ready(function() {
      
      /* ===== SHINY MESSAGE HANDLERS ===== */
      // Ace editor undo/redo commands
      Shiny.addCustomMessageHandler('aceUndo', function(editorId) {
        var editor = ace.edit(editorId);
        editor.undo();
        editor.clearSelection();
        editor.navigateTo(0, 0);
        editor.scrollToLine(0, true, true, function() {});
        editor.focus();
      });

      Shiny.addCustomMessageHandler('aceRedo', function(editorId) {
        var editor = ace.edit(editorId);
        editor.redo();
        editor.clearSelection();
        editor.navigateTo(0, 0);
        editor.scrollToLine(0, true, true, function() {});
        editor.focus();
      });
      
      // Modal control handlers
      Shiny.addCustomMessageHandler('showModal', function(modalId) {
        $('#' + modalId).modal('show');
      });

      Shiny.addCustomMessageHandler('hideModal', function(modalId) {
        $('#' + modalId).modal('hide');
      });

      Shiny.addCustomMessageHandler('updateModalTitle', function(data) {
        $('#' + data.modalId).text(data.title);
      });

      /* ===== MODAL MANAGEMENT ===== */
      // Function to reset modal content
      function resetModifyModal() {
        // Clear any cached form values
        $('#modify-content-modal .modal-body input, #modify-content-modal .modal-body textarea, #modify-content-modal .modal-body select').val('');
        
        // Reset modal title
        $('#modify-content-modal-title').text('Modify Content');
      }

      // Modal event handlers
      $('#modify-content-modal').on('hidden.bs.modal', function() {
        resetModifyModal();
      });

      $('#modify-content-modal').on('show.bs.modal', function() {
        resetModifyModal();
      });

      // Function to reset add page modal
      function resetAddPageModal() {
        // Clear the page ID input
        $('#add_page_id_input').val('');
        
        // Reset the position dropdown to the last option if it exists
        if ($('#add_page_below').length) {
          var $dropdown = $('#add_page_below');
          var lastOption = $dropdown.find('option:last').val();
          $dropdown.val(lastOption);
        }
      }

      // Add page modal event handlers
      $('#add-page-modal').on('hidden.bs.modal', function() {
        resetAddPageModal();
      });

      $('#add-page-modal').on('show.bs.modal', function() {
        resetAddPageModal();
      });

      // Function to reset add content modal
      function resetAddContentModal() {
        // Clear any cached form values
        $('#add-content-modal .modal-body input, #add-content-modal .modal-body textarea, #add-content-modal .modal-body select').val('');
        
        // Reset content type to default
        $('#add_content_type').val('text').trigger('change');
      }

      // Add content modal event handlers
      $('#add-content-modal').on('hidden.bs.modal', function() {
        resetAddContentModal();
      });

      $('#add-content-modal').on('show.bs.modal', function() {
        resetAddContentModal();
      });

      // Show/hide options input based on question type
      $(document).off('change', '#add_question_type, #modify_question_type').on('change', '#add_question_type, #modify_question_type', function() {
        updateOptionsVisibility();
      });

      // Function to update options visibility
      function updateOptionsVisibility() {
        // For add question modal
        var addQuestionType = $('#add_question_type').val();
        if (addQuestionType) {
          var choiceTypes = ['mc', 'mc_buttons', 'mc_multiple', 'mc_multiple_buttons', 'select', 'slider'];
          var $optionsDiv = $('#add_question_options_div');
          if (choiceTypes.includes(addQuestionType)) {
            $optionsDiv.show();
          } else {
            $optionsDiv.hide();
          }
        }
        
        // For modify question modal
        var modifyQuestionType = $('#modify_question_type').val();
        if (modifyQuestionType) {
          var choiceTypes = ['mc', 'mc_buttons', 'mc_multiple', 'mc_multiple_buttons', 'select', 'slider'];
          var $optionsInput = $('#modify_question_options');
          var $optionsContainer = $optionsInput.closest('div');
          if (choiceTypes.includes(modifyQuestionType)) {
            $optionsContainer.show();
          } else {
            $optionsContainer.hide();
          }
        }
      }

      // Initialize options visibility on modal show and when DOM updates
      $('#add-content-modal, #modify-content-modal').on('shown.bs.modal', function() {
        // Small delay to ensure DOM is ready
        setTimeout(updateOptionsVisibility, 100);
      });

      // Also trigger when Shiny updates the form content
      $(document).on('shiny:value', function(event) {
        if (event.target.id === 'add_content_form' || event.target.id === 'modify_content_form') {
          setTimeout(updateOptionsVisibility, 100);
        }
      });

      /* ===== BUTTON EVENT HANDLERS ===== */
      // Modify button handlers
      $(document).off('click', '.modify-page-btn').on('click', '.modify-page-btn', function(e) {
        e.preventDefault();
        e.stopPropagation();
        
        var pageId = $(this).attr('data-page-id');
        Shiny.setInputValue('modify_page_btn', {
          pageId: pageId,
          timestamp: new Date().getTime()
        });
        return false;
      });

      $(document).off('click', '.modify-content-btn').on('click', '.modify-content-btn', function(e) {
        e.preventDefault();
        e.stopPropagation();
        
        var pageId = $(this).attr('data-page-id');
        var contentId = $(this).attr('data-content-id');
        var contentType = $(this).attr('data-content-type');
        
        Shiny.setInputValue('modify_content_btn', { 
          pageId: pageId, 
          contentId: contentId, 
          contentType: contentType,
          timestamp: new Date().getTime()
        });
        return false;
      });

      // Add content button handler
      $(document).off('click', '.add-text-btn').on('click', '.add-text-btn', function(e) {
        e.preventDefault();
        e.stopPropagation();
        
        var pageId = $(this).attr('data-page-id');
        Shiny.setInputValue('add_text_btn', {
          pageId: pageId,
          timestamp: new Date().getTime()
        });
        return false;
      });

      $(document).off('click', '.add-question-btn').on('click', '.add-question-btn', function(e) {
        e.preventDefault();
        e.stopPropagation();
        
        var pageId = $(this).attr('data-page-id');
        Shiny.setInputValue('add_question_btn', {
          pageId: pageId,
          timestamp: new Date().getTime()
        });
        return false;
      });

      // Add page button handler
      $(document).off('click', '#add_page_btn').on('click', '#add_page_btn', function(e) {
        e.preventDefault();
        e.stopPropagation();
        
        // This will trigger the Shiny input
        return true;
      });

      // Delete confirmation handlers
      $(document).off('click', '.delete-page-btn').on('click', '.delete-page-btn', function(e) {
        e.preventDefault();
        e.stopPropagation();
        
        var pageId = $(this).attr('data-page-id');
        if (confirm('Are you sure you want to delete page \"' + pageId + '\"? This action cannot be undone.')) {
          Shiny.setInputValue('delete_page_btn', pageId);
        }
        return false;
      });
      
      $(document).off('click', '.delete-content-btn').on('click', '.delete-content-btn', function(e) {
        e.preventDefault();
        e.stopPropagation();
        
        var pageId = $(this).attr('data-page-id');
        var contentId = $(this).attr('data-content-id');
        var contentType = $(this).closest('[data-content-type]').attr('data-content-type');
        
        if (confirm('Are you sure you want to delete this ' + contentType + '? This action cannot be undone.')) {
          Shiny.setInputValue('delete_content_btn', { 
            pageId: pageId, 
            contentId: contentId, 
            contentType: contentType 
          });
        }
        return false;
      });

      /* ===== UI INTERACTION HANDLERS ===== */
      // Page toggle functionality
      function initToggle() {
        $('.page-header').off('click').on('click', function(e) {
          // Don't toggle if clicking on drag handle or action buttons
          if (!$(e.target).hasClass('drag-handle') && 
              !$(e.target).closest('.drag-handle').length &&
              !$(e.target).hasClass('delete-page-btn') &&
              !$(e.target).closest('.delete-page-btn').length &&
              !$(e.target).closest('.page-actions').length) {
            
            var $questions = $(this).next('.questions-container');
            var pageId = $(this).closest('.page-wrapper').attr('data-page-id');
            var willBeExpanded = $questions.is(':hidden'); // Will be expanded after toggle
            
            $questions.slideToggle();
            
            // Toggle icon
            var $icon = $(this).find('.toggle-icon i');
            if ($icon.hasClass('fa-chevron-down')) {
              $icon.removeClass('fa-chevron-down').addClass('fa-chevron-right');
            } else {
              $icon.removeClass('fa-chevron-right').addClass('fa-chevron-down');
            }
            
            // Send toggle state to Shiny
            Shiny.setInputValue('page_toggled', {
              pageId: pageId,
              isExpanded: willBeExpanded
            });
          }
        });
      }

      /* ===== DRAG & DROP FUNCTIONALITY ===== */
      // Initialize drag and drop functionality
      function initSortable() {
        // Pages sortable
        if (document.getElementById('pages-container')) {
          new Sortable(document.getElementById('pages-container'), {
            animation: 150,
            handle: '.page-drag-handle',
            ghostClass: 'sortable-ghost',
            filter: '.delete-page-btn, .page-actions, .modify-page-btn',
            preventOnFilter: true,
            onEnd: function(evt) {
              // Gather the new page order
              var pageOrder = [];
              $('#pages-container > div.page-wrapper').each(function() {
                pageOrder.push($(this).attr('data-page-id'));
              });
              
              // Send page order to Shiny
              Shiny.setInputValue('page_drag_completed', {
                order: pageOrder,
                timestamp: new Date().getTime()
              });
            }
          });
        }
        
        // Content (questions and text) sortable
        $('.questions-container').each(function() {
          var pageId = $(this).attr('data-page-id');
          new Sortable(this, {
            group: 'content-items', // This enables cross-container dragging
            animation: 150,
            handle: '.drag-handle',
            ghostClass: 'sortable-ghost',
            draggable: '.question-item, .text-item',
            filter: '.delete-content-btn, .content-actions, .modify-content-btn, .add-content-btn',
            preventOnFilter: true,
            
            // Expand collapsed pages when dragging starts
            onStart: function(evt) {
              // Add a class to indicate we're dragging
              $('body').addClass('content-dragging');
              
              // Set up hover handlers for page headers during drag
              $('.page-header').on('dragenter.sortable dragover.sortable', function(e) {
                e.preventDefault();
                var $pageHeader = $(this);
                var $questionsContainer = $pageHeader.next('.questions-container');
                
                // If the page is collapsed, expand it immediately
                if ($questionsContainer.is(':hidden')) {
                  $questionsContainer.show(); // Use show() instead of slideDown() for immediate effect
                  var $icon = $pageHeader.find('.toggle-icon i');
                  $icon.removeClass('fa-chevron-right').addClass('fa-chevron-down');
                  
                  // Add visual feedback
                  $pageHeader.addClass('drag-hover');
                }
              });
              
              $('.page-header').on('dragleave.sortable', function(e) {
                $(this).removeClass('drag-hover');
              });
            },
            
            // Clean up when dragging ends
            onEnd: function(evt) {
              // Remove dragging class and clean up event handlers
              $('body').removeClass('content-dragging');
              $('.page-header').off('.sortable').removeClass('drag-hover');
              
              var fromPageId = $(evt.from).attr('data-page-id');
              var toPageId = $(evt.to).attr('data-page-id');
              
              // Create an array of content order for the target page
              var contentOrder = [];
              
              $(evt.to).children('.question-item, .text-item').each(function() {
                var $element = $(this);
                var type = $element.attr('data-content-type');
                var id;
                
                if (type === 'question') {
                  id = $element.attr('data-question-id');
                } else if (type === 'text') {
                  id = $element.attr('data-text-id');
                }
                
                if (id && type) {
                  contentOrder.push({
                    type: type,
                    id: id
                  });
                }
              });
              
              // Convert to serialized format
              var serializedOrder = [];
              for (var i = 0; i < contentOrder.length; i++) {
                serializedOrder.push(contentOrder[i].type);
                serializedOrder.push(contentOrder[i].id);
              }
              
              // Send event to Shiny with both source and target page info
              Shiny.setInputValue('content_drag_completed', {
                fromPageId: fromPageId,
                toPageId: toPageId,
                order: serializedOrder,
                isCrossPage: fromPageId !== toPageId,
                timestamp: new Date().getTime()
              }, {priority: 'event'});
            }
          });
        });
      }

      /* ===== DOM MANAGEMENT & INITIALIZATION ===== */
      // Initialize all functionality together
      function initializeAll() {
        initToggle();
        initSortable();
      }

      // Ensure functionality is reinitialized whenever the DOM changes
      $(document).on('shiny:idle', function(event) {
        initializeAll();
      });
      
      // Watch for changes to the structure output
      var observer = new MutationObserver(function(mutations) {
        initializeAll();
      });
      
      // Start observing changes to the survey structure
      var target = document.getElementById('survey_structure');
      if (target) {
        observer.observe(target, { childList: true, subtree: true });
      }
      
      // Initialize everything on document ready
      initializeAll();
    });
  ")
}
