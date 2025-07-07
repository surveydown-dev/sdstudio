# Main UI framework
studio_ui <- function() {
  shiny::div(
    style = "position: relative;",

    # Unified floating button container (top-right)
    shiny::div(
      id = "unified-floating-buttons",
      style = "position: fixed; top: 20px; right: 20px; z-index: 2000;",
      shiny::div(
        style = "display: flex; align-items: center; gap: 10px; background: rgba(255, 255, 255, 0.95); padding: 5px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.15); backdrop-filter: blur(10px);",

        # Group 1: Refresh buttons (leftmost, context-dependent)
        shiny::div(
          id = "refresh-group",
          class = "button-group",
          style = "display: none; align-items: center;",
          # Preview refresh button
          shiny::actionButton(
            "preview_refresh_btn",
            shiny::HTML('<i class="fas fa-sync-alt"></i>'),
            class = "btn-outline-success",
            style = "padding: 8px 12px; font-size: 0.875rem; border-radius: 6px; display: none;",
            title = "Refresh Preview"
          ),
          # Responses refresh button
          shiny::actionButton(
            "responses_refresh_btn",
            shiny::HTML('<i class="fas fa-sync-alt"></i>'),
            class = "btn-outline-success",
            style = "padding: 8px 12px; font-size: 0.875rem; border-radius: 6px; display: none;",
            title = "Refresh Response Data"
          )
        ),

        # Separator 1 (only visible when refresh group is visible)
        shiny::div(
          id = "separator-1",
          style = "display: none; height: 24px; width: 1px; background-color: #dee2e6;"
        ),

        # Group 2: View Mode (Desktop/Mobile, only on Preview tab)
        shiny::div(
          id = "view-group",
          class = "button-group",
          style = "display: none; align-items: center; gap: 5px;",
          shiny::actionButton(
            "preview_widescreen_btn",
            shiny::HTML('<i class="fas fa-desktop"></i>'),
            class = "btn-outline-primary active",
            style = "padding: 8px 12px; font-size: 0.875rem; border-radius: 6px;",
            title = "Desktop View"
          ),
          shiny::actionButton(
            "preview_mobile_btn",
            shiny::HTML('<i class="fas fa-mobile-alt"></i>'),
            class = "btn-outline-primary",
            style = "padding: 8px 12px; font-size: 0.875rem; border-radius: 6px;",
            title = "Mobile View"
          )
        ),

        # Separator 2 (only visible when both view and code groups are visible)
        shiny::div(
          id = "separator-2",
          style = "display: none; height: 24px; width: 1px; background-color: #dee2e6;"
        ),

        # Group 3: Code Mode (Local/Live, visible on all tabs)
        shiny::div(
          id = "code-group",
          class = "button-group",
          style = "display: flex; align-items: center; gap: 5px;",
          shiny::actionButton(
            "code_local_btn",
            shiny::HTML('<i class="fas fa-laptop"></i>'),
            class = "btn-outline-warning",
            style = "padding: 8px 12px; font-size: 0.875rem; border-radius: 6px;",
            title = "Local Mode"
          ),
          shiny::actionButton(
            "code_live_btn",
            shiny::HTML('<i class="fas fa-cloud"></i>'),
            class = "btn-outline-primary active",
            style = "padding: 8px 12px; font-size: 0.875rem; border-radius: 6px;",
            title = "DB Mode"
          )
        )
      )
    ),

    # Main navbar
    shiny::navbarPage(
      title = "surveydown Studio",
      id = "tabset",
      theme = bslib::bs_theme(version = 5),
      ui_construction_tab(),
      ui_preview_tab(),
      ui_dashboard_tab()
    )
  )
}

# Template Selection UI
ui_template_selection <- function() {
  shiny::div(
    class = "template-selection-container",
    style = "padding: 40px; text-align: center; height: calc(100vh - 120px); display: flex; flex-direction: column; justify-content: center;",

    shiny::div(
      style = "width: 500px; margin: 0 auto;",

      shiny::h2(
        "Create New Survey",
        style = "margin-bottom: 30px; color: #333; text-align: center; margin-left: 135px;"
      ),

      # Form table layout
      shiny::tags$table(
        style = "width: 100%; border-collapse: separate; border-spacing: 0 15px;",
        shiny::tags$tr(
          shiny::tags$td(
            style = "width: 120px; font-weight: bold; vertical-align: middle; padding-right: 15px;",
            "Template:"
          ),
          shiny::tags$td(
            style = "vertical-align: middle; margin: 0.5rem;",
            shiny::selectInput(
              "template_select",
              NULL,
              choices = list(
                "Basic Templates" = list(
                  "Default" = "default",
                  "Question Types" = "question_types"
                ),
                "Advanced Features" = list(
                  "Conditional Display" = "conditional_display",
                  "Conditional Navigation" = "conditional_navigation",
                  "Random Options" = "random_options",
                  "Random Options (Predefined)" = "random_options_predefined",
                  "Reactive Drilldown" = "reactive_drilldown",
                  "Reactive Questions" = "reactive_questions"
                ),
                "Specialized" = list(
                  "Conjoint (Buttons)" = "conjoint_buttons",
                  "Conjoint (Tables)" = "conjoint_tables",
                  "Custom Leaflet Map" = "custom_leaflet_map",
                  "Custom Plotly Chart" = "custom_plotly_chart",
                  "External Redirect" = "external_redirect",
                  "Live Polling" = "live_polling"
                )
              ),
              selected = "default",
              width = "100%"
            )
          )
        ),
        shiny::tags$tr(
          shiny::tags$td(
            style = "width: 120px; font-weight: bold; vertical-align: middle; padding-right: 15px;",
            "Directory:"
          ),
          shiny::tags$td(
            style = "vertical-align: middle;",
            shiny::div(
              shiny::actionButton(
                "path_display_btn",
                paste0(basename(getwd()), "/"),
                class = "btn-outline-secondary",
                style = "width: 100%; text-align: center; font-family: monospace; padding: 8px 12px; margin: 0.5rem;",
                title = paste("Click to edit:", getwd())
              ),
              shiny::div(
                style = "display: none;",
                shiny::textInput(
                  "path_input",
                  NULL,
                  value = getwd()
                )
              )
            )
          )
        )
      ),

      # Create button
      shiny::div(
        style = "margin-top: 30px; margin-left: 135px;",
        shiny::actionButton(
          "create_survey_btn",
          "Create Survey",
          class = "btn-primary btn-lg",
          style = "padding: 12px 30px; font-size: 16px; font-weight: bold;"
        )
      )
    ),

    # Directory edit modal
    shiny::div(
      id = "edit-directory-modal",
      class = "modal fade",
      tabindex = "-1",
      shiny::div(
        class = "modal-dialog modal-lg",
        shiny::div(
          class = "modal-content",
          shiny::div(
            class = "modal-header",
            shiny::h5("Edit Directory Path", class = "modal-title"),
            shiny::tags$button(
              type = "button",
              class = "btn-close",
              `data-bs-dismiss` = "modal"
            )
          ),
          shiny::div(
            class = "modal-body",
            shiny::textInput(
              "path_edit_input",
              "Directory Path:",
              value = getwd(),
              width = "100%",
              placeholder = "Enter full directory path"
            ),
            shiny::p(
              style = "font-size: 0.9em; color: #666; margin-top: 10px;"
            )
          ),
          shiny::div(
            class = "modal-footer",
            shiny::actionButton(
              "confirm_path_edit",
              "Confirm",
              class = "btn btn-primary"
            ),
            shiny::tags$button(
              "Cancel",
              type = "button",
              class = "btn btn-secondary",
              `data-bs-dismiss` = "modal"
            )
          )
        )
      )
    )
  )
}

# Build tab UI
ui_construction_tab <- function() {
  shiny::tabPanel(
    "Build",

    shiny::tags$head(
      htmltools::htmlDependency(
        name = "sdstudio",
        version = "1.0.0",
        src = c(file = system.file("", package = "sdstudio")),
        stylesheet = "css/sdstudio.css",
        script = "js/sdstudio.js"
      ),
      shiny::tags$script(
        src = "https://cdn.jsdelivr.net/npm/sortablejs@1.14.0/Sortable.min.js"
      ),

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
              shiny::tags$button(
                type = "button",
                class = "btn-close",
                `data-bs-dismiss` = "modal"
              )
            ),
            shiny::div(
              class = "modal-body",
              shiny::textInput("modify_page_id_input", "Page ID:", value = "")
            ),
            shiny::div(
              class = "modal-footer",
              shiny::actionButton(
                "modify_page_confirm",
                "Save Changes",
                class = "btn btn-primary"
              ),
              shiny::tags$button(
                "Cancel",
                type = "button",
                class = "btn btn-secondary",
                `data-bs-dismiss` = "modal"
              )
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
              shiny::h5(
                id = "modify-content-modal-title",
                "Modify Content",
                class = "modal-title"
              ),
              shiny::tags$button(
                type = "button",
                class = "btn-close",
                `data-bs-dismiss` = "modal"
              )
            ),
            shiny::div(
              class = "modal-body",
              shiny::uiOutput("modify_content_form")
            ),
            shiny::div(
              class = "modal-footer",
              shiny::actionButton(
                "modify_content_confirm",
                "Save Changes",
                class = "btn btn-primary"
              ),
              shiny::tags$button(
                "Cancel",
                type = "button",
                class = "btn btn-secondary",
                `data-bs-dismiss` = "modal"
              )
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
              shiny::h5(
                id = "add-content-modal-title",
                "Add Content",
                class = "modal-title"
              ),
              shiny::tags$button(
                type = "button",
                class = "btn-close",
                `data-bs-dismiss` = "modal"
              )
            ),
            shiny::div(
              class = "modal-body",
              shiny::uiOutput("add_content_form")
            ),
            shiny::div(
              class = "modal-footer",
              shiny::actionButton(
                "add_content_confirm",
                "Add Content",
                class = "btn btn-primary"
              ),
              shiny::tags$button(
                "Cancel",
                type = "button",
                class = "btn btn-secondary",
                `data-bs-dismiss` = "modal"
              )
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
              shiny::tags$button(
                type = "button",
                class = "btn-close",
                `data-bs-dismiss` = "modal"
              )
            ),
            shiny::div(
              class = "modal-body",
              shiny::textInput(
                "add_page_id_input",
                "Page ID:",
                placeholder = "Enter page ID"
              ),
              shiny::uiOutput("add_page_position_ui")
            ),
            shiny::div(
              class = "modal-footer",
              shiny::actionButton(
                "add_page_confirm",
                "Add Page",
                class = "btn btn-success"
              ),
              shiny::tags$button(
                "Cancel",
                type = "button",
                class = "btn btn-secondary",
                `data-bs-dismiss` = "modal"
              )
            )
          )
        )
      )
    ),

    # Conditional content based on whether survey exists
    shiny::uiOutput("build_tab_content")
  )
}

# Normal Build Interface UI
ui_normal_build <- function() {
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
          shiny::h5(
            "Structure",
            style = "margin: 0; text-align: center; flex-grow: 1;"
          ),

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
            style = "overflow-y: auto; height: calc(100vh - 141px);",
            shiny::uiOutput("survey_structure")
          ),
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
          shiny::h5(
            "Code",
            style = "margin: 0; text-align: center; flex-grow: 1;"
          )
        ),

        # Code panel with tabs
        shiny::wellPanel(
          style = "background-color: #f0fff0; border-color: #d4edda; padding: 0.5rem; position: relative;",

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
}

# Preview tab UI
ui_preview_tab <- function() {
  shiny::tabPanel(
    "Preview",
    shiny::div(
      style = "display: flex; flex-direction: column; height: calc(100vh - 79px); padding: 10px; position: relative;",

      # Preview container (now takes full height)
      shiny::div(
        style = "flex: 1; display: flex; justify-content: center; align-items: flex-start;",
        shiny::div(
          id = "preview_container",
          style = "width: 100%; height: 100%; max-width: 100%; border: 1px solid #ddd; border-radius: 5px; transition: all 0.3s ease;",
          shiny::uiOutput("preview_frame")
        )
      )
    )
  )
}

# Responses tab UI
ui_dashboard_tab <- function() {
  shiny::tabPanel(
    "Responses",

    shiny::tags$head(
      shiny::tags$script(
        "
        function myFunction() {
          var x = document.getElementById('password');
          if (x.type === 'password') {
            x.type = 'text';
            document.getElementById('toggle_password').innerText = 'Hide';
          } else {
            x.type = 'password';
            document.getElementById('toggle_password').innerText = 'Show';
          }
        }
        
        $(document).ready(function() {
          $('#toggle_db_settings').click(function() {
            var form = $('#db_settings_form');
            var button = $(this);
            if (form.is(':visible')) {
              form.slideUp();
              button.find('i').removeClass('fa-times').addClass('fa-cog');
              button.contents().filter(function() {
                return this.nodeType === 3;
              }).remove();
              button.append(' Settings');
            } else {
              form.slideDown();
              button.find('i').removeClass('fa-cog').addClass('fa-times');
              button.contents().filter(function() {
                return this.nodeType === 3;
              }).remove();
              button.append(' Close');
            }
          });
        });
      "
      )
    ),

    # Database Connection (full width, collapsible)
    bslib::card(
      id = "database_connection_card",
      bslib::card_header(
        class = "d-flex align-items-center justify-content-between",
        shiny::div(
          class = "d-flex align-items-center",
          "Database Connection",
          shiny::actionButton(
            "toggle_db_settings",
            "Settings",
            class = "btn-sm btn-outline-primary ms-3",
            style = "width: 90px;",
            icon = shiny::icon("cog")
          )
        ),
        # Connection state indicator
        shiny::div(
          id = "connection_state_indicator",
          class = "d-flex align-items-center",
          shiny::span(
            id = "connection_icon",
            class = "me-2",
            style = "font-size: 1.2em;",
            shiny::icon("circle", class = "text-secondary") # Default gray state
          ),
          shiny::span(
            id = "connection_text",
            class = "text-muted small",
            "Not connected"
          )
        )
      ),
      bslib::card_body(
        # Update form (collapsible)
        shiny::div(
          id = "db_settings_form",
          style = "display: none;",
          shiny::textInput("host", "Host:", value = Sys.getenv("SD_HOST", "")),
          shiny::textInput("port", "Port:", value = Sys.getenv("SD_PORT", "")),
          shiny::textInput(
            "dbname",
            "Database Name:",
            value = Sys.getenv("SD_DBNAME", "")
          ),
          shiny::textInput("user", "User:", value = Sys.getenv("SD_USER", "")),
          shiny::div(
            id = "password-container",
            style = "position: relative;",
            shiny::div(
              style = "display: flex; align-items: center;",
              shiny::passwordInput(
                "password",
                "Password:",
                value = Sys.getenv("SD_PASSWORD", "")
              ),
              shiny::div(
                style = "margin-left: 10px; margin-top: 1em;",
                shiny::actionButton(
                  "toggle_password",
                  "Show",
                  class = "btn-sm btn-secondary",
                  onclick = "myFunction()"
                )
              )
            )
          ),
          shiny::textInput(
            "default_table",
            "Table:",
            value = Sys.getenv("SD_TABLE", "")
          ),
          shiny::div(
            style = "margin-top: 20px;",
            shiny::actionButton(
              "test_connection",
              "Test Connection",
              class = "btn-primary",
              style = "width: 300px;"
            )
          ),
          shiny::textOutput("connection_status")
        )
      )
    ),

    # Table Selection
    shiny::selectizeInput(
      "table_select",
      "Choose a table to view:",
      choices = c("Loading..." = ""),
      width = "20%",
      options = list(
        create = TRUE,
        placeholder = "Select or type table name..."
      )
    ),

    # Value boxes
    bslib::layout_column_wrap(
      width = 1 / 3,
      heights_equal = "row",
      bslib::value_box(
        title = "Total Responses",
        value = shiny::textOutput("total_responses"),
        showcase = shiny::icon("table")
      ),
      bslib::value_box(
        title = "Daily Average",
        value = shiny::textOutput("daily_average"),
        showcase = shiny::icon("chart-line")
      ),
      bslib::value_box(
        title = "Completion Rate",
        value = shiny::textOutput("completion_rate"),
        showcase = shiny::icon("check-circle")
      )
    ),

    # Charts
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Cumulative Responses"),
        shiny::plotOutput("cumulative_trend", height = "300px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Daily Responses"),
        shiny::plotOutput("daily_trend", height = "300px")
      )
    ),

    # Data table
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Survey Responses",
        shiny::downloadButton(
          "download_survey_data",
          "Download CSV",
          class = "btn-sm btn-secondary"
        )
      ),
      DT::dataTableOutput("survey_data_table")
    )
  )
}
