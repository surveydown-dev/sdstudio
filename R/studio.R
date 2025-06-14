#' Launch surveydown Studio
#'
#' This function serves two purposes:
#' 1. It generates a new survey project from a template
#' 2. It launches a Shiny app with 4 tabs: Construction, Preview, Dashboard, and Settings
#'
#' @param template A character string specifying the template to use.
#'   Default is "default" which uses the built-in package template.
#'   See `surveydown::sd_create_survey()` for other available templates.
#' @param path A character string specifying the directory where the survey
#'   template should be created. Defaults to the current working directory.
#' @param gssencmode Character string. The GSS encryption mode for the database
#'   connection. Defaults to `"prefer"`. Set to `"disable"` if you're having
#'   connection issues on a secure connection like a VPN.
#'
#' @return No return value, called for its side effects of creating a survey
#'   project and launching a Shiny app.
#' @importFrom stats runif setNames
#' @importFrom utils head
#' @importFrom shiny req
#' @importFrom surveydown sd_create_survey sd_db_connect
#' @importFrom htmltools htmlDependency
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Launch studio with default template
#'   sd_studio()
#'
#'   # Launch studio with a specific template and path
#'   sd_studio(template = "question_types", path = "my_survey")
#'   
#'   # Launch studio with disabled GSS encryption (for VPN connections)
#'   sd_studio(gssencmode = "disable")
#' }
sd_studio <- function(template = "default", path = getwd(), gssencmode = "prefer") {
  template_explicitly_provided <- !missing(template)
  survey_exists <- file.exists(file.path(path, "survey.qmd"))
  app_exists <- file.exists(file.path(path, "app.R"))

  if (template_explicitly_provided || !survey_exists || !app_exists) {
    surveydown::sd_create_survey(template = template, path = path)
  }
  
  original_dir <- getwd()
  on.exit(setwd(original_dir), add = TRUE)
  setwd(path)
  
  shiny::shinyApp(ui = studio_ui(), server = studio_server(gssencmode = gssencmode))
}
