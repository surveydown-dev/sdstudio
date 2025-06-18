#' Launch surveydown Studio
#'
#' This function launches a Shiny app with 3 tabs: Build, Preview, and Responses.
#' The Build tab includes a template selection interface for creating new surveys.
#' Database connections automatically attempt GSS encryption first, falling back to
#' disabled encryption if needed.
#'
#' @return No return value, called for its side effects of launching a Shiny app.
#' @importFrom stats runif setNames
#' @importFrom utils head
#' @importFrom shiny req
#' @importFrom surveydown sd_create_survey sd_db_connect
#' @importFrom htmltools htmlDependency
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Launch studio
#'   sd_studio()
#' }
sd_studio <- function() {
  shiny::shinyApp(ui = studio_ui(), server = studio_server())
}
