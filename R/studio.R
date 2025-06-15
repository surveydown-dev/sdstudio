#' Launch surveydown Studio
#'
#' This function launches a Shiny app with 4 tabs: Build, Preview, Responses, and Settings.
#' The Build tab includes a template selection interface for creating new surveys.
#'
#' @param gssencmode Character string. The GSS encryption mode for the database
#'   connection. Defaults to `"prefer"`. Set to `"disable"` if you're having
#'   connection issues on a secure connection like a VPN.
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
#'   
#'   # Launch studio with disabled GSS encryption (for VPN connections)
#'   sd_studio(gssencmode = "disable")
#' }
sd_studio <- function(gssencmode = "prefer") {
  shiny::shinyApp(ui = studio_ui(), server = studio_server(gssencmode = gssencmode))
}
