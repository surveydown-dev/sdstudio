#' Launch surveydown Studio
#'
#' This function launches a Shiny app with 3 tabs: Build, Preview, and Responses.
#' The Build tab includes a template selection interface for creating new surveys.
#'
#' @param gssencmode Character string. The GSS encryption mode for the database
#'   connection. Defaults to `"auto"`. Options include:
#'   \itemize{
#'     \item `"auto"`: Tries `"prefer"` first, then falls back to `"disable"` if GSSAPI fails (recommended)
#'     \item `"prefer"`: Uses GSSAPI encryption if available, plain connection if not
#'     \item `"disable"`: Forces plain connection without GSSAPI encryption
#'   }
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
#'   # Launch studio (uses "auto" mode by default)
#'   sd_studio()
#'   
#'   # Launch studio with disabled GSS encryption (for VPN connections)
#'   sd_studio(gssencmode = "disable")
#'   
#'   # Launch studio with prefer mode (no fallback)
#'   sd_studio(gssencmode = "prefer")
#' }
sd_studio <- function(gssencmode = "auto") {
  shiny::shinyApp(ui = studio_ui(), server = studio_server(gssencmode = gssencmode))
}
