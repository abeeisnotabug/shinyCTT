#' A wrapper function for the Shiny App. This is the function that should be called to run the app. It has no arguments.
#' @examples
#' if (interactive()) shinyCTTApp()
#' @returns None. This function is called for its side effect of launching the Shiny app.
#' @export
shinyCTTApp <- function() {
  shiny::shinyApp(
    ui = ui,
    server = server,
    options = list(display.mode = "normal"))
}
