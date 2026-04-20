#' @export
shinyCTTApp <- function() {
  shiny::shinyApp(
    ui = ui,
    server = server,
    options = list(display.mode = "normal"))
}
