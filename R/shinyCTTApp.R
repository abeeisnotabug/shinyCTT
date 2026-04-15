#' @export
shinyCTTApp <- function() {
  # appDir <- system.file("shinyCTTApp", package = "shinyCTT")
  # if (appDir == "") {
  #   stop("Could not find directory. Try re-installing `shinyCTT`.", call. = FALSE)
  # }

  # For some reason, the app lags on startup if mvn has not run before at least once
  # suppressMessages(invisible(MVN::mvn(rnorm(10))))

  shiny::shinyApp(
    ui = ui,
    server = server,
    options = list(display.mode = "normal"))
}
