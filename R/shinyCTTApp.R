#' @export
shinyCTTApp <- function(launch.browser = TRUE) {
  appDir <- system.file("shinyCTTApp", package = "shinyCTT")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `shinyCTT`.", call. = FALSE)
  }

  # For some reason, the app lags on startup if mvn has not run before at least once
  suppressMessages(invisible(MVN::mvn(rnorm(10))))

  shiny::runApp(appDir, display.mode = "normal", launch.browser = launch.browser)
}
