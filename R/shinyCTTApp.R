#' @export
shinyCTTApp <- function() {
  appDir <- system.file("shinyCTTApp", package = "shinyCTT")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `shinyCTT`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
