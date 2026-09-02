#' A wrapper function for the Shiny App. This is the function that should be called to run the app.
#' @param language Which language to show the app in: "en" (the default) or "de". Stored as
#'   options(shinyCTT.language = language) before the app starts, which every screen reads
#'   through tr() (see R/translations.R).
#' @examples
#' if (interactive()) shinyCTTApp()
#' if (interactive()) shinyCTTApp(language = "de")
#' @returns None. This function is called for its side effect of launching the Shiny app.
#' @export
shinyCTTApp <- function(language = "en") {
  options(shinyCTT.language = language)

  shiny::shinyApp(
    ui = ui,
    server = server,
    options = list(display.mode = "normal"))
}
