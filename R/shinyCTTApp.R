#' A wrapper function for the Shiny App. This is the function that should be called to run the app.
#' @param language Which language to show the app in: "en" (the default) or "de". Stored as
#'   options(shinyCTT.language = language) while the app runs, which every screen reads
#'   through tr() (see R/translations.R). Both options the app sets are put back the way they
#'   were when it closes.
#' @examples
#' if (interactive()) shinyCTTApp()
#' if (interactive()) shinyCTTApp(language = "de")
#' @returns None. This function is called for its side effect of launching the Shiny app.
#' @export
shinyCTTApp <- function(language = "en") {
  shiny::shinyApp(
    ui = ui,
    server = server,

    # The two settings the app needs while it runs, set when it starts and put back when it
    # stops. options() hands back what each one was before, and feeding that list to
    # options() again restores it - including removing one that was not set at all.
    #
    # This is onStart rather than the body of shinyCTTApp(), because shinyApp() only builds
    # the app object; the app runs afterwards, when that object is printed.
    onStart = function() {
      previousOptions <- options(
        shinyCTT.language = language,

        # Without this, DT leaves a missing value as an empty cell rather than printing NA.
        htmlwidgets.TOJSON_ARGS = list(na = "string"))

      shiny::onStop(function() options(previousOptions))
    },

    options = list(display.mode = "normal"))
}
