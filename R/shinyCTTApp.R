#' A wrapper function for the Shiny App. This is the function that should be called to run the app.
#' @param language Which language to start the app in: "en", "de" or "fr". Defaults to the
#'   language R itself is running in, when the app has been translated into it.
#'
#'   This is only the starting point. Each visitor picks their own from the chooser in the
#'   header, which puts ?lang= into the address; that is what every screen reads through
#'   tr(), out of the visitor's own session (see R/translations.R). So two people can have
#'   the app open in different languages at once, whatever this was set to.
#'
#'   Stored as options(shinyCTT.language = language) while the app runs. The three options
#'   the app sets are put back the way they were when it closes.
#' @examples
#' if (interactive()) shinyCTTApp()
#' if (interactive()) shinyCTTApp(language = "de")
#' @returns None. This function is called for its side effect of launching the Shiny app.
#' @export
shinyCTTApp <- function(language = systemLanguage()) {
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
        htmlwidgets.TOJSON_ARGS = list(na = "string"),

        # How every reactable table in the app looks. reactable() reads this by itself -
        # its theme argument defaults to getOption("reactable.theme") - so no table has to
        # be told about it. The one place to change to restyle all of them.
        reactable.theme = reactable::reactableTheme(
          borderColor = "#F0F0F0",
          stripedColor = "#F7F7F7",
          highlightColor = "#F0F7DC",   # a pale version of the FU green
          cellPadding = "6px 8px",

          # fit-content makes a table as wide as its columns need instead of stretching it
          # across the box; maxWidth stops it there when it needs more than the box has,
          # and .rt-table's own overflow-x then scrolls it sideways. Without the maxWidth a
          # 20-item covariance matrix is 2100px wide and hangs out of a 1180px box.
          style = list(fontFamily = "Arial", width = "fit-content", maxWidth = "100%")))

      shiny::onStop(function() options(previousOptions))
    },

    options = list(display.mode = "normal"))
}
