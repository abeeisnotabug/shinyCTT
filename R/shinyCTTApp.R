#' A wrapper function for the Shiny App. This is the function that should be called to run the app.
#' @param language Which language to start the app in: "en", "de" or "fr". Defaults to the
#'   language R itself is running in, when the app has been translated into it.
#'
#'   This is only the starting point. Each visitor picks their own from the chooser in the
#'   header, which puts ?lang= into the address; that is what every screen reads through
#'   tr(), out of the visitor's own session (see R/helpers-translations.R). So two people can have
#'   the app open in different languages at once, whatever this was set to.
#'
#'   Stored as options(shinyCTT.language = language) while the app runs. Every option the
#'   app sets is put back the way it was when it closes - see the onStart block below for
#'   which ones.
#' @param workspace Whether to offer the objects lying around in R as a data source.
#'
#'   The default is TRUE when the app is started from somebody's own console and FALSE
#'   otherwise, which is what a hosted app is: there globalenv() holds whatever was left
#'   there by whoever put the app up, and nothing of the visitor's. Set it by hand when the
#'   test gets it wrong, as it does for R -e 'shinyCTT::shinyCTTApp()'.
#'
#'   Uploading a .RData, .rda or .rds file is offered either way, and is the way in when
#'   the workspace is not on offer.
#' @param data Data sets to start the app with, as a **named list of data frames** —
#'   `data = list(scores = myData)`. NULL, the default, means the app comes with none.
#'
#'   They appear as their own entry at the top of the source list, "Supplied data", which is
#'   the one selected when the app opens, and the names of the list are what the chooser
#'   under it offers. This is how a hosted copy comes with its own data: the visitor sees it
#'   ready to analyse and does not have to upload anything.
#'
#'   Everything else stays on offer beside it, so a visitor can still bring their own file.
#'
#'   A bare data frame is refused, because the name is what the factor score downloads are
#'   called after and what the exported script says — pass it in a list, where you name it
#'   yourself.
#' @examples
#' if (interactive()) shinyCTTApp()
#' if (interactive()) shinyCTTApp(language = "de")
#' if (interactive()) shinyCTTApp(workspace = FALSE)
#' if (interactive()) shinyCTTApp(data = list(rtdata = rtdata))
#' @returns None. This function is called for its side effect of launching the Shiny app.
#' @export
shinyCTTApp <- function(language = systemLanguage(), workspace = interactive(),
                        data = NULL) {

  # Checked here rather than in the app, so that whoever is starting it is told in their own
  # console instead of every visitor being shown a chooser with nothing in it.
  if (!is.null(data)) {
    if (!is.list(data) || is.data.frame(data) || is.null(names(data)) ||
        any(!nzchar(names(data))))
      stop("data must be a named list of data frames, as in ",
           "data = list(scores = myData).")

    if (!all(vapply(data, is.data.frame, logical(1))))
      stop("Every element of data must be a data frame.")
  }

  shiny::shinyApp(
    ui = ui,
    server = server,

    # The settings the app needs while it runs, set when it starts and put back when it
    # stops. options() hands back what each one was before, and feeding that list to
    # options() again restores it - including removing one that was not set at all.
    #
    # This is onStart rather than the body of shinyCTTApp(), because shinyApp() only builds
    # the app object; the app runs afterwards, when that object is printed.
    onStart = function() {
      previousOptions <- options(
        shinyCTT.language = language,

        # Read by dataSourceUI(), to decide whether the source list offers the workspace.
        shinyCTT.workspace = workspace,

        # The data sets the app comes with. Step 1 offers them as their own source.
        shinyCTT.data = data,

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

      # How big an uploaded file may be. shiny's own default is 5 MB, and a CSV of 5,000
      # people by 200 items is around 10 MB, so that bites on a real study. Left alone when
      # the caller has set it themselves.
      if (is.null(getOption("shiny.maxRequestSize")))
        previousOptions <- c(previousOptions,
                             options(shiny.maxRequestSize = 50 * 1024^2))

      shiny::onStop(function() options(previousOptions))
    },

    options = list(display.mode = "normal"))
}
