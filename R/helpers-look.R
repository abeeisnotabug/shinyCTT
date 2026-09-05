## How the app looks, in two halves:
##
##   fuTheme()  - the settings Bootstrap has a name for: the colours, the font, the base
##                text size, the green bar. bslib compiles these into a style sheet.
##   fuStyle()  - everything else, out of inst/styles.css and into the page's head.
##
## A rule goes in the style sheet, never in the theme - bslib serves its own CSS after the
## theme and silently wins (see GOTCHAS.md, and WALKTHROUGH.md 6b).

## ---- the FU look ----
fuTheme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#000000",
    primary = "#99CC00",
    base_font = "Arial",
    "font-size-base" = "0.8125rem",

    # The green bar across the top, and it is one setting: page_sidebar() paints its bar
    # with $navbar-bg and leaves it the colour of the page when that is unset.
    "navbar-bg" = "#99CC00")
}

## Everything the theme has no setting for, out of inst/styles.css. Read the note at the top
## of this file, and the one at the top of that one, before adding to it.
fuStyle <- function() {
  # The rules are in inst/styles.css, so that an editor can see them as CSS rather than as
  # one long piece of R text. includeCSS() reads the file and writes it into the page.
  tags$head(includeCSS(system.file("styles.css", package = "shinyCTT")))
}
