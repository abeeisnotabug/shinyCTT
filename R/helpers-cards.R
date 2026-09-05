## The two shapes of box the app draws with. Every box on every tab is one of these, so
## how a box behaves is changed here and nowhere else. Neither takes a width: a card has
## none, so ui.R and the modules put each box in a column.

## A box: a card with the title in its header.
##
## fillable = FALSE keeps it as tall as its contents; bslib's default cuts a table in half
## (see GOTCHAS.md).
cttCard <- function(..., title = NULL) {
  bslib::card(
    if (!is.null(title)) bslib::card_header(title),
    bslib::card_body(fillable = FALSE, ...))
}

## A box with a tab strip in its header, for the "Overall" / "Group-wise" pairs and the
## per-model strips on the results pages.
##
##   titleRight : the title on the right of the tabs instead of the left. Three of the
##                strips had it there - shinydashboard::tabBox()'s own default.
cttTabCard <- function(..., title = NULL, titleRight = FALSE, id = NULL) {
  strip <- bslib::navset_card_tab(
    id = id,
    title = title,

    # Each panel is as tall as what is in it, for the same reason cttCard() is.
    wrapper = function(...) bslib::card_body(..., fillable = FALSE),
    ...)

  if (titleRight) div(class = "cttTitleRight", strip) else strip
}
