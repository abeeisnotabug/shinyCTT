## The two shapes of box the app draws with. Every box on every tab is one of these, so
## how a box behaves is changed here and nowhere else.
##
## They were shinydashboard::box() and shinydashboard::tabBox(), which took a `width` in
## twelfths and wrapped themselves in a column. A bslib card has no width of its own, so
## the widths moved out to the call sites - ui.R and the modules' own layouts.

## A box: a card with the title in its header, as tall as what is in it.
##
## fillable = FALSE is what makes it as tall as its contents. Left at bslib's default a
## card hands its own height to what is inside it, and a table comes out cut off half way
## down with no pager (see GOTCHAS.md).
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
