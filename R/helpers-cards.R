## The two shapes of box the app draws with. Every box on every tab is one of these, so
## how a box behaves is changed here and nowhere else. Neither takes a width: a card has
## none, so ui.R and the modules put each box in a column.

## Every box title ends in a colon, so the text in inst/translations.csv carries none.
## HTML() because a title may hold an entity or a symbol - see sym.eta.hat.
boxTitle <- function(title) HTML(paste0(title, tr("common.colon")))

## A box: a card with the title in its header.
##
## fillable = FALSE keeps it as tall as its contents; bslib's default cuts a table in half
## (see GOTCHAS.md).
cttCard <- function(..., title = NULL) {
  bslib::card(
    if (!is.null(title)) bslib::card_header(boxTitle(title)),
    bslib::card_body(fillable = FALSE, ...))
}

## A box with a tab strip in its header, for the "Overall" / "Group-wise" pairs and the
## per-model strips on the results pages. The title sits left of the tabs, on every strip.
cttTabCard <- function(..., title = NULL, id = NULL) {
  bslib::navset_card_tab(
    id = id,
    title = if (!is.null(title)) boxTitle(title),

    # Each panel is as tall as what is in it, for the same reason cttCard() is.
    wrapper = function(...) bslib::card_body(..., fillable = FALSE),
    ...)
}
