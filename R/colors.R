## Every colour the app draws with, written down once. Nothing else in the package names
## a colour.

## The plots' colours.
##
## The old ones were a blue that appeared nowhere else in the app, and, for the group-wise
## plots, ggplot2's own defaults - hcl() spaced evenly round the colour wheel, which put a
## magenta and a cyan next to the FU green of the header.
fuColors <- function() {
  list(
    # The Freie Universitaet's green, the header bar's colour: what a filled area - the
    # histogram's bars - is drawn in. `mark` is the same green darkened, for the things
    # that are a thin line or a small point and would be washed out in the bright one:
    # the scatter's dots and the density curve over the bars.
    fill = "#99CC00",
    mark = "#4F6B1E",

    # What a plot draws several groups in, in order, wrapping round when there are more
    # groups than colours. The first is the FU green again, the second the blue the app's
    # value boxes already use, the third the red a table rates a bad cell with.
    groups = c("#99CC00", "#003F8A", "#B33A2B", "#E0A32E", "#5B7C99", "#767676"))
}

## The three colours a table cell can be painted, and the text colour that goes on top of
## them. A table works out a rating - "good", "bad" or "neutral" - and ratingStyle() turns
## that into a colour, so no table has to be handed one.
cttColors <- function() {
  list(
    good = "#6B8E23",     # olivedrab: the FU green #99CC00, a little darkened
    bad = "#B33A2B",      # a brick red of about the same weight
    neutral = "#767676",  # the grey the dashboard theme already uses
    text = "#FFFFFF")
}

## What a rated cell looks like. Handed to a column's style, which reactable calls once per
## row. NA -> no colour at all, for a cell with nothing to rate (the empty upper triangle of
## the correlation table, the first row of the hierarchical table).
ratingStyle <- function(rating) {
  if (length(rating) != 1 || is.na(rating)) return(NULL)

  list(background = cttColors()[[rating]], color = cttColors()$text)
}

## The colour each group is drawn in, and a paler version of each for the density curves
## drawn over the histogram bars.
##
## Named by group on purpose: a discrete ggplot2 scale hands its palette to whichever
## levels are still in the data, so de-selecting a group used to recolour the ones left
## (see GOTCHAS.md).
groupPalette <- function(groups) {
  palette <- fuColors()$groups
  solid <- palette[(seq_along(groups) - 1) %% length(palette) + 1]

  list(
    solid = stats::setNames(solid, groups),
    light = stats::setNames(mixTowardsWhite(solid, 0.4), groups))
}

## `amount` of the way from each colour to white.
mixTowardsWhite <- function(colors, amount) {
  grDevices::rgb(
    t((1 - amount) * grDevices::col2rgb(colors) + amount * 255),
    maxColorValue = 255)
}
