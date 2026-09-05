## The five CTT models: names, labels, how many items each needs, how they nest, and where
## the hierarchical plot draws them. See WALKTHROUGH.md to add or rename one.


## Works out which models can be compared with each other.
##
## Arguments
##   models    : the model names, e.g. c("tko", "ete", "teq", "etp", "tpa").
##   hierarchy : a two-column matrix. Each row is one edge of the nesting graph, written
##               c(parent, child), where the child is the *more constrained* model. The row
##               c("tko", "ete") says: the essentially tau-equivalent model is a special
##               case of the tau-congeneric model.
##
## Returns one name per comparable pair, the two model names stuck together with the more
## constrained one first: "etetko" is ess. tau-equivalent vs. tau-congeneric. Models that
## are not nested in each other are left out - there is no valid test between them.
nestedPairs <- function(models, hierarchy) {

  # Collected as we go; one entry per comparable pair.
  pairs <- character(0)

  # Take each model in turn and find every model nested inside it.
  for (parentModel in models) {

    # The models sitting directly below this one. `hierarchy` holds the edges as rows, so
    # this keeps the rows whose first column is our model, and takes their second column.
    descendants <- hierarchy[hierarchy[, 1] == parentModel, 2]

    # Add the models below those, then the ones below them, and so on. Nothing can be more
    # than length(models) - 1 steps down, so that many passes always reach the bottom.
    for (step in seq_along(models)) {
      childrenOfWhatWeHave <- hierarchy[hierarchy[, 1] %in% descendants, 2]
      descendants <- union(descendants, childrenOfWhatWeHave)
    }

    # One pair name per descendant. Looping over `models` keeps them in the family's own
    # order, so the comparison tables always list them the same way round.
    for (childModel in models) {
      if (childModel %in% descendants) {
        pairs <- c(pairs, paste0(childModel, parentModel))
      }
    }
  }

  # Named after themselves, so a pair can be looked up as family$comparable[["etetko"]].
  stats::setNames(nm = pairs)
}


cttModelFamily <- function() {

  # The five models, in order from least to most constrained.
  models <- c("tko", "ete", "teq", "etp", "tpa")

  # The nesting graph, one row per edge, written c(parent, child) where the child is the
  # more constrained model.
  #
  #     tko  ->  ete  ->  teq  ->  tpa
  #                \                /
  #                 ->  etp  ------
  #
  # No edge joins teq and etp: same degrees of freedom, neither nested in the other. That
  # is the one pair the grid shows as "Not testable."
  hierarchy <- rbind(
    c("tko", "ete"),
    c("ete", "teq"),
    c("ete", "etp"),
    c("teq", "tpa"),
    c("etp", "tpa"))

  list(
    # The model names. Named after themselves so that models[["tko"]] works, and so that
    # subsetting keeps the names: models[c(TRUE, FALSE, ...)] stays labelled.
    names = stats::setNames(nm = models),

    # Full names, used for tab titles and the warning/error message tables.
    long = c(tko = tr("model.tko.long"),
             ete = tr("model.ete.long"),
             teq = tr("model.teq.long"),
             etp = tr("model.etp.long"),
             tpa = tr("model.tpa.long")),

    # Short names, used for table headers and the comparison grid, where the full ones
    # would not fit.
    abbrev = c(tko = tr("model.tko.abbrev"),
               ete = tr("model.ete.abbrev"),
               teq = tr("model.teq.abbrev"),
               etp = tr("model.etp.abbrev"),
               tpa = tr("model.tpa.abbrev")),

    # The fewest items each model can be tested with, meaning at least one degree of
    # freedom left over. One item fewer only just identifies it (df = 0, fits perfectly,
    # tests nothing); two fewer and lavaan fails. See GOTCHAS.md.
    minItems = stats::setNames(c(4, 3, 2, 2, 2), models),

    hierarchy = hierarchy,

    # Every pair that can be compared, worked out from the graph above rather than listed
    # by hand. See GOTCHAS.md.
    comparable = nestedPairs(models, hierarchy),

    # Everything the hierarchical comparison plot needs, one row per model.
    #   name             : the model's label, as an R plotmath expression (the plot draws
    #                      it with parse = TRUE, so it cannot be HTML). A ~ is a space
    #                      there, which a translation has to keep using.
    #   x, y             : where that label is printed
    #   xstarts, ystarts : where the line leaving that model begins
    #   xends, yends     : where it ends
    #   labelxs, labelys : where the chi-square label on that line is printed
    plot = data.frame(
      name = c(tr("model.tko.plot"),
               tr("model.ete.plot"),
               tr("model.teq.plot"),
               tr("model.etp.plot"),
               tr("model.tpa.plot")),
      x       = c(0, 0, -2, 2, 0),
      y       = c(6, 4, 2, 2, 0),
      xstarts = c(0, 0, 0, -2, 2),
      xends   = c(0, -2, 2, 0, 0),
      ystarts = c(5.8, 3.8, 3.8, 1.8, 1.8),
      yends   = c(4.2, 2.2, 2.2, 0.2, 0.2),
      labelxs = c(0, -2, 2, -2, 2),
      labelys = c(5, 3, 3, 1, 1),
      row.names = models))
}
