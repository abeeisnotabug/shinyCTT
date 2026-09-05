## Builds the table of checkboxes on the "4. Testing Parameters" tab, where the user picks
## which models to fit and which pairs of them to compare.
##
## The table is square, one row and one column per model:
##
##                  tko    ete    teq    etp    tpa
##          tko  [Include]
##          ete  [Compare] [Include]
##          teq  [Compare] [Compare] [Include]
##          etp  [Compare] [Compare]   n/a    [Include]
##          tpa  [Compare] [Compare] [Compare] [Compare] [Include]
##
## The box on the diagonal includes that model in the run. A box below the diagonal compares
## the row's model against the column's. Everything above the diagonal is left blank,
## because comparing A with B is the same test as comparing B with A.
##
## Arguments
##   family : the list built by cttModelFamily() in helpers-model-family.R. The caller passes it in;
##            this function reads $names, $abbrev, $minItems and $comparable from it.
##   ns     : only does something when this table is placed inside a Shiny module, which it
##            is not yet. With the default it leaves every id unchanged.
comparisonGrid <- function(family, ns = shiny::NS(NULL)) {

  models <- family$names

  # Every fluidRow() we build, in order: the header row first, then one row per model.
  gridRows <- list()

  ## ---- The header row: an empty corner cell, then one model name per column. ----------
  headerCells <- list(column(2))

  for (model in models) {
    modelName <- paste0("<b>", family$abbrev[[model]], "</b>")
    headerCells[[length(headerCells) + 1]] <- column(2, HTML(modelName))
  }

  gridRows[[1]] <- fluidRow(headerCells)

  ## ---- One row per model. --------------------------------------------------------------
  for (rowNumber in seq_along(models)) {

    rowModel <- models[[rowNumber]]

    # Each row starts with the model's name in the left-hand column.
    rowName <- paste0("<b>", family$abbrev[[rowModel]], "</b>")
    rowCells <- list(column(2, HTML(rowName)))

    for (columnNumber in seq_along(models)) {

      columnModel <- models[[columnNumber]]

      if (columnNumber > rowNumber) {

        ## ---- Above the diagonal: nothing. ----
        cell <- column(2)

      } else if (columnNumber == rowNumber) {

        ## ---- On the diagonal: include this model in the run? ----

        # The fewest items this model can be tested with, straight out of the family.
        fewestItems <- family$minItems[[rowModel]]

        # The strings below are JavaScript, which the browser evaluates to decide which
        # part of the cell to show. Reading them:
        #   output.nItemsChosen     how many item columns the user has ticked. A value the
        #                           server sends rather than an input, because the tick
        #                           boxes live inside the subset box and their id carries
        #                           that box's name (see GOTCHAS.md). Undefined before any
        #                           data is chosen, which is why every condition tests it
        #                           first - without that guard the browser throws roughly
        #                           18 errors per walkthrough.
        #   input.goModels          how often "Test the models" has been pressed, so 0
        #                           means the run has not happened yet.
        #   input.tko               whether the tau-congeneric checkbox is ticked, and so on
        #                           for each model.
        enoughItems <- paste0("output.nItemsChosen > ", fewestItems - 1)
        tooFewItems <- paste0("output.nItemsChosen <= ", fewestItems - 1)
        modelIsTicked <- paste0("input.", rowModel)

        # Exactly one of these four is visible at any moment.
        cell <- column(
          2,

          # Before the run, with enough items: let the user choose.
          conditionalPanel(
            paste0("output.nItemsChosen && ", enoughItems, " && input.goModels == 0"),
            checkboxInput(ns(rowModel), tr("params.grid.include"), value = TRUE),
            ns = ns),

          # Too few items for this model to have any degrees of freedom.
          conditionalPanel(
            paste0("output.nItemsChosen && ", tooFewItems),
            helpText(tr("params.grid.too.few")),
            ns = ns),

          # After the run: report what was done.
          conditionalPanel(
            paste0("input.goModels > 0 && ", modelIsTicked),
            helpText(tr("params.grid.tested")),
            ns = ns),

          conditionalPanel(
            paste0("output.nItemsChosen && input.goModels > 0 && !", modelIsTicked,
                   " && !(", tooFewItems, ")"),
            helpText(tr("params.grid.dont.test")),
            ns = ns))

      } else {

        ## ---- Below the diagonal: compare the row's model against the column's. ----

        # The checkbox id is the two model names stuck together, more constrained model
        # first: "etetko" is ess. tau-equivalent against tau-congeneric.
        comparisonId <- paste0(rowModel, columnModel)

        if (!comparisonId %in% family$comparable) {

          # These two models are not nested in each other, so a likelihood-ratio test
          # between them would be meaningless. In the CTT family this happens exactly once,
          # for tau-equivalent against essentially tau-parallel.
          cell <- column(2, helpText(tr("params.grid.not.testable")))

        } else {

          # A comparison can only be run if *both* its models can be fitted, so it needs
          # whichever of the two wants more items.
          fewestItems <- max(family$minItems[[rowModel]], family$minItems[[columnModel]])

          enoughItems <- paste0("output.nItemsChosen > ", fewestItems - 1)
          tooFewItems <- paste0("output.nItemsChosen <= ", fewestItems - 1)
          bothModelsTicked <- paste0("input.", columnModel, " && input.", rowModel)
          comparisonIsTicked <- paste0("input.", comparisonId)

          cell <- column(
            2,

            # Before the run, with enough items and both models included: offer the box.
            conditionalPanel(
              paste0("output.nItemsChosen && ", bothModelsTicked, " && ", enoughItems,
                     " && input.goModels == 0"),
              checkboxInput(ns(comparisonId), tr("params.grid.compare"), value = TRUE),
              ns = ns),

            # Not available: one of the two models is not included, or there are too few
            # items, or the run has happened and this comparison was not ticked.
            conditionalPanel(
              paste0("output.nItemsChosen && (!(", bothModelsTicked, ") || ", tooFewItems,
                     " || (input.goModels > 0 && !", comparisonIsTicked, "))"),
              helpText(tr("params.grid.dont.test")),
              ns = ns),

            conditionalPanel(
              paste0("input.goModels > 0 && ", comparisonIsTicked),
              helpText(tr("params.grid.tested")),
              ns = ns))
        }
      }

      rowCells[[length(rowCells) + 1]] <- cell
    }

    gridRows[[length(gridRows) + 1]] <- fluidRow(rowCells)
  }

  tagList(gridRows)
}
