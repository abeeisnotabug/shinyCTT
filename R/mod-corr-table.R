## The "Correlation table with confidence intervals:" box on the Correlations tab.
##
## Two UI entry points sharing one module id: corrTableControlsUI() places the two
## controls in one column, corrTableUI() places the table itself underneath.

## The p-value and the confidence interval of every pair of items, as three matrices laid
## out the same way as the correlation matrix they are shown beside.
##
##   items     : the item columns, as a data frame
##   use       : how to treat missing values -> the same string stats::cor() is given,
##               "pairwise.complete.obs" or "complete.obs"
##   confLevel : the confidence level of the intervals, e.g. 0.95
##
## Returns list(p =, lowCI =, uppCI =), the three matrices makeCorrTableWithCIs() reads.
corTestMatrices <- function(items, use, confLevel) {

  # "complete.obs" -> throw the incomplete rows away once, up front, so every pair is
  # tested on the same rows the correlations were computed from. cor.test() drops the
  # incomplete pairs by itself, which is what "pairwise.complete.obs" means.
  if (identical(use, "complete.obs")) items <- items[stats::complete.cases(items), ]

  nItems <- ncol(items)

  # Filled in pair by pair below. An item with itself sits on the diagonal: p = 0, and an
  # interval of zero width at 1.
  pMat <- lowMat <- uppMat <- matrix(
    NA_real_, nItems, nItems, dimnames = list(colnames(items), colnames(items)))

  diag(pMat) <- 0
  diag(lowMat) <- diag(uppMat) <- 1

  for (i in seq_len(nItems - 1)) {
    for (j in (i + 1):nItems) {

      thisTest <- stats::cor.test(items[, i], items[, j], conf.level = confLevel)

      pMat[i, j] <- pMat[j, i] <- thisTest$p.value

      # cor.test() reports no interval on fewer than four complete pairs.
      if (!is.null(thisTest$conf.int)) {
        lowMat[i, j] <- lowMat[j, i] <- thisTest$conf.int[1]
        uppMat[i, j] <- uppMat[j, i] <- thisTest$conf.int[2]
      }
    }
  }

  list(p = pMat, lowCI = lowMat, uppCI = uppMat)
}

corrTableControlsUI <- function(id) {
  ns <- NS(id)

  shinydashboard::box(
    width = NULL,
    title = tr("stats.corrtable.title"),
    shinyjs::hidden(
      radioButtons(
        ns("corrTabNA"),
        tr("stats.corrtable.missing.label"),
        choiceNames = list(
          tr("stats.corrtable.missing.pairwise"),
          tr("stats.corrtable.missing.complete")),
        choiceValues = c("pairwise.complete.obs", "complete.obs"),
        selected = "pairwise.complete.obs")),
    numericInput(
      ns("corrTabSL"),
      tr("stats.corrtable.siglvl.label"),
      value = 0.05,
      min = 0.001,
      max = 1,
      step = 0.001))
}

corrTableUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("box"))
}

## Arguments, all reactives:
##   data          : the chosen items, and the group column if there is one
##   itemCols      : the names of the item columns
##   groupCol      : the name of the group column
##   hasGroups     : TRUE when the group column is usable
##   estimatorName : the estimator label shown in the legend, e.g. "ML" or "FIML"
##   sigLvl        : the significance level from the Statistics tab, used only in the legend
##   useFIML       : TRUE when the fit should use full information maximum likelihood
corrTableServer <- function(id, data, itemCols, groupCol, hasGroups, estimatorName, sigLvl,
                            useFIML) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## the missing-value choice only matters when there are missing values ----
    # shinyjs puts the module's name in front of the id itself (see GOTCHAS.md), so this
    # is the plain id, not ns("corrTabNA").
    observeEvent(useFIML(), {
      if (useFIML()) shinyjs::show(id = "corrTabNA")
    })

    ## the box ----
    output$box <- renderUI({
      req(data())

      ## box create raw cor table and test for errors ----
      corrTableWithCIsRaw <- list(
        cor = tryCatch(
          stats::cor(data()[, itemCols()],
              use = input$corrTabNA),
          warning = function(w) NULL,
          error = function(e) NULL),
        test = tryCatch(
          corTestMatrices(data()[, itemCols()],
                          use = input$corrTabNA,
                          confLevel = (1 - input$corrTabSL)),
          warning = function(w) w,
          error = function(e) e))

      ## box the overall table ----
      # corTestMatrices() gives back a list when it worked and a condition when it did not.
      if (identical(class(corrTableWithCIsRaw$test)[1], "list")) {

        singleCorrTable <- drawCorrTable(
          makeCorrTableWithCIs(
            rawTable = corrTableWithCIsRaw,
            sigLvl = input$corrTabSL,
            itemCols = itemCols()),
          itemCols())

      } else {
        singleCorrTable <-
          paste(tr("stats.error.prefix"), corrTableWithCIsRaw$test) %>%
          HTML() %>%
          div(style = "color:red")
      }

      ## box if groups ----
      if (hasGroups()) {

        groups <- unique(data()[, groupCol()])

        # One table per group, each under its own heading. kableExtra put that heading in
        # a dark band row inside one tall table; reactable cannot draw a row that is not in
        # the data.
        mgCorrTables <- lapply(groups, function(group) {
          groupRows <- data()[, groupCol()] == group

          tagList(
            groupHeading(sprintf(tr("common.group.label"), group, sum(groupRows))),
            drawCorrTable(
              makeCorrTableWithCIs(
                rawTable = list(
                  cor = suppressWarnings(stats::cor(
                    subset(data()[, itemCols()], groupRows),
                    use = input$corrTabNA)),
                  test = corTestMatrices(
                    subset(data()[, itemCols()], groupRows),
                    use = input$corrTabNA,
                    confLevel = (1 - input$corrTabSL))),
                sigLvl = input$corrTabSL,
                itemCols = itemCols()),
              itemCols()))
        })

        # assemble in tabBox
        shinydashboard::tabBox(
          width = 12,
          title = tr("stats.corrtable.title"),
          side = "right",

          tabPanel(
              tr("common.overall"),
              groupHeading(sprintf(tr("common.overall.n"), nrow(data()))),
              singleCorrTable,
              br(),
              makeLegend("corrTable", estimatorName(), sigLvl())),
          tabPanel(
              tr("common.groupwise"),
              unname(mgCorrTables),
              br(),
              makeLegend("corrTable", estimatorName(), sigLvl()))

        ) # tabBox

      } ## box if no groups ----
      else {

        shinydashboard::box(
            width = 12,
            title = tr("stats.corrtable.title"),

            groupHeading(sprintf(tr("common.overall.n"), nrow(data()))),
            singleCorrTable,
            br(),
            makeLegend("corrTable", estimatorName(), sigLvl()))
      }
    })
  })
}
