## Everything the app builds a table out of: how a number is formatted, how a rating is
## turned into a header or a divider, and the tables themselves. The colours those ratings
## stand for are in R/helpers-colors.R.

## The words DT puts around a table of its own - the search box, the row count, the two
## paging buttons. DT ships them in English only, so they are handed to it as text like
## everything else the user reads.
##
## The _MENU_, _START_, _END_, _TOTAL_ and _MAX_ are DataTables' own placeholders: it
## fills the numbers in. A translation has to keep them.
dtLanguage <- function() {
  list(
    search = tr("dt.search"),
    lengthMenu = tr("dt.length.menu"),
    info = tr("dt.info"),
    infoEmpty = tr("dt.info.empty"),
    infoFiltered = tr("dt.info.filtered"),
    zeroRecords = tr("dt.zero.records"),
    emptyTable = tr("dt.empty.table"),
    paginate = list(previous = tr("dt.paginate.previous"),
                    `next` = tr("dt.paginate.next")))
}

## The heading above one group's table. Bold, because the table's own row labels are, and
## with far more space above it than below so it reads as belonging to the table under it
## rather than to the one before.
groupHeading <- function(text) {
  tags$p(
    HTML(text),
    style = paste("font-weight: bold; font-size: 15px;",
                  "margin: 28px 0 6px 0; color: #444;"))
}

## CFI is rated the same way in the fit index table and in the hierarchical table, so the
## rule is written once: Schermelleh-Engel & Moosbrugger, >= .97 good, >= .95 acceptable,
## anything less bad.
rateCFI <- function(cfi) {
  ifelse(cfi >= 0.97, "good", ifelse(cfi >= 0.95, "neutral", "bad"))
}

## Format a fit index that is bounded to [0, 1] (p-values, RMSEA, CFI, SRMR, ...) the
## conventional way: fixed decimal places, no leading zero (".048", not "0.048").
formatBounded <- function(x, digits = 3) {
  sub("^(-?)0\\.", "\\1.", sprintf(paste0("%.", digits, "f"), x))
}

## The descriptives table: one row per item, the four numbers itemMoments() worked out. How
## many rows went into it is said in the heading above it, the same way every other table on
## that page says it.
##
## locales = "en-US" pins the decimal point - without it reactable rounds in the reader's
## own language and a German browser prints 1,504 (see GOTCHAS.md).
momentsTable <- function(moments) {
  reactable::reactable(
    as.data.frame(moments),
    rownames = TRUE,
    defaultColDef = reactable::colDef(
      # Wide enough for the longest of the four headers in any language, "Mittelwert".
      minWidth = 84,
      format = reactable::colFormat(digits = 3, locales = "en-US")),
    columns = list(
      .rownames = reactable::colDef(name = "", style = list(fontWeight = "bold")),
      Mean = reactable::colDef(name = tr("stats.desc.mean")),
      SD = reactable::colDef(name = tr("stats.desc.sd")),
      Skew = reactable::colDef(name = tr("stats.desc.skew")),
      Excess = reactable::colDef(name = tr("stats.desc.excess"))),
    resizable = getOption("shinyCTT.resizable"),
    sortable = FALSE,
    pagination = FALSE,
    compact = TRUE)
}


## The covariance matrix of whichever rows are handed in, as a table. Only the lower
## triangle is filled: the upper one repeats it, so it is blanked and shown empty.
##
## locales = "en-US" pins the decimal point - without it reactable rounds in the reader's
## own language and a German browser prints 0,129 (see GOTCHAS.md).
covarianceTable <- function(rows) {
  covariances <- stats::cov(rows, use = "pairwise.complete.obs")
  covariances[upper.tri(covariances)] <- NA

  reactable::reactable(
    as.data.frame(covariances),
    rownames = TRUE,
    defaultColDef = reactable::colDef(
      # Wide enough for a covariance at three decimal places, and for an item name of
      # ordinary length above it. One number for every column, because a matrix has as
      # many of them as there are items.
      minWidth = 66,
      na = "",
      format = reactable::colFormat(digits = 3, locales = "en-US")),
    columns = list(
      .rownames = reactable::colDef(name = "", style = list(fontWeight = "bold"))),
    resizable = getOption("shinyCTT.resizable"),
    sortable = FALSE,
    pagination = FALSE,
    compact = TRUE)
}


## The correlation table: every pair's correlation with its confidence interval on the row
## underneath it, lower triangle only - the upper one repeats it.
##
## Gives back two things of the same shape: `shown`, the text of every cell, and `ratings`,
## how each cell is rated. A significant positive correlation is "good", a significant
## negative one "bad", anything not significant "neutral"; the diagonal and the upper
## triangle are rated NA and stay unpainted. The colours are not here - ratingStyle() has
## them - so this function can be tested by reading the ratings.
makeCorrTableWithCIs <- function(rawTable, sigLvl, itemCols) {

  correlations <- rawTable$cor
  tests <- rawTable$test

  lower <- lower.tri(correlations)

  ## one rating per pair ----
  pairRating <- matrix(NA_character_, nrow(correlations), ncol(correlations))
  pairRating[lower] <- ifelse(
    tests$p[lower] < sigLvl,
    ifelse(correlations[lower] >= 0, "good", "bad"),
    "neutral")

  ## the two kinds of row ----
  correlationRows <- ciRows <- matrix("", nrow(correlations), ncol(correlations))

  correlationRows[lower] <- sprintf("%.3f", correlations[lower])
  diag(correlationRows) <- "1"

  ciRows[lower] <- sprintf("[%.3f, %.3f]", tests$lowCI[lower], tests$uppCI[lower])
  diag(ciRows) <- "-"

  ## interleaved, a correlation row then its interval row ----
  shown <- matrix("", 2 * nrow(correlations), ncol(correlations))
  ratings <- matrix(NA_character_, 2 * nrow(correlations), ncol(correlations))

  correlationRowNumbers <- seq(1, nrow(shown), by = 2)
  ciRowNumbers <- seq(2, nrow(shown), by = 2)

  shown[correlationRowNumbers, ] <- correlationRows
  shown[ciRowNumbers, ] <- ciRows

  # Both rows of a pair carry the pair's rating, so the interval is painted with its
  # correlation.
  ratings[correlationRowNumbers, ] <- pairRating
  ratings[ciRowNumbers, ] <- pairRating

  colnames(shown) <- colnames(ratings) <- itemCols

  # The row labels are a column of their own rather than row names: every second one is the
  # word CI, and as.data.frame() would make those unique - CI, CI.1, CI.2 - on the screen.
  shown <- data.frame(
    rowLabel = c(rbind(itemCols, tr("results.col.ci"))),
    shown,
    stringsAsFactors = FALSE,
    check.names = FALSE)

  list(shown = shown, ratings = ratings)
}

## Draws what makeCorrTableWithCIs() gave back. Its first column holds the item names and
## the word CI, and gets no header of its own.
drawCorrTable <- function(corrTable, itemCols) {
  itemColumns <- lapply(itemCols, function(item) {
    reactable::colDef(
      style = function(value, index) ratingStyle(corrTable$ratings[index, item]))
  })

  reactable::reactable(
    corrTable$shown,
    columns = c(
      list(rowLabel = reactable::colDef(name = "", style = list(fontWeight = "bold"))),
      stats::setNames(itemColumns, itemCols)),
    resizable = getOption("shinyCTT.resizable"),
    sortable = FALSE,
    pagination = FALSE,
    compact = TRUE)
}

## The hierarchical model comparison table: each model against the one above it in the
## nesting order, plus the fit indices that rate each model on its own.
##
## Every coloured column gets a rating vector alongside it - "good", "bad" or "neutral",
## one per row - and reactable paints from that. The first row is the baseline model, which
## has nothing above it to be compared against, so its four difference columns are empty and
## rated NA.
makeHierTable <- function(succTable, CFIs, estimatorName, sigLvl, modelsAbbrev) {
  hierTable <- succTable
  hierTable$CFI <- CFIs

  ## lavTestLRT() omits the RMSEA-of-the-difference column entirely whenever any compared
  ## model was fitted with missing = "fiml" - regardless of whether the data actually has
  ## missing values. Fill it with NA so the column selection below doesn't error out; it then
  ## renders as an explicit "NA" in a neutral cell.
  if (!"RMSEA" %in% names(hierTable)) hierTable$RMSEA <- NA_real_

  hierTable <- hierTable[, c("Df diff", "Chisq diff", "Pr(>Chisq)", "RMSEA", "CFI", "AIC", "BIC")]

  firstRow <- 1
  laterRows <- -1

  ## the three difference columns ----
  # All three say the same thing about the comparison, so they share one rating: the test
  # came out significant, or it did not.
  differenceRating <- c(
    NA_character_,
    ifelse(hierTable[laterRows, "Pr(>Chisq)"] < sigLvl, "bad", "good"))

  deltaDf <- c("", sprintf("+%i", hierTable[laterRows, "Df diff"]))
  deltaChisq <- c("", sprintf("+%.2f", hierTable[laterRows, "Chisq diff"]))
  pValue <- c("", formatBounded(hierTable[laterRows, "Pr(>Chisq)"]))

  ## RMSEA of the difference ----
  rmseaD <- hierTable[laterRows, "RMSEA"]

  rmseaDShown <- c("", ifelse(is.na(rmseaD), tr("results.na"), formatBounded(rmseaD)))
  rmseaDRating <- c(
    NA_character_,
    ifelse(is.na(rmseaD), "neutral", ifelse(rmseaD < 0.05, "good", "bad")))

  ## CFI, AIC and BIC ----
  # These rate each model on its own, the same way the fit index table does, so unlike the
  # columns above they cover every row including the baseline.
  cfiShown <- formatBounded(hierTable$CFI)
  cfiRating <- rateCFI(hierTable$CFI)

  # Compare the rounded (i.e. displayed) values, not the raw ones - otherwise two AICs that
  # display identically at one decimal place could still disagree on which is "the minimum".
  aicRounded <- round(as.numeric(hierTable$AIC), 1)
  bicRounded <- round(as.numeric(hierTable$BIC), 1)

  shown <- data.frame(
    deltaDf = deltaDf,
    deltaChisq = deltaChisq,
    p = pValue,
    rmseaD = rmseaDShown,
    cfi = cfiShown,
    aic = sprintf("%.1f", aicRounded),
    bic = sprintf("%.1f", bicRounded),
    row.names = modelsAbbrev[rownames(hierTable)],
    check.names = FALSE)

  ratings <- list(
    deltaDf = differenceRating,
    deltaChisq = differenceRating,
    p = differenceRating,
    rmseaD = rmseaDRating,
    cfi = cfiRating,
    aic = ifelse(aicRounded == min(aicRounded), "good", "bad"),
    bic = ifelse(bicRounded == min(bicRounded), "good", "bad"))

  headers <- c(
    deltaDf = tr("sym.delta.df"),
    deltaChisq = paste0(estimatorName, tr("sym.delta.chi2")),
    p = tr("common.col.p"),
    rmseaD = tr("sym.rmsea.d"),
    cfi = tr("results.col.cfi"),
    aic = tr("results.col.aic"),
    bic = tr("results.col.bic"))

  # How narrow each column may be, in pixels: enough for the widest thing it can hold, at
  # the 14px Arial the tables are drawn in. Without these every column is reactable's own
  # 100px, and eight of those do not fit beside a second copy of the table (see GOTCHAS.md).
  # The header is the widest thing in the first four; the number is, in the last three.
  minWidths <- c(
    deltaDf = 46,      # three digits and the plus sign
    deltaChisq = 85,   # the header at its longest estimator name, FIML with robust errors
    p = 42,
    rmseaD = 76,
    cfi = 50,
    aic = 62,          # seven characters: 12345.6, or -1234.5
    bic = 62)

  reactable::reactable(
    shown,
    rownames = TRUE,
    columns = c(
      list(.rownames = reactable::colDef(
        name = "", html = TRUE, style = list(fontWeight = "bold"))),
      ratedColumns(headers, ratings, minWidths)),

    # The baseline model's own row was greyed by kableExtra::row_spec(); its cells carry no
    # rating, so the shading goes on the row itself.
    rowStyle = function(index) if (index == firstRow) list(background = "lightgrey"),
    resizable = getOption("shinyCTT.resizable"),
    sortable = FALSE,
    pagination = FALSE,
    compact = TRUE)
}

## One reactable column per rated column: `headers` gives each its title, `ratings` the
## rating of every row in it, `minWidths` how narrow it may be. All three are keyed by the
## column's name in the data. NULL[["x"]] is NULL, so leaving minWidths out leaves every
## column at reactable's own 100px.
ratedColumns <- function(headers, ratings, minWidths = NULL) {
  columns <- lapply(names(headers), function(column) {
    reactable::colDef(
      name = headers[[column]],
      html = TRUE,
      minWidth = minWidths[[column]],
      style = function(value, index) ratingStyle(ratings[[column]][index]))
  })

  stats::setNames(columns, names(headers))
}

## The fit index table: one row per model, every index rated against its own reference
## values rather than against the other models.
makeFitsTable <- function(fits, estimatorName, sigLvl, rmseaCiLvl, modelsAbbrev) {

  # df, chi-square and its p-value all say the same thing about the model's fit, so they
  # share one rating.
  chisqRating <- ifelse(fits$pvalue < sigLvl, "bad", "good")

  shown <- data.frame(
    df = sprintf("%i", fits$df),
    chisq = sprintf("%.2f", fits$chisq),
    pvalue = formatBounded(fits$pvalue),
    rmsea = formatBounded(fits$rmsea),
    rmseaCi = sprintf("[%s, %s]",
                      formatBounded(fits$rmsea.ci.lower), formatBounded(fits$rmsea.ci.upper)),
    rmseaP = formatBounded(fits$rmsea.pvalue),
    rmseaNotClose = formatBounded(fits$rmsea.notclose.pvalue),
    cfi = formatBounded(fits$cfi),
    srmr = formatBounded(fits$srmr),
    row.names = modelsAbbrev[rownames(fits)],
    check.names = FALSE)

  ratings <- list(
    df = chisqRating,
    chisq = chisqRating,
    pvalue = chisqRating,
    rmsea = ifelse(fits$rmsea < 0.05, "good", "bad"),

    # The interval is good when it lies below .05, neutral when it straddles it, bad when
    # it lies above.
    rmseaCi = ifelse(
      fits$rmsea.ci.upper < 0.05, "good",
      ifelse(fits$rmsea.ci.lower < 0.05, "neutral", "bad")),

    rmseaP = ifelse(fits$rmsea.pvalue < sigLvl, "bad", "good"),
    rmseaNotClose = ifelse(fits$rmsea.notclose.pvalue < sigLvl, "good", "bad"),
    cfi = rateCFI(fits$cfi),
    srmr = ifelse(fits$srmr < 0.05, "good", "bad"))

  headers <- c(
    df = tr("results.col.df"),
    chisq = paste0(estimatorName, tr("sym.chi2")),
    pvalue = tr("common.col.p"),
    rmsea = tr("results.col.rmsea"),
    rmseaCi = sprintf(tr("results.col.rmsea.ci"), 100 * rmseaCiLvl),
    rmseaP = tr("sym.p.rmsea.le05"),
    rmseaNotClose = tr("sym.p.rmsea.ge08"),
    cfi = tr("results.col.cfi"),
    srmr = tr("results.col.srmr"))

  # How narrow each column may be, in pixels: enough for the widest thing it can hold, at
  # the 14px Arial the tables are drawn in (see GOTCHAS.md). The header is the widest thing
  # in every column here, and the two RMSEA p-values have the longest headers in the app.
  minWidths <- c(
    df = 40,
    chisq = 76,        # the header at its longest estimator name, FIML with robust errors
    pvalue = 45,
    rmsea = 66,
    rmseaCi = 86,
    rmseaP = 133,
    rmseaNotClose = 133,
    cfi = 52,
    srmr = 58)

  columns <- ratedColumns(headers, ratings, minWidths)

  # The two dividing lines: one closing the chi-square block, one closing the RMSEA block.
  columns$pvalue$style <- rightBorderAfter(ratings$pvalue)
  columns$rmseaNotClose$style <- rightBorderAfter(ratings$rmseaNotClose)

  reactable::reactable(
    shown,
    rownames = TRUE,
    columns = c(
      list(.rownames = reactable::colDef(
        name = "", html = TRUE, style = list(fontWeight = "bold"))),
      columns),
    resizable = getOption("shinyCTT.resizable"),
    sortable = FALSE,
    pagination = FALSE,
    compact = TRUE)
}

## Draws one of the three pairwise model comparison tables. `cells` is what compMatrices()
## gave back for one of them; `headers` names each column and `minWidths` says how narrow it
## may be; `groups`, when given, is the band above them - the combined table puts two columns
## under each model's name - and `dividers` names the columns that carry a line down their
## right edge.
drawCompTable <- function(cells, headers, minWidths, groups = NULL, dividers = NULL) {

  columns <- lapply(names(headers), function(column) {
    reactable::colDef(
      name = headers[[column]],
      html = TRUE,
      minWidth = minWidths[[column]],
      style = function(value, index) {
        c(ratingStyle(cells$ratings[index, column]),
          if (cells$ownFit[index, column]) list(fontStyle = "italic"),
          if (column %in% dividers) list(borderRight = "1px solid lightgrey"))
      })
  })

  reactable::reactable(
    as.data.frame(cells$shown, stringsAsFactors = FALSE),
    rownames = TRUE,
    columns = c(
      list(.rownames = reactable::colDef(
        name = "", html = TRUE, style = list(fontWeight = "bold"))),
      stats::setNames(columns, names(headers))),
    columnGroups = groups,
    resizable = getOption("shinyCTT.resizable"),
    sortable = FALSE,
    pagination = FALSE,
    compact = TRUE)
}


## A rated column that also closes a group of columns, so it carries a line down its right
## edge.
rightBorderAfter <- function(rating) {
  function(value, index) {
    c(ratingStyle(rating[index]), list(borderRight = "1px solid lightgrey"))
  }
}

## The estimated parameters of one model, for one group: four blocks of columns - the
## factor loadings, the intercepts, the error variances and the item reliabilities - each
## with an estimate, a standard error and a confidence interval. Nothing here is rated, so
## no colours.
##
## A single-group fit is group 1 and nothing else, so the caller always names a group and
## this function never has to know how many there are.
makeParTableWithCIs <- function(fitObject, estimatorName, sigLvl, itemCols, group = 1) {

  # extractParameters() stacks the groups, each of them one row per item plus a row for the
  # group's own variance and reliability.
  rowsPerGroup <- length(itemCols) + 1

  parameters <- extractParameters(fitObject, alpha = sigLvl)
  parameters <- parameters[(group - 1) * rowsPerGroup + seq_len(rowsPerGroup), ]

  # extractParameters() repeats label/est/se/CI once per block, so its data frame has the
  # same name on several columns. reactable keys its columns by name, so name them for what
  # they hold; the headers on screen come from displayNames below.
  names(parameters) <- c(
    "item",
    "lambdaLabel", "lambdaEst", "lambdaSe", "lambdaCi",
    "stdEst", "stdSe", "stdCi",
    "alphaLabel", "alphaEst", "alphaSe", "alphaCi",
    "errorLabel", "errorEst", "errorSe", "errorCi",
    "relLabel", "relEst", "relSe", "relCi")

  # "SE" and "CI" carry the estimator's name as a subscript, e.g. SE<sub>MLR</sub>.
  seName <- paste0(tr("results.col.se"), "<sub>", estimatorName, "</sub>")
  ciName <- paste0(tr("results.col.ci"), "<sub>", estimatorName, "</sub>")

  displayNames <- c(
    item = tr("common.col.item"),
    lambdaLabel = tr("sym.lambda.i"),
    lambdaEst = tr("results.col.est"), lambdaSe = seName, lambdaCi = ciName,
    stdEst = tr("results.col.std.est"), stdSe = seName, stdCi = ciName,
    alphaLabel = tr("sym.alpha.i"),
    alphaEst = tr("results.col.est"), alphaSe = seName, alphaCi = ciName,
    errorLabel = tr("sym.sigma2.epsilon.i"),
    errorEst = tr("results.col.est"), errorSe = seName, errorCi = ciName,
    relLabel = tr("sym.reliability.i"),
    relEst = tr("results.col.est"), relSe = seName, relCi = ciName)

  # The estimates and standard errors are real numbers - makeKable() used to round them to
  # three places for the whole table. Everything else is already text.
  roundedColumns <- c("lambdaEst", "lambdaSe", "stdEst", "stdSe", "alphaEst", "alphaSe",
                      "errorEst", "errorSe", "relEst", "relSe")

  # How narrow each column may be, in pixels. The four blocks have the same shape, so the
  # widths repeat: a symbol, an estimate, a standard error, an interval. Sized for the
  # longest the header gets in any of the three languages - "Std. Schaetzer" in German is
  # the widest one in the table, and an interval is the widest cell.
  minWidths <- c(
    item = 60,
    lambdaLabel = 45, lambdaEst = 74, lambdaSe = 77, lambdaCi = 103,
    stdEst = 104, stdSe = 77, stdCi = 103,
    alphaLabel = 45, alphaEst = 74, alphaSe = 77, alphaCi = 103,
    errorLabel = 45, errorEst = 74, errorSe = 77, errorCi = 103,
    relLabel = 45, relEst = 74, relSe = 77, relCi = 103)

  columns <- lapply(names(displayNames), function(column) {
    reactable::colDef(
      name = displayNames[[column]],
      html = TRUE,
      minWidth = minWidths[[column]],
      format = if (column %in% roundedColumns)
        reactable::colFormat(digits = 3, locales = "en-US"))
  })

  reactable::reactable(
    parameters,
    columns = stats::setNames(columns, names(displayNames)),

    columnGroups = list(
      reactable::colGroup(
        name = tr("results.partable.loadings"),
        columns = c("lambdaLabel", "lambdaEst", "lambdaSe", "lambdaCi",
                    "stdEst", "stdSe", "stdCi")),
      reactable::colGroup(
        name = tr("results.partable.intercepts"),
        columns = c("alphaLabel", "alphaEst", "alphaSe", "alphaCi")),
      reactable::colGroup(
        name = tr("results.partable.variances"),
        columns = c("errorLabel", "errorEst", "errorSe", "errorCi")),
      reactable::colGroup(
        name = tr("results.partable.reliabilities"),
        columns = c("relLabel", "relEst", "relSe", "relCi"))),

    # The last row is the group's own variance and reliability rather than an item's.
    rowStyle = function(index) if (index == rowsPerGroup) list(fontWeight = "bold"),
    resizable = getOption("shinyCTT.resizable"),
    sortable = FALSE,
    pagination = FALSE,
    compact = TRUE)
}

## One cell of a legend: a piece of text, and how it is rated. A rating of NA leaves it
## unpainted, which is what the labels between the coloured chips are.
chip <- function(text, rating = NA) list(text = text, rating = rating)

## Draws a legend as the small condensed table it has always been. Each argument is one row
## of chips.
drawLegend <- function(...) {
  rows <- list(...)

  tags$table(
    class = "table table-condensed",
    style = "width: auto; margin-bottom: 0;",

    tags$tbody(lapply(rows, function(row) {
      tags$tr(lapply(row, function(cell) {
        painted <- ratingStyle(cell$rating)

        tags$td(
          HTML(cell$text),
          style = if (is.null(painted))
            "font-weight: bold;"
          else
            sprintf("background-color: %s; color: %s;", painted$background, painted$color))
      }))
    })))
}

## Builds the legend under one of the tables.
##
##   rmseaCiLvl : the confidence level of the RMSEA interval. Only the fit index legend
##                shows that interval, so only that call passes it.
makeLegend <- function(whichLegend, estimatorName, sigLvl, rmseaCiLvl = 0.90) {

  # The two thresholds every legend below quotes, written once.
  atLeastSig <- paste0(tr("results.legend.p.ge"), formatBounded(sigLvl))
  belowSig <- paste0(tr("results.legend.p.lt"), formatBounded(sigLvl))

  # "Delta-df, ML-Delta-chi-squared, p:" - the three columns those chips describe.
  differenceColumns <- paste(
    c(tr("sym.delta.df"), paste0(estimatorName, tr("sym.delta.chi2")),
      paste0(tr("common.col.p"), ":")),
    collapse = ", ")

  switch(
    whichLegend,

    "corrTable" = drawLegend(list(
      chip(tr("common.legend")),
      chip(tr("common.sig.pos"), "good"),
      chip(tr("common.sig.neg"), "bad"),
      chip(tr("common.not.sig"), "neutral"))),

    "hierTables" = drawLegend(list(
      chip(tr("common.legend")),
      chip(differenceColumns),
      chip(atLeastSig, "good"),
      chip(belowSig, "bad"),

      chip(tr("sym.rmsea.d")),
      chip(tr("results.legend.lt.05"), "good"),
      chip(tr("results.legend.ge.05"), "bad"),
      chip(tr("results.legend.na.fiml"), "neutral"),

      chip(paste0(tr("results.col.cfi"), ":")),
      chip(tr("results.legend.ge.97"), "good"),
      chip(tr("results.legend.ge.95"), "neutral"),
      chip(tr("results.legend.lt.95"), "bad"),

      chip(tr("results.legend.aicbic.label")),
      chip(tr("results.legend.min"), "good"),
      chip(tr("results.legend.else"), "bad"))),

    "fitIndexTable" = drawLegend(
      list(
        chip(tr("common.legend")),
        chip(differenceColumns),
        chip(atLeastSig, "good"),
        chip(belowSig, "bad"),

        chip(""), chip(""), chip(""), chip(""),

        chip(tr("results.col.cfi")),
        chip(tr("results.legend.ge.97"), "good"),
        chip(tr("results.legend.ge.95"), "neutral"),
        chip(tr("results.legend.lt.95"), "bad"),

        chip(tr("results.col.srmr")),
        chip(tr("results.legend.lt.05"), "good"),
        chip(tr("results.legend.ge.05"), "bad")),

      list(
        chip(""),
        chip(tr("results.col.rmsea")),
        chip(tr("results.legend.lt.05"), "good"),
        chip(tr("results.legend.ge.05"), "bad"),

        chip(sprintf(tr("results.col.rmsea.ci"), 100 * rmseaCiLvl)),
        chip(tr("results.legend.lt.05"), "good"),
        chip(tr("results.legend.gt.05"), "bad"),
        chip(tr("sym.contains.05"), "neutral"),

        chip(tr("sym.p.05")),
        chip(paste0(tr("results.legend.ge"), formatBounded(sigLvl)), "good"),
        chip(paste0(tr("results.legend.lt"), formatBounded(sigLvl)), "bad"),

        chip(tr("sym.p.08")),
        chip(paste0(tr("results.legend.lt"), formatBounded(sigLvl)), "good"),
        chip(paste0(tr("results.legend.ge"), formatBounded(sigLvl)), "bad"),
        chip(""))),

    "combCompTable" = drawLegend(list(
      chip(tr("common.legend")),
      chip(paste0(tr("sym.delta.df"), ", ", estimatorName, tr("sym.delta.chi2"), ":")),
      chip(atLeastSig, "good"),
      chip(belowSig, "bad"),
      chip(tr("results.legend.stars"), "neutral"))),

    "infCompTable" = drawLegend(list(
      chip(tr("common.legend")),
      chip(tr("results.col.aicbic")),
      chip(tr("results.legend.lt.0"), "good"),
      chip(tr("results.legend.gt.0"), "bad"),
      chip(tr("results.legend.eq.0"), "neutral"))),

    stop(sprintf("No legend available for table %s.", whichLegend)))
}

## What lavaan said about a model, for the orange and the red box above the results: one row
## per model, its name and the message.
messageTable <- function(modelNames, messages) {
  tags$table(
    class = "table table-condensed",
    style = "width: auto;",

    tags$tbody(Map(function(modelName, message) {
      tags$tr(
        tags$td(HTML(paste0(modelName, ":&emsp;")), style = "font-weight: bold;"),
        tags$td(message))
    }, modelNames, messages)))
}

