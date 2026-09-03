## The three colours a table cell can be painted, and the text colour that goes on top of
## them. Written down here once. The tables below no longer take them as arguments: each
## works out a rating - "good", "bad" or "neutral" - and ratingStyle() turns that into a
## colour. The hierarchy plot in mod-ctt-results.R reads them from here too.
cttColors <- function() {
  list(good = "darkgreen", bad = "darkred", neutral = "grey", text = "white")
}

## What a rated cell looks like. Handed to a column's style, which reactable calls once per
## row. NA -> no colour at all, for a cell with nothing to rate (the empty upper triangle of
## the correlation table, the first row of the hierarchical table).
ratingStyle <- function(rating) {
  if (length(rating) != 1 || is.na(rating)) return(NULL)

  list(background = cttColors()[[rating]], color = cttColors()$text)
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
    rowLabel = c(rbind(itemCols, tr("CI"))),
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

  rmseaDShown <- c("", ifelse(is.na(rmseaD), tr("NA"), formatBounded(rmseaD)))
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
    deltaDf = tr("&Delta;df"),
    deltaChisq = paste0(estimatorName, tr("-&Delta;&chi;&sup2;")),
    p = tr("p"),
    rmseaD = tr("RMSEA<sub>D</sub>"),
    cfi = tr("CFI"),
    aic = tr("AIC"),
    bic = tr("BIC"))

  reactable::reactable(
    shown,
    rownames = TRUE,
    columns = c(
      list(.rownames = reactable::colDef(
        name = "", html = TRUE, style = list(fontWeight = "bold"))),
      ratedColumns(headers, ratings)),

    # The baseline model's own row was greyed by kableExtra::row_spec(); its cells carry no
    # rating, so the shading goes on the row itself.
    rowStyle = function(index) if (index == firstRow) list(background = "lightgrey"),
    sortable = FALSE,
    pagination = FALSE,
    compact = TRUE)
}

## One reactable column per rated column: `headers` gives each its title, `ratings` the
## rating of every row in it. Both are keyed by the column's name in the data.
ratedColumns <- function(headers, ratings) {
  columns <- lapply(names(headers), function(column) {
    reactable::colDef(
      name = headers[[column]],
      html = TRUE,
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
    df = tr("df"),
    chisq = paste0(estimatorName, tr("-&chi;&sup2;")),
    pvalue = tr("p"),
    rmsea = tr("RMSEA"),
    rmseaCi = sprintf(tr("%g%%-CI"), 100 * rmseaCiLvl),
    rmseaP = tr("p<sub>H0:RMSEA<=.05</sub>"),
    rmseaNotClose = tr("p<sub>H0:RMSEA>=.08</sub>"),
    cfi = tr("CFI"),
    srmr = tr("SRMR"))

  columns <- ratedColumns(headers, ratings)

  # The two dividing lines kableExtra::column_spec() drew, after RMSEA and after the second
  # RMSEA p-value.
  columns$rmsea$style <- rightBorderAfter(ratings$rmsea)
  columns$rmseaNotClose$style <- rightBorderAfter(ratings$rmseaNotClose)

  reactable::reactable(
    shown,
    rownames = TRUE,
    columns = c(
      list(.rownames = reactable::colDef(
        name = "", html = TRUE, style = list(fontWeight = "bold"))),
      columns),
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
  seName <- paste0(tr("SE"), "<sub>", estimatorName, "</sub>")
  ciName <- paste0(tr("CI"), "<sub>", estimatorName, "</sub>")

  displayNames <- c(
    item = tr("Item"),
    lambdaLabel = tr("&lambda;<sub>i</sub>"),
    lambdaEst = tr("Est."), lambdaSe = seName, lambdaCi = ciName,
    stdEst = tr("Std. Est."), stdSe = seName, stdCi = ciName,
    alphaLabel = tr("&alpha;<sub>i</sub>"),
    alphaEst = tr("Est."), alphaSe = seName, alphaCi = ciName,
    errorLabel = tr("&sigma;&sup2;<sub>&epsilon;<sub>i</sub></sub>"),
    errorEst = tr("Est."), errorSe = seName, errorCi = ciName,
    relLabel = tr("R<sub>i</sub>"),
    relEst = tr("Est."), relSe = seName, relCi = ciName)

  # The estimates and standard errors are real numbers - makeKable() used to round them to
  # three places for the whole table. Everything else is already text.
  roundedColumns <- c("lambdaEst", "lambdaSe", "stdEst", "stdSe", "alphaEst", "alphaSe",
                      "errorEst", "errorSe", "relEst", "relSe")

  columns <- lapply(names(displayNames), function(column) {
    reactable::colDef(
      name = displayNames[[column]],
      html = TRUE,
      format = if (column %in% roundedColumns)
        reactable::colFormat(digits = 3, locales = "en-US"))
  })

  reactable::reactable(
    parameters,
    columns = stats::setNames(columns, names(displayNames)),

    columnGroups = list(
      reactable::colGroup(
        name = tr("Discrimination Parameters (Factor Loadings)"),
        columns = c("lambdaLabel", "lambdaEst", "lambdaSe", "lambdaCi",
                    "stdEst", "stdSe", "stdCi")),
      reactable::colGroup(
        name = tr("Easiness Parameters (Intercepts)"),
        columns = c("alphaLabel", "alphaEst", "alphaSe", "alphaCi")),
      reactable::colGroup(
        name = tr("Variances"),
        columns = c("errorLabel", "errorEst", "errorSe", "errorCi")),
      reactable::colGroup(
        name = tr("Reliabilities"),
        columns = c("relLabel", "relEst", "relSe", "relCi"))),

    # The last row is the group's own variance and reliability rather than an item's.
    rowStyle = function(index) if (index == rowsPerGroup) list(fontWeight = "bold"),
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
  atLeastSig <- paste0(tr("p >= "), formatBounded(sigLvl))
  belowSig <- paste0(tr("p < "), formatBounded(sigLvl))

  # "Delta-df, ML-Delta-chi-squared, p:" - the three columns those chips describe.
  differenceColumns <- paste(
    c(tr("&Delta;df"), paste0(estimatorName, tr("-&Delta;&chi;&sup2;")), tr("p:")),
    collapse = ", ")

  switch(
    whichLegend,

    "corrTable" = drawLegend(list(
      chip(tr("Legend:")),
      chip(tr("Sig. pos."), "good"),
      chip(tr("Sig. neg."), "bad"),
      chip(tr("Not sig."), "neutral"))),

    "hierTables" = drawLegend(list(
      chip(tr("Legend:")),
      chip(differenceColumns),
      chip(atLeastSig, "good"),
      chip(belowSig, "bad"),

      chip(tr("RMSEA<sub>D</sub>")),
      chip(tr("< .05"), "good"),
      chip(tr(">= .05"), "bad"),
      chip(tr("NA (FIML, lavaan >= 0.6-21)"), "neutral"),

      chip(tr("CFI:")),
      chip(tr(">= .97"), "good"),
      chip(tr(">= .95"), "neutral"),
      chip(tr("< .95"), "bad"),

      chip(tr("AIC, BIC:")),
      chip(tr("min."), "good"),
      chip(tr("else"), "bad"))),

    "fitIndexTable" = drawLegend(
      list(
        chip(tr("Legend:")),
        chip(differenceColumns),
        chip(atLeastSig, "good"),
        chip(belowSig, "bad"),

        chip(""), chip(""), chip(""), chip(""),

        chip(tr("CFI")),
        chip(tr(">= .97"), "good"),
        chip(tr(">= .95"), "neutral"),
        chip(tr("< .95"), "bad"),

        chip(tr("SRMR")),
        chip(tr("< .05"), "good"),
        chip(tr(">= .05"), "bad")),

      list(
        chip(""),
        chip(tr("RMSEA")),
        chip(tr("< .05"), "good"),
        chip(tr(">= .05"), "bad"),

        chip(sprintf(tr("%g%%-CI"), 100 * rmseaCiLvl)),
        chip(tr("< .05"), "good"),
        chip(tr("> .05"), "bad"),
        chip(tr("&ni; .05"), "neutral"),

        chip(tr("p<sub>.05</sub>")),
        chip(paste0(tr(">= "), formatBounded(sigLvl)), "good"),
        chip(paste0(tr("< "), formatBounded(sigLvl)), "bad"),

        chip(tr("p<sub>.08</sub>")),
        chip(paste0(tr("< "), formatBounded(sigLvl)), "good"),
        chip(paste0(tr(">= "), formatBounded(sigLvl)), "bad"),
        chip(""))),

    "combCompTable" = drawLegend(list(
      chip(tr("Legend:")),
      chip(paste0(tr("&Delta;df, "), estimatorName, tr("-&Delta;&chi;&sup2;:"))),
      chip(atLeastSig, "good"),
      chip(belowSig, "bad"),
      chip(tr("* / ** / *** if p < .05 / .01 / .001"), "neutral"))),

    "infCompTable" = drawLegend(list(
      chip(tr("Legend:")),
      chip(tr("AIC/BIC")),
      chip(tr("< 0"), "good"),
      chip(tr("> 0"), "bad"),
      chip(tr("= 0"), "neutral"))),

    stop(sprintf("No legend available for table %s.", whichLegend)))
}
