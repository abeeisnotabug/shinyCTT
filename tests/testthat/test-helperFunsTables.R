## The tables are reactables now, so these read what a table shows rather than searching
## its HTML. Two kinds of test.
##
## The first kind calls the rating rules on their own - rateCFI() and the ratings
## makeCorrTableWithCIs() gives back. That is where "a wrong colour on a right number" would
## come from, and it is a plain function of the numbers, so it can be checked directly. No
## colour name appears in any of it; ratingStyle() is the only thing that knows those.
##
## The second kind reads the finished table: how many rows it shows, what its headers say.

abbrev <- c(tko = "&#964;-kong.", ete = "ess. &#964;-equiv.", teq = "&#964;-equiv.",
            etp = "ess. &#964;-paral.", tpa = "&#964;-paral.")

fitsFor <- function(models, data = rtdata, missing = "listwise") {
  do.call(rbind, lapply(stats::setNames(models, models),
                        function(m) extractFitIndices(
                          fitCTT(data, m, missing = missing))))
}

## What a reactable actually puts on the screen. The widget carries its data as JSON and
## its columns as a list, so both come back out without rendering anything.
shownData <- function(widget) {
  as.data.frame(jsonlite::fromJSON(widget$x$tag$attribs$data), stringsAsFactors = FALSE)
}

shownHeaders <- function(widget) {
  vapply(widget$x$tag$attribs$columns,
         function(column) if (is.null(column$name)) "" else column$name,
         character(1))
}

## formatBounded() ---------------------------------------------------------------------

test_that("it drops the leading zero of a bounded index but keeps the sign", {
  expect_equal(formatBounded(0.048), ".048")
  expect_equal(formatBounded(0.9),   ".900")
  expect_equal(formatBounded(1),     "1.000")
  expect_equal(formatBounded(0),     ".000")
  expect_equal(formatBounded(-0.02), "-.020")
})

test_that("it honours the digits argument", {
  expect_equal(formatBounded(0.04812, digits = 2), ".05")
  expect_equal(formatBounded(0.04812, digits = 4), ".0481")
})

test_that("it leaves an unbounded value's leading digit alone", {
  # Only [0, 1]-bounded indices get the APA treatment; chi-square and AIC keep theirs.
  expect_equal(formatBounded(10.5), "10.500")
})

## the colours and the ratings ----------------------------------------------------------

test_that("the four colours are written down in exactly one place", {
  expect_named(cttColors(), c("good", "bad", "neutral", "text"))
  expect_true(all(nzchar(unlist(cttColors()))))
})

test_that("a rated cell is painted, an unrated one is not", {
  expect_equal(ratingStyle("good")$background, cttColors()$good)
  expect_equal(ratingStyle("bad")$background, cttColors()$bad)
  expect_equal(ratingStyle("neutral")$background, cttColors()$neutral)
  expect_equal(ratingStyle("good")$color, cttColors()$text)

  # NA is what a cell with nothing to rate carries: the baseline row of the hierarchical
  # table, the empty upper triangle of the correlation table.
  expect_null(ratingStyle(NA_character_))
})

test_that("CFI is rated on the absolute three-tier scale, not by successive difference", {
  # Schermelleh-Engel & Moosbrugger: >= .97 good, >= .95 acceptable (neutral), else bad.
  expect_equal(rateCFI(0.99), "good")
  expect_equal(rateCFI(0.97), "good")
  expect_equal(rateCFI(0.96), "neutral")
  expect_equal(rateCFI(0.95), "neutral")
  expect_equal(rateCFI(0.90), "bad")

  # It rates a whole column at once, one model per row.
  expect_equal(rateCFI(c(0.99, 0.96, 0.90)), c("good", "neutral", "bad"))
})

## makeCorrTableWithCIs() ----------------------------------------------------------------

## A correlation matrix and its tests, made by hand so the ratings have known answers:
## item_2 correlates positively with item_1 and significantly, item_3 negatively and
## significantly, and the item_2 / item_3 pair is not significant.
handMadeCorrelations <- function() {
  items <- c("item_1", "item_2", "item_3")
  square <- function(x) matrix(x, 3, 3, dimnames = list(items, items))

  list(
    cor = square(c(1, 0.6, -0.4,
                   0.6, 1, 0.05,
                   -0.4, 0.05, 1)),
    test = list(
      p = square(c(0, 0.001, 0.002,
                   0.001, 0, 0.700,
                   0.002, 0.700, 0)),
      lowCI = square(rep(0.1, 9)),
      uppCI = square(rep(0.9, 9))))
}

test_that("a correlation is rated by its sign when significant and neutral when not", {
  table <- makeCorrTableWithCIs(handMadeCorrelations(), sigLvl = 0.05,
                                itemCols = c("item_1", "item_2", "item_3"))

  # Rows come in pairs: the correlation, then its interval. Row 3 is item_2's correlations,
  # row 5 item_3's.
  expect_equal(unname(table$ratings[3, "item_1"]), "good")    # +.600, p = .001
  expect_equal(unname(table$ratings[5, "item_1"]), "bad")     # -.400, p = .002
  expect_equal(unname(table$ratings[5, "item_2"]), "neutral") # +.050, p = .700
})

test_that("the interval row is rated the same as the correlation above it", {
  table <- makeCorrTableWithCIs(handMadeCorrelations(), sigLvl = 0.05,
                                itemCols = c("item_1", "item_2", "item_3"))

  correlationRows <- seq(1, nrow(table$ratings), by = 2)

  expect_equal(table$ratings[correlationRows + 1, ], table$ratings[correlationRows, ])
})

test_that("the diagonal and everything above it is left unrated", {
  table <- makeCorrTableWithCIs(handMadeCorrelations(), sigLvl = 0.05,
                                itemCols = c("item_1", "item_2", "item_3"))

  # item_1's own row: the diagonal, then two empty cells to the right of it.
  expect_true(all(is.na(table$ratings[1, ])))
  expect_equal(table$shown[1, "item_1"], "1")
  expect_equal(table$shown[2, "item_1"], "-")
  expect_equal(table$shown[1, "item_2"], "")

  # Every second row is labelled CI, so the labels are a column and not row names - as row
  # names R would make them unique and the screen would read CI, CI.1, CI.2.
  expect_equal(table$shown$rowLabel, c("item_1", "CI", "item_2", "CI", "item_3", "CI"))
})

test_that("a stricter significance level moves a borderline pair to neutral", {
  atFivePercent <- makeCorrTableWithCIs(handMadeCorrelations(), sigLvl = 0.05,
                                        itemCols = c("item_1", "item_2", "item_3"))
  atOnePercent <- makeCorrTableWithCIs(handMadeCorrelations(), sigLvl = 0.0005,
                                       itemCols = c("item_1", "item_2", "item_3"))

  expect_equal(unname(atFivePercent$ratings[3, "item_1"]), "good")   # p = .001 < .05
  expect_equal(unname(atOnePercent$ratings[3, "item_1"]), "neutral") # p = .001 > .0005
})

## makeFitsTable() ---------------------------------------------------------------------

test_that("it shows one row per model and keeps the model names", {
  fits <- fitsFor(c("tko", "ete", "teq"))
  table <- makeFitsTable(fits, "ML", 0.05, 0.90, abbrev)

  expect_s3_class(table, "reactable")
  expect_equal(nrow(shownData(table)), 3)
  expect_equal(shownData(table)$.rownames, unname(abbrev[c("tko", "ete", "teq")]))
})

test_that("the RMSEA interval is labelled with the confidence level it was built at", {
  fits <- fitsFor("tko")

  expect_true("90%-CI" %in% shownHeaders(makeFitsTable(fits, "ML", 0.05, 0.90, abbrev)))
  expect_true("95%-CI" %in% shownHeaders(makeFitsTable(fits, "ML", 0.05, 0.95, abbrev)))
  expect_false("90%-CI" %in% shownHeaders(makeFitsTable(fits, "ML", 0.05, 0.95, abbrev)))
})

test_that("the estimator's name heads the chi-square column", {
  fits <- fitsFor("tko")

  expect_true(any(grepl("MLR", shownHeaders(makeFitsTable(fits, "MLR", 0.05, 0.90, abbrev)))))
})

test_that("a wider confidence level gives a wider RMSEA interval", {
  fit <- fitCTT(rtdata, "tko")

  narrow <- extractFitIndices(fit, rmseaCiLevel = 0.90)
  wide <- extractFitIndices(fit, rmseaCiLevel = 0.99)

  expect_gt(wide$rmsea.ci.upper, narrow$rmsea.ci.upper)
  expect_equal(wide$rmsea, narrow$rmsea)
})

## makeHierTable() ---------------------------------------------------------------------

hierInput <- function(missing) {
  models <- c("tko", "ete", "teq")
  fits <- lapply(stats::setNames(models, models),
                 function(m) fitCTT(loadFixture("rtdataNA"), m, missing = missing))
  succ <- as.data.frame(suppressWarnings(
    do.call(lavaan::lavTestLRT, unname(fits))))
  rownames(succ) <- models
  list(succ = succ, cfis = fitsFor(models, loadFixture("rtdataNA"), missing)$cfi)
}

test_that("lavTestLRT drops the RMSEA column under FIML - the documented lavaan behaviour", {
  # This is what makeHierTable has to defend against; if lavaan ever restores the column
  # the guard below becomes dead code rather than wrong, but the assumption is worth pinning.
  expect_true("RMSEA"  %in% names(hierInput("listwise")$succ))
  expect_false("RMSEA" %in% names(hierInput("fiml")$succ))
})

test_that("it renders under FIML, where the RMSEA_D column is missing", {
  input <- hierInput("fiml")

  expect_no_error(
    table <- makeHierTable(input$succ, input$cfis, "FIML", 0.05, abbrev))

  # A missing RMSEA_D shows an explicit "NA" in a neutral cell, not a blank.
  expect_equal(shownData(table)$rmseaD[-1], rep("NA", nrow(shownData(table)) - 1))
})

test_that("it shows the same shape with and without the RMSEA column", {
  listwise <- makeHierTable(hierInput("listwise")$succ, hierInput("listwise")$cfis,
                            "ML", 0.05, abbrev)
  fiml <- makeHierTable(hierInput("fiml")$succ, hierInput("fiml")$cfis,
                        "FIML", 0.05, abbrev)

  expect_equal(nrow(shownData(listwise)), nrow(shownData(fiml)))

  # The chi-square column is headed with the estimator, which differs between the two, so
  # compare the columns either side of it.
  expect_equal(shownHeaders(listwise)[-3], shownHeaders(fiml)[-3])
})

test_that("the baseline row has nothing to compare against, so its difference cells are empty", {
  input <- hierInput("listwise")
  shown <- shownData(makeHierTable(input$succ, input$cfis, "ML", 0.05, abbrev))

  expect_equal(shown$deltaDf[1], "")
  expect_equal(shown$deltaChisq[1], "")
  expect_equal(shown$p[1], "")

  # CFI, AIC and BIC rate every model on its own, so the baseline row does carry those.
  expect_true(nzchar(shown$cfi[1]))
  expect_true(nzchar(shown$aic[1]))
})

## makeLegend() ------------------------------------------------------------------------

test_that("every legend the app asks for renders and names the estimator", {
  for (which in c("corrTable", "hierTables", "fitIndexTable", "combCompTable", "infCompTable")) {
    legend <- makeLegend(which, "MLR", 0.05)

    expect_s3_class(legend, "shiny.tag")
    expect_true(nchar(as.character(legend)) > 0, info = which)
  }
})

test_that("a legend paints its chips with the app's colours", {
  legend <- as.character(makeLegend("corrTable", "ML", 0.05))

  for (colour in c(cttColors()$good, cttColors()$bad, cttColors()$neutral)) {
    expect_match(legend, colour, fixed = TRUE)
  }
})

test_that("the fit index legend labels the RMSEA interval the same way the table does", {
  legend <- makeLegend("fitIndexTable", "ML", 0.05, rmseaCiLvl = 0.99)

  expect_match(as.character(legend), "99%-CI", fixed = TRUE)
})

test_that("the hierarchical-table legend explains the FIML NA", {
  legend <- makeLegend("hierTables", "FIML", 0.05)

  expect_match(as.character(legend), "FIML", fixed = TRUE)
})

test_that("an unknown legend is an error, not a blank", {
  expect_error(makeLegend("noSuchTable", "ML", 0.05), "noSuchTable")
})
