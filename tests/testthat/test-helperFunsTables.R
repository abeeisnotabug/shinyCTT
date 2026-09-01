colors <- list(good = "darkgreen", bad = "darkred", neutr = "grey", text = "white")

abbrev <- c(tko = "&#964;-kong.", ete = "ess. &#964;-equiv.", teq = "&#964;-equiv.",
            etp = "ess. &#964;-paral.", tpa = "&#964;-paral.")

fitsFor <- function(models, data = rtdata, missing = "listwise") {
  do.call(rbind, lapply(stats::setNames(models, models),
                        function(m) extractFitIndices(
                          fitCTT(data, m, missing = missing))))
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

## makeFitsTable() ---------------------------------------------------------------------

test_that("it renders one row per model and keeps the model names", {
  fits <- fitsFor(c("tko", "ete", "teq"))
  html <- makeFitsTable(fits, "ML", 0.05, 0.90, colors$good, colors$bad, colors$neutr,
                        colors$text, abbrev)

  expect_type(html, "character")
  # kableExtra decodes the numeric entities, so assert on the ASCII-distinctive part of
  # each abbreviation rather than on "&#964;".
  for (a in c("kong.", "ess. ", "equiv.")) expect_match(html, a, fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("<tr>", html))), 3 + 1)  # models + header
})

test_that("CFI is rated on the absolute three-tier scale, not by successive difference", {
  # Schermelleh-Engel & Moosbrugger: >= .97 good, >= .95 acceptable (neutral), else bad.
  rate <- function(cfi) {
    fits <- fitsFor("tko")
    fits$cfi <- cfi
    html <- makeFitsTable(fits, "ML", 0.05, 0.90, colors$good, colors$bad, colors$neutr,
                          colors$text, abbrev)
    html
  }
  expect_match(rate(0.99), colors$good,  fixed = TRUE)
  expect_match(rate(0.96), colors$neutr, fixed = TRUE)
  expect_match(rate(0.90), colors$bad,   fixed = TRUE)
})

test_that("the RMSEA interval is labelled with the confidence level it was built at", {
  fits <- fitsFor("tko")

  html90 <- makeFitsTable(fits, "ML", 0.05, 0.90, colors$good, colors$bad, colors$neutr,
                          colors$text, abbrev)

  expect_match(html90, "90%-CI", fixed = TRUE)
  expect_no_match(html90, "95%-CI", fixed = TRUE)

  html95 <- makeFitsTable(fits, "ML", 0.05, 0.95, colors$good, colors$bad, colors$neutr,
                          colors$text, abbrev)

  expect_match(html95, "95%-CI", fixed = TRUE)
  expect_no_match(html95, "90%-CI", fixed = TRUE)
})

test_that("the fit index legend labels the RMSEA interval the same way", {
  legend <- makeLegend("fitIndexTable", "ML", 0.05, colors$good, colors$bad, colors$neutr,
                       colors$text, rmseaCiLvl = 0.99)

  expect_match(as.character(legend), "99%-CI", fixed = TRUE)
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
    html <- makeHierTable(input$succ, input$cfis, "FIML", 0.05,
                          colors$good, colors$bad, colors$neutr, colors$text, abbrev))
  # Missing RMSEA_D renders as an explicit "NA", not as a blank cell.
  expect_match(html, "NA", fixed = TRUE)
})

test_that("it renders identically-shaped output with and without the RMSEA column", {
  listwise <- hierInput("listwise")
  fiml     <- hierInput("fiml")

  htmlL <- makeHierTable(listwise$succ, listwise$cfis, "ML", 0.05, colors$good,
                         colors$bad, colors$neutr, colors$text, abbrev)
  htmlF <- makeHierTable(fiml$succ, fiml$cfis, "FIML", 0.05, colors$good,
                         colors$bad, colors$neutr, colors$text, abbrev)

  countRows <- function(h) lengths(regmatches(h, gregexpr("<tr>", h)))
  expect_equal(countRows(htmlL), countRows(htmlF))
})

## makeLegend() ------------------------------------------------------------------------

test_that("every legend the app asks for renders and names the estimator", {
  for (which in c("corrTable", "hierTables", "fitIndexTable", "combCompTable", "infCompTable")) {
    legend <- makeLegend(which, "MLR", 0.05, colors$good, colors$bad, colors$neutr, colors$text)
    expect_s3_class(legend, "html")
    expect_true(nchar(legend) > 0, info = which)
  }
})

test_that("the hierarchical-table legend explains the FIML NA", {
  legend <- makeLegend("hierTables", "FIML", 0.05, colors$good, colors$bad,
                       colors$neutr, colors$text)
  expect_match(as.character(legend), "FIML", fixed = TRUE)
})
