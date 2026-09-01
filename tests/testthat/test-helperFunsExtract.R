## extractFitIndices() ------------------------------------------------------------------

test_that("it returns the documented indices and strips the MLR .scaled suffix", {
  expected <- c("df", "chisq", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
                "rmsea.pvalue", "rmsea.notclose.pvalue", "cfi", "srmr", "aic", "bic")

  ml  <- extractFitIndices(fitCTT(rtdata, "tko", estimator = "ML"))
  mlr <- extractFitIndices(fitCTT(rtdata, "tko", estimator = "MLR"))

  expect_named(ml,  expected)
  expect_named(mlr, expected)   # MLR reports these as *.scaled; the names must match anyway
  expect_equal(nrow(ml), 1)
})

test_that("MLR reports a different chi-square to ML but the same df", {
  ml  <- extractFitIndices(fitCTT(rtdata, "tko", estimator = "ML"))
  mlr <- extractFitIndices(fitCTT(rtdata, "tko", estimator = "MLR"))

  expect_equal(ml$df, mlr$df)
  expect_false(isTRUE(all.equal(ml$chisq, mlr$chisq)))
})

test_that("rbind-ing several models produces the matrix makeFitsTable expects", {
  fits <- do.call(rbind, lapply(
    c(tko = "tko", ete = "ete", teq = "teq"),
    function(m) extractFitIndices(fitCTT(rtdata, m))))

  expect_equal(nrow(fits), 3)
  expect_equal(rownames(fits), c("tko", "ete", "teq"))
  # Model complexity decreases down the list, so df increases and chi-square cannot fall.
  expect_true(all(diff(fits$df) > 0))
  expect_true(all(diff(fits$chisq) > 0))
})

## extractParameters() -----------------------------------------------------------------

test_that("it produces one row per item plus a summary row, in the wide display layout", {
  par <- extractParameters(fitCTT(rtdata, "tko"))

  expect_equal(nrow(par), 6 + 1)          # six items + the eta/sum-reliability row
  expect_equal(ncol(par), 20)             # Item + five blocks
  expect_equal(par$Item[1:6], itemNames())
  expect_true(all(is.na(par[7, "Item"])))
})

test_that("multigroup stacks one block of rows per group", {
  par <- extractParameters(fitCTT(rtdata, "tko", group = "gender"))

  expect_equal(nrow(par), 2 * (6 + 1))
  expect_equal(par$Item[1:6], itemNames())
  expect_equal(par$Item[8:13], itemNames())
})

test_that("labels are rewritten to the HTML entities the tables render", {
  par <- extractParameters(fitCTT(rtdata, "tko"))

  expect_equal(par[[2]][1],  "&lambda;<sub>1</sub>")
  expect_equal(par[[9]][1],  "&alpha;<sub>1</sub>")
  expect_equal(par[[13]][1], "&sigma;&sup2;<sub>&epsilon;<sub>1</sub></sub>")
  expect_equal(par[[17]][1], "R<sub>1</sub>")
  expect_equal(par[[17]][7], "R<sub>&Sigma;</sub>")

  # No circumflex hats: removed because they were illegible next to a subscript.
  expect_false(any(grepl("&#x302;", unlist(par), fixed = TRUE)))
})

test_that("alpha reaches every confidence interval, not just the reliabilities", {
  fit  <- fitCTT(rtdata, "tko")
  wide <- extractParameters(fit, alpha = 0.01)
  narr <- extractParameters(fit, alpha = 0.10)

  # A 99% CI is strictly wider than a 90% one, in the loading block as well as the
  # reliability block - the four non-reliability columns used to be frozen at 95%.
  widthOf <- function(par, col, row) {
    cell <- par[[col]][row]
    diff(as.numeric(unlist(regmatches(cell, gregexpr("-?[0-9.]+", cell)))))
  }
  # Row 2 for the loading: item 1's is fixed by auto.fix.first, so its CI is NA.
  expect_gt(widthOf(wide, 5,  2), widthOf(narr, 5,  2))   # lambda_2 CI
  expect_gt(widthOf(wide, 20, 1), widthOf(narr, 20, 1))   # R_1 CI
})

test_that("a reliability past the Heywood boundary gets a clamped CI, not NaN", {
  # rtdataWarn drives item_1's error variance negative, which pushes its reliability just
  # past 1. The logit-scale CI is undefined there; the clamp reports the boundary instead.
  fit <- fitCTT(loadFixture("rtdataWarn"), "tko")
  par <- extractParameters(fit)

  expect_gt(lavaan::parameterEstimates(fit)$est[
    lavaan::parameterEstimates(fit)$label == "rel_1"], 1)

  expect_equal(par[[20]][1], "[1.000, 1.000]")      # clamped
  expect_false(any(grepl("NaN", unlist(par), fixed = TRUE)))

  # An estimate that is merely close to 1 still gets a real interval.
  expect_false(par[[20]][2] == "[1.000, 1.000]")
})

test_that("the Heywood path does not emit a log() warning", {
  # log(rels / (1 - rels)) is NaN out there; the result is discarded by the clamp, so the
  # console warning it used to print was noise.
  fit <- fitCTT(loadFixture("rtdataWarn"), "tko")
  expect_no_warning(extractParameters(fit))
})
