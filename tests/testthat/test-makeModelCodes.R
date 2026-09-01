## makeModelCodes() is the join point between the five-model vocabulary and lavaan: the
## models differ only in which parameter labels they reuse, so the tests below assert the
## label-sharing pattern rather than the generated string.

labelsOf <- function(code, prefix) {
  unique(regmatches(code, gregexpr(paste0(prefix, "_\\d+(_g\\d+)?"), code))[[1]])
}

test_that("it returns the five models in the documented order", {
  codes <- makeModelCodes(rtdata, itemNames())

  expect_named(codes, c("tko", "ete", "teq", "etp", "tpa"))
  expect_type(codes, "list")
  expect_true(all(vapply(codes, is.character, logical(1))))
})

test_that("each model constrains the parameters that define it", {
  codes <- makeModelCodes(rtdata, itemNames())

  # Discrimination (loadings): free only in tau-congeneric.
  expect_length(labelsOf(codes$tko, "lambda"), 6)
  for (m in c("ete", "teq", "etp", "tpa"))
    expect_length(labelsOf(codes[[m]], "lambda"), 1)

  # Easiness (intercepts): constrained in the two tau-equivalent models.
  for (m in c("tko", "ete", "etp")) expect_length(labelsOf(codes[[m]], "alpha"), 6)
  for (m in c("teq", "tpa"))        expect_length(labelsOf(codes[[m]], "alpha"), 1)

  # Error variances: constrained in the two parallel models.
  for (m in c("tko", "ete", "teq")) expect_length(labelsOf(codes[[m]], "sigma_epsilon"), 6)
  for (m in c("etp", "tpa"))        expect_length(labelsOf(codes[[m]], "sigma_epsilon"), 1)
})

test_that("every model defines one reliability per item plus a sum reliability", {
  for (code in makeModelCodes(rtdata, itemNames())) {
    expect_length(labelsOf(code, "rel"), 6)
    expect_match(code, "sumrel :=", fixed = TRUE)
  }
})

test_that("the mean structure parameterization switches which parameter is fixed", {
  fixedEta <- makeModelCodes(rtdata, itemNames(), etaIntFree = FALSE)
  freeEta  <- makeModelCodes(rtdata, itemNames(), etaIntFree = TRUE)

  # Free latent mean => the first intercept is fixed instead, and mu_eta is estimated.
  expect_no_match(fixedEta$tko, "alpha_1 == 0", fixed = TRUE)
  expect_match(freeEta$tko,     "alpha_1 == 0", fixed = TRUE)
  expect_match(freeEta$tko,     "mu_eta",       fixed = TRUE)
})

test_that("a group argument produces per-group labels and definitions", {
  single <- makeModelCodes(rtdata, itemNames())
  mg     <- makeModelCodes(rtdata, itemNames(), group = "gender")

  # rtdata's gender has two levels, so every group-varying label gains a _g1/_g2 suffix.
  expect_length(labelsOf(single$tko, "sigma_epsilon"), 6)
  expect_length(labelsOf(mg$tko,     "sigma_epsilon"), 12)

  expect_match(mg$tko, "sigma_eta_g1", fixed = TRUE)
  expect_match(mg$tko, "sigma_eta_g2", fixed = TRUE)
  expect_match(mg$tko, "sumrel_g1 :=", fixed = TRUE)
  expect_match(mg$tko, "sumrel_g2 :=", fixed = TRUE)

  # Loadings are still shared across items within a group for the constrained models.
  expect_length(labelsOf(mg$tpa, "lambda"), 1)
})

test_that("the generated syntax is accepted by lavaan and gives the expected df", {
  # df is the identifiability property the minItems thresholds are derived from: with 6
  # items there are 27 moments, and each added constraint buys back a degree of freedom.
  expected <- c(tko = 9, ete = 14, teq = 19, etp = 19, tpa = 24)

  for (m in names(expected)) {
    fit <- fitCTT(rtdata, model = m)
    expect_true(lavaan::lavInspect(fit, "converged"), info = m)
    expect_equal(as.numeric(lavaan::fitMeasures(fit, "df")), unname(expected[m]), info = m)
  }
})
