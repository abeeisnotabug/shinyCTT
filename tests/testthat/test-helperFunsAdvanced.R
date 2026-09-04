## getPredictedScores() ----------------------------------------------------------------

test_that("it returns one row per observation with an estimate and a standard error", {
  fit <- fitCTT(rtdata, "tko")
  scores <- getPredictedScores(fit)

  expect_named(scores, c("n", "eta.hat", "se"))
  expect_equal(nrow(scores), nrow(rtdata))
  expect_true(all(is.finite(scores$eta.hat)))
  expect_true(all(scores$se > 0))
})

test_that("multigroup adds the group column and keeps every row aligned to its group", {
  fit <- fitCTT(rtdata, "tko", group = "gender")
  scores <- getPredictedScores(fit, rtdata$gender)

  expect_true("gender" %in% names(scores))
  expect_equal(nrow(scores), nrow(rtdata))
  expect_equal(scores$gender, rtdata$gender)

  # The SE is constant within a group but differs between them, so a row landing in the
  # wrong group's block would show up here.
  expect_equal(length(unique(round(scores$se, 10))), 2)
})

test_that("a multigroup fit without a group variable is an error, not silent nonsense", {
  fit <- fitCTT(rtdata, "tko", group = "gender")
  expect_error(getPredictedScores(fit), "group variable")
})

test_that("scores depend only on the fit, so the table and the download agree", {
  # server.R calls getPredictedScores() twice per model - once for the DT table, once
  # inside the downloadHandler - and the user is entitled to assume the CSV matches what
  # is on screen. Two independent fits of the same model on the same data must therefore
  # produce the same scores; that is what makes computing it once and reusing it safe.
  a <- getPredictedScores(fitCTT(rtdata, "tko"))
  b <- getPredictedScores(fitCTT(rtdata, "tko"))

  expect_equal(a$eta.hat, b$eta.hat)
  expect_equal(a$se, b$se)
})

## makeRCode() -------------------------------------------------------------------------

## makeRCode() takes an explicit dataSource descriptor rather than the `input` object,
## because inside a module `input` sees only its own namespace and every read would come
## back NULL with no error. These helpers build the five descriptor shapes.

wsSource    <- function() list(type = "Workspace", object = "rtdata")
csvSource   <- function(name = "mydata.csv") list(type = "CSV", name = name, header = TRUE,
                                                  sep = ",", quote = "\"")
spssSource  <- function(name = "mydata.sav") list(type = "SPSS", name = name)
rdsSource   <- function(name = "mydata.rds") list(type = "RDS", name = name)
rdataSource <- function(name = "mydata.RData") list(type = "RData", name = name,
                                                    object = "rtdata")

## Positional order: dataSource, groupCol, groups, modelCode, estimator, missingMethod,
## isSubset, model, isMg.
rcode <- function(dataSource = wsSource(), groupCol = "gender", groups = c("m", "f"),
                  modelCode = "eta =~ item_1", estimator = "ML",
                  missingMethod = "listwise", isSubset = FALSE, model = "tko",
                  isMg = FALSE) {
  makeRCode(dataSource, groupCol, groups, modelCode, estimator, missingMethod,
            isSubset, model, isMg)
}

test_that("the generated script loads lavaan, the data, the syntax and the fit", {
  code <- rcode()

  expect_match(code, "library(lavaan)", fixed = TRUE)
  expect_match(code, "rawData <- rtdata", fixed = TRUE)
  expect_match(code, "modelCode <- ", fixed = TRUE)
  expect_match(code, "tkoFitted <- cfa(", fixed = TRUE)
  expect_match(code, 'estimator = "ML"', fixed = TRUE)
  expect_match(code, 'missing = "listwise"', fixed = TRUE)
})

test_that("the multigroup script passes group and group.equal", {
  code <- rcode(estimator = "MLR", missingMethod = "fiml", model = "tpa", isMg = TRUE)

  expect_match(code, "tpaFittedMg <- cfa(", fixed = TRUE)
  expect_match(code, 'group = "gender"', fixed = TRUE)
  expect_match(code, 'group.equal = c("loadings", "intercepts")', fixed = TRUE)
  expect_match(code, 'missing = "fiml"', fixed = TRUE)
})

test_that("a subset of groups adds the subset step and fits the subset", {
  full   <- rcode(isSubset = FALSE)
  subset <- rcode(isSubset = TRUE)

  expect_no_match(full, "subsetData", fixed = TRUE)
  expect_match(subset, "subsetData <- subset(rawData,", fixed = TRUE)
  expect_match(subset, 'data = subsetData', fixed = TRUE)
  expect_match(subset, '"m", "f"', fixed = TRUE)
})

test_that("each data source produces its own loading call", {
  expect_match(rcode(wsSource()), "rawData <- rtdata", fixed = TRUE)

  csv <- rcode(csvSource())
  expect_match(csv, "read.csv(", fixed = TRUE)
  expect_match(csv, 'file = "mydata.csv"', fixed = TRUE)

  sav <- rcode(spssSource())
  expect_match(sav, "haven::read_spss(", fixed = TRUE)
  # The filename must be quoted, or the generated script treats it as a symbol and fails
  # at runtime - and does not even parse when the name contains a space.
  expect_match(sav, 'read_spss(file = "mydata.sav")', fixed = TRUE)

  expect_match(rcode(rdsSource()), 'rawData <- readRDS("mydata.rds")', fixed = TRUE)

  # An .RData takes two lines: load() puts its objects into the workspace under their own
  # names, and the one the user picked is then the data.
  rdata <- rcode(rdataSource())
  expect_match(rdata, 'load("mydata.RData")', fixed = TRUE)
  expect_match(rdata, "rawData <- rtdata", fixed = TRUE)
})

test_that("the generated script parses as valid R, from every source", {
  # The whole point of the Model code tab is that the user can paste it and run it. The
  # source loop matters: the SPSS branch used to emit an unquoted filename, which parses
  # only by accident and breaks outright on a name containing a space.
  for (src in list(wsSource(), csvSource(), spssSource(), spssSource("my data.sav"),
                   rdsSource(), rdsSource("my data.rds"),
                   rdataSource(), rdataSource("my data.RData"))) {
    for (isMg in c(FALSE, TRUE)) {
      for (isSubset in c(FALSE, TRUE)) {
        code <- rcode(src, isSubset = isSubset, isMg = isMg)
        expect_no_error(parse(text = code))
      }
    }
  }
})
