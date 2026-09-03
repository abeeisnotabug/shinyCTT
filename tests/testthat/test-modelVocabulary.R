## The five-model vocabulary in server.R - models, possComps' magic [-8], minItems - is a
## hard-coded encoding of the nesting structure. These tests pin the statistical facts that
## encoding stands for, so the restructure can turn it into a data structure without
## silently changing which comparisons the app offers.

test_that("model complexity decreases monotonically along the documented order", {
  order <- c("tko", "ete", "teq", "etp", "tpa")
  dfs <- vapply(order, function(m) as.numeric(lavaan::fitMeasures(fitCTT(rtdata, m), "df")),
                numeric(1))

  # teq and etp both sit at 19 df; every other step strictly adds constraints.
  expect_true(all(diff(dfs) >= 0))
  expect_equal(unname(dfs), c(9, 14, 19, 19, 24))
})

test_that("teq and etp are not nested in each other - the reason for possComps' [-8]", {
  teq <- fitCTT(rtdata, "teq")
  etp <- fitCTT(rtdata, "etp")

  # Equal df with different constraints: neither is a special case of the other, so a
  # likelihood-ratio test between them is meaningless. This is the single pair the
  # comparison grid excludes.
  expect_equal(as.numeric(lavaan::fitMeasures(teq, "df")),
               as.numeric(lavaan::fitMeasures(etp, "df")))
  expect_false(isTRUE(all.equal(as.numeric(lavaan::fitMeasures(teq, "chisq")),
                                as.numeric(lavaan::fitMeasures(etp, "chisq")))))
})

test_that("the comparison set is the nine testable pairs", {
  family <- cttModelFamily()
  models <- family$names

  expect_length(family$comparable, 9)   # 10 lower-triangle pairs, minus the teq/etp pair
  expect_false("etpteq" %in% family$comparable)
  expect_false("teqetp" %in% family$comparable)

  for (comp in family$comparable) {
    expect_true(substr(comp, 1, 3) %in% models)
    expect_true(substr(comp, 4, 6) %in% models)
    expect_false(substr(comp, 1, 3) == substr(comp, 4, 6))
  }
})

test_that("the comparison set still equals the magic [-8] it replaced", {
  # possComps used to be outer(models, models, paste0)[lower.tri(diag(5))][-8], where the
  # [-8] dropped the teq/etp pair *by position* and so silently became wrong if the model
  # order ever changed. Pin that the derived set reproduces it exactly.
  models <- c("tko", "ete", "teq", "etp", "tpa")
  legacy <- outer(models, models, paste0)[lower.tri(diag(5))][-8]

  expect_equal(sort(unname(cttModelFamily()$comparable)), sort(legacy))
})

test_that("nestedPairs() derives the comparison set from the nesting graph alone", {
  # The property a second model family (bifactor S-1, S.I-1, symmetric) relies on: give it
  # a different graph and the comparison set follows, with no hard-coded pair list.
  chain <- nestedPairs(c("a", "b", "c"), rbind(c("a", "b"), c("b", "c")))
  expect_equal(sort(unname(chain)), sort(c("ba", "ca", "cb")))   # transitive: c vs a too

  # Two siblings under one parent are comparable to the parent but not to each other.
  fork <- nestedPairs(c("a", "b", "c"), rbind(c("a", "b"), c("a", "c")))
  expect_equal(sort(unname(fork)), sort(c("ba", "ca")))

  # A family with no nesting at all offers no comparisons.
  expect_length(nestedPairs(c("a", "b"), matrix(character(0), ncol = 2)), 0)
})

test_that("the family reproduces every label vector it replaced", {
  family <- cttModelFamily()

  expect_equal(unname(family$names),  c("tko", "ete", "teq", "etp", "tpa"))
  # The labels come out of inst/translations.csv now, so what is pinned here is that the
  # family reads the right rows, not the text itself.
  expect_equal(unname(family$long)[1], tr("model.tko.long"))
  expect_equal(unname(family$abbrev)[1], tr("model.tko.abbrev"))
  expect_match(unname(family$long)[1], "kongeneric", fixed = TRUE)
  expect_equal(unname(family$minItems), c(4, 3, 2, 2, 2))

  # Every vector is keyed by model name, which is what lets the rest of the app index them.
  for (field in c("names", "long", "abbrev", "minItems"))
    expect_named(family[[field]], unname(family$names), info = field)

  # The plot table carries one row per model, in the same order.
  expect_equal(rownames(family$plot), unname(family$names))
  expect_equal(nrow(family$plot), length(family$names))

  # Its labels must parse - ggplot2 draws them with parse = TRUE.
  for (label in family$plot$name) expect_no_error(parse(text = label))
})

test_that("a comparison needs as many items as its hungrier model", {
  # comparisonGrid() works this out inline, with max() over the two models' minItems. This
  # pins the numbers that go into it, so the thresholds shown in the grid stay right.
  family <- cttModelFamily()

  expect_equal(max(family$minItems[c("tpa", "tko")]), 4)   # tau-parallel vs tau-congeneric
  expect_equal(max(family$minItems[c("teq", "etp")]), 2)
  expect_equal(max(family$minItems[c("ete", "teq")]), 3)
})

test_that("minItems matches the item count at which each model gains a positive df", {
  # server.R's minItems, ui.R's conditionalPanel thresholds and the item-count
  # notification text are three copies of these numbers. This is the ground truth.
  minItems <- c(tko = 4, ete = 3, teq = 2, etp = 2, tpa = 2)

  for (m in names(minItems)) {
    k <- minItems[[m]]
    items <- paste0("item_", seq_len(k))

    ok <- suppressWarnings(try(fitCTT(rtdata, m, items = items), silent = TRUE))
    expect_false(inherits(ok, "try-error"), info = paste(m, "at", k, "items"))
    expect_gt(as.numeric(lavaan::fitMeasures(ok, "df")), 0, label = paste(m, k))

    if (k > 2) {
      # One item fewer leaves no testable model: tau-congeneric at 3 items is
      # just-identified (df = 0, fits perfectly, tests nothing) and at 2 items
      # under-identified (df = -1, which is what killed the goModels observer).
      tooFew <- suppressWarnings(try(
        fitCTT(rtdata, m, items = paste0("item_", seq_len(k - 1))), silent = TRUE))
      if (!inherits(tooFew, "try-error"))
        expect_lte(as.numeric(lavaan::fitMeasures(tooFew, "df")), 0,
                   label = paste(m, "at", k - 1, "items"))
    }
  }
})

test_that("the rtdataWarn fixture still produces a warned but usable fit", {
  # The fixture exists to exercise "a warning does not discard the fit". If lavaan ever
  # stops warning here, the fixture has stopped doing its job and needs rebuilding.
  data <- loadFixture("rtdataWarn")
  codes <- makeModelCodes(data, itemNames())

  expect_warning(
    fit <- lavaan::lavaan(model = codes$tko, data = data, meanstructure = TRUE,
                          estimator = "ML", missing = "listwise", int.ov.free = TRUE,
                          int.lv.free = FALSE, auto.fix.first = TRUE, auto.fix.single = TRUE,
                          auto.var = TRUE, auto.cov.lv.x = TRUE, auto.efa = TRUE,
                          auto.th = TRUE, auto.delta = TRUE, auto.cov.y = TRUE))

  # Warned, but converged and fully usable - which is why goodModels keeps it.
  expect_s4_class(fit, "lavaan")
  expect_true(lavaan::lavInspect(fit, "converged"))
  expect_no_error(extractFitIndices(fit))
  expect_no_error(extractParameters(fit))

  # And genuinely a Heywood case, not just a rounding artefact.
  pe <- lavaan::parameterEstimates(fit)
  expect_lt(pe$est[pe$label == "sigma_epsilon_1"], 0)
})
