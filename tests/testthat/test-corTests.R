## corTestMatrices() (R/mod-corr-table.R), written to replace corrplot::cor.mtest().
##
## The numbers corrplot 0.95 produced are pinned here as literals, so a later change cannot
## quietly move the correlation table without a test noticing. The last two tests are the
## bug the helper was written for: cor.mtest() always dropped the incomplete pairs, so the
## p-values did not follow the missing-value setting the correlations beside them used.

test_that("it returns the three matrices makeCorrTableWithCIs() reads, laid out like cor()", {
  tests <- corTestMatrices(rtdata[, itemNames()], "pairwise.complete.obs", 0.95)

  expect_named(tests, c("p", "lowCI", "uppCI"))

  for (mat in tests) {
    expect_equal(dim(mat), c(6, 6))
    expect_equal(dimnames(mat), list(itemNames(), itemNames()))
    expect_true(isSymmetric(mat))
  }

  # An item with itself: certain, and an interval of zero width at 1. diag() carries the
  # item names along, which is not what is being tested here.
  expect_equal(unname(diag(tests$p)), rep(0, 6))
  expect_equal(unname(diag(tests$lowCI)), rep(1, 6))
  expect_equal(unname(diag(tests$uppCI)), rep(1, 6))
})

test_that("the pairwise numbers are the ones corrplot::cor.mtest() gave", {
  tests <- corTestMatrices(loadFixture("rtdataNA")[, itemNames()],
                           "pairwise.complete.obs", 0.95)

  # From corrplot 0.95 on the same fixture. Compared relatively, because all.equal() falls
  # back to an absolute comparison on p-values this small and would call anything equal.
  expect_lt(abs(tests$p[1, 2] / 2.718105e-15 - 1), 1e-6)
  expect_equal(tests$lowCI[1, 2], 0.39505534, tolerance = 1e-6)
  expect_equal(tests$uppCI[1, 2], 0.59483337, tolerance = 1e-6)
})

test_that("complete.obs tests the same rows the listwise correlations are computed from", {
  items <- loadFixture("rtdataNA")[, itemNames()]
  completeRows <- items[stats::complete.cases(items), ]

  tests <- corTestMatrices(items, "complete.obs", 0.95)
  direct <- stats::cor.test(completeRows[, 1], completeRows[, 2], conf.level = 0.95)

  expect_equal(tests$p[1, 2], direct$p.value)
  expect_equal(tests$lowCI[1, 2], direct$conf.int[1])
  expect_equal(tests$uppCI[1, 2], direct$conf.int[2])
})

test_that("the two missing-value settings actually give different answers", {
  # This is the bug the helper replaced: corrplot::cor.mtest() always dropped incomplete
  # pairs, so the p-values did not follow the setting the correlations beside them used.
  items <- loadFixture("rtdataNA")[, itemNames()]

  pairwise <- corTestMatrices(items, "pairwise.complete.obs", 0.95)
  complete <- corTestMatrices(items, "complete.obs", 0.95)

  expect_false(isTRUE(all.equal(pairwise$lowCI[1, 2], complete$lowCI[1, 2])))
  expect_gt(complete$p[1, 2], pairwise$p[1, 2])
})

test_that("the confidence level reaches cor.test", {
  items <- rtdata[, itemNames()]

  narrow <- corTestMatrices(items, "pairwise.complete.obs", 0.90)
  wide <- corTestMatrices(items, "pairwise.complete.obs", 0.99)

  expect_gt(narrow$lowCI[1, 2], wide$lowCI[1, 2])
  expect_lt(narrow$uppCI[1, 2], wide$uppCI[1, 2])
})
