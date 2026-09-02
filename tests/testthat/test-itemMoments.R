## itemSkewness() and itemKurtosis() (R/mod-descriptives.R), written to replace the moments
## package.
##
## The numbers moments 0.14.1 produced are pinned here as literals, so a later change cannot
## quietly move the descriptive statistics without a test noticing.

test_that("they reproduce what moments::skewness() and moments::kurtosis() gave", {
  # moments uses the plain central moments, with no small-sample correction.
  fromMoments <- c(skew = -0.4612532500, kurt = 3.8001785745)

  expect_equal(itemSkewness(rtdata$item_1), fromMoments[["skew"]], tolerance = 1e-8)
  expect_equal(itemKurtosis(rtdata$item_1), fromMoments[["kurt"]], tolerance = 1e-8)
})

test_that("missing values are dropped rather than poisoning the result", {
  withNAs <- c(rtdata$item_1, NA, NA)

  expect_equal(itemSkewness(withNAs), itemSkewness(rtdata$item_1))
  expect_equal(itemKurtosis(withNAs), itemKurtosis(rtdata$item_1))
})

test_that("a symmetric variable has no skewness, and a normal one excess kurtosis near 0", {
  symmetric <- c(-2, -1, 0, 1, 2)
  expect_equal(itemSkewness(symmetric), 0)

  set.seed(1)
  expect_equal(itemKurtosis(stats::rnorm(20000)) - 3, 0, tolerance = 0.1)
})
