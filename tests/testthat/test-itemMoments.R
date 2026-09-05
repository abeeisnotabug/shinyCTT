## itemMoments() (R/helpers-stats.R), written to replace the moments package.
##
## The numbers moments 0.14.1 produced are pinned here as literals, so a later change cannot
## quietly move the descriptive statistics without a test noticing.

test_that("it reproduces what moments::skewness() and moments::kurtosis() gave", {
  # moments uses the plain central moments, with no small-sample correction, and reports
  # kurtosis rather than excess kurtosis - hence the 3.
  fromMoments <- c(skew = -0.4612532500, kurt = 3.8001785745)

  moments <- itemMoments(rtdata[, itemNames()])

  expect_equal(moments["item_1", "Skew"], fromMoments[["skew"]], tolerance = 1e-8)
  expect_equal(moments["item_1", "Excess"] + 3, fromMoments[["kurt"]], tolerance = 1e-8)
})

test_that("mean and standard deviation are the ordinary ones", {
  moments <- itemMoments(rtdata[, itemNames()])

  expect_equal(moments[, "Mean"], vapply(rtdata[, itemNames()], mean, numeric(1)))
  expect_equal(moments[, "SD"], vapply(rtdata[, itemNames()], stats::sd, numeric(1)))
})

test_that("it gives one row per item and the four columns the table shows", {
  moments <- itemMoments(rtdata[, itemNames()])

  expect_equal(dim(moments), c(6L, 4L))
  expect_equal(rownames(moments), itemNames())
  expect_equal(colnames(moments), c("Mean", "SD", "Skew", "Excess"))
})

test_that("missing values are dropped rather than poisoning the result", {
  withNAs <- rtdata[, itemNames()]
  withNAs[1:3, "item_2"] <- NA

  # item_2 is worked out from the rows it still has
  expect_equal(itemMoments(withNAs)["item_2", ],
               itemMoments(rtdata[-(1:3), itemNames()])["item_2", ])

  # the other items are untouched
  expect_equal(itemMoments(withNAs)["item_1", ],
               itemMoments(rtdata[, itemNames()])["item_1", ])
})

test_that("a symmetric item has no skewness, and a normal one no excess kurtosis", {
  symmetric <- data.frame(item = c(-2, -1, 0, 1, 2))
  expect_equal(itemMoments(symmetric)[, "Skew"], 0)

  set.seed(1)
  expect_equal(itemMoments(data.frame(item = stats::rnorm(20000)))[, "Excess"],
               0, tolerance = 0.1)
})
