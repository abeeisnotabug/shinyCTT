## mardiaTests() and andersonDarlingTests() (R/helpers-stats.R), written to replace the
## MVN package.
##
## The numbers MVN 6.3 printed on rtdata are pinned here as literals, so a later change
## cannot quietly move the normality tab without a test noticing. MVN rounded everything it
## reported to three decimals, so the full-precision values below are this package's own and
## the round() checks are what MVN itself showed.

test_that("mardiaTests() reproduces what MVN::mvn(mvn_test = \"mardia\") gave", {
  fromMVN <- c(skewness = 148.484, kurtosis = 3.684)

  mardia <- mardiaTests(rtdata[, itemNames()])

  expect_equal(mardia$Test, c("Mardia Skewness", "Mardia Kurtosis"))
  expect_equal(round(mardia$Statistic, 3), unname(fromMVN))

  # this package's own precision, so a drift smaller than MVN's rounding still fails
  expect_equal(mardia$Statistic, c(148.48395921696, 3.68433783743), tolerance = 1e-10)
  expect_equal(mardia$p, c(2.62470643233e-10, 2.29297950658e-04), tolerance = 1e-10)
})

test_that("andersonDarlingTests() reproduces what MVN gave, item by item", {
  fromMVN <- data.frame(
    Item = itemNames(),
    Statistic = c(0.792, 0.412, 0.345, 1.031, 0.520, 0.252),
    p = c(0.039, 0.338, 0.484, 0.010, 0.185, 0.734))

  ad <- andersonDarlingTests(rtdata[, itemNames()])

  expect_equal(ad$Test, rep("Anderson-Darling", 6))
  expect_equal(ad$Item, fromMVN$Item)
  expect_equal(round(ad$Statistic, 3), fromMVN$Statistic)
  expect_equal(round(ad$p, 3), fromMVN$p)

  expect_equal(ad$Statistic,
               c(0.792389321782, 0.411564849746, 0.344562041066,
                 1.030502258917, 0.519746057175, 0.252456867271),
               tolerance = 1e-10)
})

test_that("the module can read both tables the same way", {
  # mvnResult() guards every output with is.data.frame() on these two names
  mardia <- mardiaTests(rtdata[, itemNames()])
  ad <- andersonDarlingTests(rtdata[, itemNames()])

  expect_true(is.data.frame(mardia))
  expect_true(is.data.frame(ad))
  expect_equal(names(mardia), c("Test", "Statistic", "p"))
  expect_equal(names(ad), c("Test", "Item", "Statistic", "p"))

  # p-values are numbers now, not MVN's "<0.001" string
  expect_true(is.numeric(mardia$p))
  expect_true(is.numeric(ad$p))
})

test_that("missing values are dropped rather than poisoning the result", {
  withNAs <- rtdata[, itemNames()]
  withNAs[1, "item_1"] <- NA

  # Mardia needs complete cases, so the app hands it stats::na.omit() output
  expect_equal(mardiaTests(stats::na.omit(withNAs))$Statistic,
               mardiaTests(rtdata[-1, itemNames()])$Statistic)

  # Anderson-Darling drops them per item
  expect_equal(andersonDarlingTests(withNAs)$Statistic[-1],
               andersonDarlingTests(rtdata[, itemNames()])$Statistic[-1])
})

test_that("normal data passes both tests and skewed data does not", {
  set.seed(1)
  normal <- as.data.frame(matrix(stats::rnorm(3000), ncol = 3))
  skewed <- as.data.frame(matrix(stats::rexp(3000), ncol = 3))

  expect_true(all(mardiaTests(normal)$p > 0.05))
  expect_true(all(mardiaTests(skewed)$p < 0.05))

  expect_true(all(andersonDarlingTests(normal)$p > 0.05))
  expect_true(all(andersonDarlingTests(skewed)$p < 0.05))
})
