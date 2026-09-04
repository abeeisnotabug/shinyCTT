## The statistics the app works out itself, rather than taking them from a package.
##
## The two normality tests the "Test on multivariate normality" tab shows. Both are pinned
## against the numbers MVN gave, in tests/testthat/test-mvnTests.R.

## Mardia's two tests for multivariate normality, from Mardia (1970).
##
## Takes the item columns, gives back one row per statistic: the name the tab prints, the
## statistic, and its p-value.
mardiaTests <- function(items) {
  x <- as.matrix(items)
  n <- nrow(x)
  p <- ncol(x)

  centered <- scale(x, center = TRUE, scale = FALSE)
  covML <- crossprod(centered) / n              # divide by n, as Mardia does, not by n - 1
  # distances[i, j] -> how far case i lies from case j, in the covariance's own units
  distances <- centered %*% solve(covML) %*% t(centered)

  skewness <- sum(distances^3) / n^2
  skewStat <- n * skewness / 6                  # -> chi-square
  skewDf <- p * (p + 1) * (p + 2) / 6

  kurtosis <- sum(diag(distances)^2) / n
  kurtStat <- (kurtosis - p * (p + 2)) * sqrt(n / (8 * p * (p + 2)))   # -> standard normal

  data.frame(
    Test = c("Mardia Skewness", "Mardia Kurtosis"),
    Statistic = c(skewStat, kurtStat),
    p = c(stats::pchisq(skewStat, df = skewDf, lower.tail = FALSE),
          2 * stats::pnorm(abs(kurtStat), lower.tail = FALSE)),
    stringsAsFactors = FALSE)
}

## The Anderson-Darling test for normality, one item at a time, with each item's own mean
## and standard deviation. The statistic is Anderson & Darling (1954); the small-sample
## correction and the four p-value pieces are D'Agostino & Stephens (1986), table 4.9.
##
## Takes the item columns, gives back one row per item.
andersonDarlingTests <- function(items) {
  oneItem <- function(values) {
    values <- sort(values[!is.na(values)])
    n <- length(values)
    # where each value sits in the normal distribution it would have come from
    position <- stats::pnorm((values - mean(values)) / stats::sd(values))
    rank <- seq_len(n)

    a2 <- -n - mean((2 * rank - 1) * (log(position) + log1p(-rev(position))))
    # the two estimated parameters make a2 too small in a short item
    corrected <- a2 * (1 + 0.75 / n + 2.25 / n^2)

    p <- if (corrected < 0.2) {
      1 - exp(-13.436 + 101.14 * corrected - 223.73 * corrected^2)
    } else if (corrected < 0.34) {
      1 - exp(-8.318 + 42.796 * corrected - 59.938 * corrected^2)
    } else if (corrected < 0.6) {
      exp(0.9177 - 4.279 * corrected - 1.38 * corrected^2)
    } else if (corrected < 10) {
      exp(1.2937 - 5.709 * corrected + 0.0186 * corrected^2)
    } else {
      3.7e-24                                   # the formula's floor
    }

    c(statistic = a2, p = p)
  }

  perItem <- vapply(as.data.frame(items), oneItem, numeric(2))

  data.frame(
    Test = "Anderson-Darling",
    Item = colnames(perItem),
    Statistic = perItem["statistic", ],
    p = perItem["p", ],
    row.names = NULL,
    stringsAsFactors = FALSE)
}
