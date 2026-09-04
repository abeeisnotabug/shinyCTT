## The statistics the app works out itself, rather than taking them from a package.
##
## In the order the Statistics tab shows them: the descriptives, the correlation tests, and
## the two normality tests. Each is pinned against the numbers the package it replaced gave,
## in tests/testthat/test-itemMoments.R, test-corTests.R and test-mvnTests.R.

## Mean, standard deviation, skewness and excess kurtosis of every item, as the moments
## package computed them: the third and fourth central moments divided by the second, with
## no small-sample correction. Missing values are dropped item by item.
##
## Takes the item columns and gives back one row per item, so the descriptives table can
## show it as it stands. The excess kurtosis of a normal distribution is 0, hence the -3.
itemMoments <- function(rows) {
  t(apply(
    rows,
    MARGIN = 2,
    FUN = function(col) {
      col <- col[!is.na(col)]
      n <- length(col)
      colMean <- mean(col)
      centered <- col - colMean

      c(Mean = colMean,
        SD = sqrt(sum(centered^2) / (n - 1)),
        Skew = (sum(centered^3) / n) / (sum(centered^2) / n)^(3 / 2),
        Excess = n * sum(centered^4) / sum(centered^2)^2 - 3)
    }))
}

## The p-value and the confidence interval of every pair of items, as three matrices laid
## out the same way as the correlation matrix they are shown beside.
##
##   items     : the item columns, as a data frame
##   use       : how to treat missing values -> the same string stats::cor() is given,
##               "pairwise.complete.obs" or "complete.obs"
##   confLevel : the confidence level of the intervals, e.g. 0.95
##
## Returns list(p =, lowCI =, uppCI =), the three matrices makeCorrTableWithCIs() reads.
corTestMatrices <- function(items, use, confLevel) {

  # "complete.obs" -> throw the incomplete rows away once, up front, so every pair is
  # tested on the same rows the correlations were computed from. cor.test() drops the
  # incomplete pairs by itself, which is what "pairwise.complete.obs" means.
  if (identical(use, "complete.obs")) items <- items[stats::complete.cases(items), ]

  nItems <- ncol(items)

  # Filled in pair by pair below. An item with itself sits on the diagonal: p = 0, and an
  # interval of zero width at 1.
  pMat <- lowMat <- uppMat <- matrix(
    NA_real_, nItems, nItems, dimnames = list(colnames(items), colnames(items)))

  diag(pMat) <- 0
  diag(lowMat) <- diag(uppMat) <- 1

  for (i in seq_len(nItems - 1)) {
    for (j in (i + 1):nItems) {

      thisTest <- stats::cor.test(items[, i], items[, j], conf.level = confLevel)

      pMat[i, j] <- pMat[j, i] <- thisTest$p.value

      # cor.test() reports no interval on fewer than four complete pairs.
      if (!is.null(thisTest$conf.int)) {
        lowMat[i, j] <- lowMat[j, i] <- thisTest$conf.int[1]
        uppMat[i, j] <- uppMat[j, i] <- thisTest$conf.int[2]
      }
    }
  }

  list(p = pMat, lowCI = lowMat, uppCI = uppMat)
}

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
