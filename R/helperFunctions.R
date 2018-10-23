#' @export
makeCorrTableWithCIs <- function(rawTable, goodColor, badColor, neutrColor, textColor, sigLvl, itemCols) {
  CIs <- rawTable$test

  corrTableRaw <- rawTable$cor
  corrTableCors <- corrTableCIs <- matrix(NA, nrow = nrow(corrTableRaw), ncol = ncol(corrTableRaw))
  corrTableComb <- rbind(corrTableCors, corrTableCIs)

  corrTableCors[lower.tri(corrTableCors)] <- kableExtra::cell_spec(
    sprintf("%.3f", corrTableRaw[lower.tri(corrTableRaw)]),
    color = textColor,
    background = ifelse(
      CIs$p[lower.tri(CIs$p)] < sigLvl,
      ifelse(
        corrTableRaw[lower.tri(corrTableRaw)] >= 0,
        goodColor,
        badColor
      ),
      neutrColor
    )
  )
  diag(corrTableCors) <- 1

  corrTableCIs[lower.tri(corrTableCIs)] <- kableExtra::cell_spec(
    sprintf(
      "[%.3f, %.3f]",
      CIs$lowCI[lower.tri(CIs$lowCI)],
      CIs$uppCI[lower.tri(CIs$uppCI)]
    ),
    color = textColor,
    background = ifelse(
      CIs$p[lower.tri(CIs$p)] < sigLvl,
      ifelse(
        corrTableRaw[lower.tri(corrTableRaw)] >= 0,
        goodColor,
        badColor
      ),
      neutrColor
    )
  )
  diag(corrTableCIs) <- "-"

  corrTableComb[seq(1, nrow(corrTableComb), 2), ] <- corrTableCors
  corrTableComb[seq(2, nrow(corrTableComb), 2), ] <- corrTableCIs

  colnames(corrTableComb) <- itemCols
  rownames(corrTableComb) <- c(rbind(itemCols, "CI"))

  corrTableComb
}

#' @export
makeKable <- function(table,
                      digits = 3,
                      full_width = FALSE,
                      position = "center",
                      bootstrap_options = "striped",
                      col.names = NA,
                      row.names = NA,
                      ...) {
  kableExtra::kable_styling(
    kableExtra::kable(table,
                      digits = digits,
                      escape = FALSE,
                      col.names = col.names,
                      row.names = row.names),
    full_width = full_width,
    position = position,
    bootstrap_options = bootstrap_options,
    ...)
}

#' @export
extractFitParameters <- function(fittedModel) {
  scaledAddon <- switch(length(fittedModel@test), "", ".scaled")
  rawParams <- lavInspect(fittedModel, what = "fit")

  paramsDfLeft <- as.data.frame(t(
    rawParams[c(
      paste0(c("df", "chisq", "pvalue"), scaledAddon),
      paste0(c("rmsea", "rmsea.pvalue"), scaledAddon)
    )]
  ))

  paramsDfRight <- as.data.frame(t(
    rawParams[c(
      paste0("cfi", scaledAddon),
      "srmr",
      "aic",
      "bic"
    )]
  ))

  paramsDf <- cbind(paramsDfLeft,
                    rmsea.ci = sprintf("[%.3f, %.3f]",
                                       rawParams[paste0("rmsea.ci.lower", scaledAddon)],
                                       rawParams[paste0("rmsea.ci.upper", scaledAddon)]),
                    paramsDfRight,
                    stringsAsFactors = FALSE)

  names(paramsDf) <- gsub(".scaled", "", names(paramsDf))

  paramsDf
}

#' @export
getPredictedScores <- function(fittedModel, groupVar = FALSE) {
  if (fittedModel@Data@ngroups > 1 && isFALSE(groupVar))
    stop("The data is based on groups but you didn't supply a group variable.")

  n <- do.call(sum, args = fittedModel@Data@nobs)

  out <- data.frame(
    n = 1:n,
    eta.hat = numeric(n)
  )

  if (fittedModel@Data@ngroups > 1) {
    for (group in fittedModel@Data@group.label)
      out$eta.hat[groupVar == group] <- lavPredict(fittedModel)[[which(fittedModel@Data@group.label == group)]]

    out[[fittedModel@Data@group]] <- groupVar
  } else {
    out$eta.hat <- lavPredict(fittedModel)
  }

  out
}
