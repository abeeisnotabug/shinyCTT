## Pulls the fit indices out of a fitted model.
##
##   rmseaCiLevel : the confidence level of the RMSEA interval. lavaan's own default is
##                  0.90, which is why it is the default here too.
extractFitIndices <- function(fittedModel, rmseaCiLevel = 0.90) {
  scaledAddon <- switch(length(fittedModel@test), "", ".scaled")

  # fitMeasures() rather than lavInspect(fittedModel, "fit"): same numbers under both ML
  # and MLR, but it takes the confidence level for the RMSEA interval.
  rawParams <- lavaan::fitMeasures(
    fittedModel,
    fm.args = list(rmsea.ci.level = rmseaCiLevel))

  paramsDfLeft <- as.data.frame(t(
    rawParams[c(
      paste0(c("df", "chisq", "pvalue"), scaledAddon),
      paste0(c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
               "rmsea.pvalue", "rmsea.notclose.pvalue"), scaledAddon))]
  ))

  paramsDfRight <- as.data.frame(t(
    rawParams[c(
      paste0("cfi", scaledAddon),
      "srmr",
      "aic",
      "bic")]
  ))

  paramsDf <- cbind(paramsDfLeft,
                    # rmsea.ci = sprintf("[%.3f, %.3f]",
                    #                    rawParams[paste0("rmsea.ci.lower", scaledAddon)],
                    #                    rawParams[paste0("rmsea.ci.upper", scaledAddon)]),
                    paramsDfRight,
                    stringsAsFactors = FALSE)

  names(paramsDf) <- gsub(".scaled", "", names(paramsDf))

  paramsDf
}

extractParameters <- function(fittedModel, alpha = 0.05, display = TRUE) {
  nGroups <- fittedModel@Data@ngroups           # Look for multigroup
  etaIntFree <- fittedModel@Options$int.lv.free # Look for standardization

  nItems <- length(lavaan::lavNames(fittedModel))

  if (nGroups > 1) {
    parDf <- lavaan::parameterEstimates(fittedModel,
                                        zstat = FALSE,
                                        pvalue = FALSE,
                                        rsquare = FALSE,
                                        level = 1 - alpha)[, -c(1, 2, 3, 4)]

    parDf$group[parDf$group == 0] <- c(rep(1:nGroups, each = nItems), 1:nGroups)

    stdDf <- lavaan::standardizedSolution(fittedModel,
                                          zstat = FALSE,
                                          pvalue = FALSE,
                                          level = 1 - alpha)[grep("lambda", parDf$label), -c(1, 2, 3)]

    stdDf$label <- rep(paste("std", 1:nItems, sep = "_"), nGroups)
  } else {
    parDf <- cbind(
      group = 1,
      lavaan::parameterEstimates(fittedModel,
                                 zstat = FALSE,
                                 pvalue = FALSE,
                                 rsquare = FALSE,
                                 level = 1 - alpha)[, -c(1, 2, 3)])

    stdDf <- cbind(
      group = 1,
      lavaan::standardizedSolution(fittedModel,
                                   zstat = FALSE,
                                   pvalue = FALSE,
                                   level = 1 - alpha)[grep("lambda", parDf$label), -c(1, 2, 3)])

    stdDf$label <- paste("std", 1:nItems, sep = "_")
  }

  names(stdDf)[names(stdDf) == "est.std"] <- "est"
  df <- rbind(parDf, stdDf[, names(parDf)])

  ## CIs for reliabilities: ------------------------------------------------------------------------------------------------
  rels <- df$est[grep("rel_", df$label)]
  relsSE <- df$se[grep("rel_", df$label)]

  # rels > 1 or < 0 (a Heywood case, see below) makes rels / (1 - rels) negative, and log() of
  # that is NaN - correctly, since the logit isn't defined out there. The ifelse() clamp below
  # discards it before it's ever used, so the NaN is harmless; suppress the console warning it
  # would otherwise print for a value nobody keeps.
  relsLogit <- suppressWarnings(log(rels / (1 - rels)))
  relsLogitSE <- relsSE / (rels * (1 - rels))

  # At a Heywood boundary (rel <= 0 or rel >= 1) the logit is +-Inf and its SE blows up too,
  # so the formula below divides Inf by Inf and returns NaN on one side. Report a zero-width
  # CI pinned to the boundary there instead, rather than let a valid bound sit next to a NaN.
  relsCiL <- ifelse(rels <= 0, 0, ifelse(rels >= 1, 1,
    1 / (1 + exp(-relsLogit + stats::qnorm(1 - alpha / 2) * relsLogitSE))))
  relsCiU <- ifelse(rels <= 0, 0, ifelse(rels >= 1, 1,
    1 / (1 + exp(-relsLogit - stats::qnorm(1 - alpha / 2) * relsLogitSE))))

  df$ci.lower[grep("rel_", df$label)] <- relsCiL
  df$ci.upper[grep("rel_", df$label)] <- relsCiU
  # -----------------------------------------------------------------------------------------------------------------------

  # If there are fixed parameters, omit se and ci
  df[df$se == 0 | is.na(df$se), c("se", "ci.lower", "ci.upper")] <- NA

  df$CI <- ifelse(is.na(df$ci.lower) & is.na(df$ci.upper), NA, sprintf("[%.3f, %.3f]", df$ci.lower, df$ci.upper))
  df <- df[grep("epsilon|alpha|lambda|eta|rel|std", df$label), -c(5, 6)]

  if (display) {
    # Prepare the names for HTML
    df$label <- gsub("_g(\\d+)", "", df$label)

    df$label <- gsub("sigma_epsilon_(\\d+)",
                     "&sigma;&sup2;<sub>&epsilon;<sub>\\1</sub></sub>",
                     df$label)
    df$label <- gsub("lambda_(\\d+)",
                     "&lambda;<sub>\\1</sub>",
                     df$label)
    df$label <- gsub("alpha_(\\d+)",
                     "&alpha;<sub>\\1</sub>",
                     df$label)
    df$label[grep("rel_", df$label)] <- sprintf("R<sub>%i</sub>", 1:length(lavaan::lavNames(fittedModel)))
    df$label[grep("sumrel", df$label)] <- "R<sub>&Sigma;</sub>"
    df$label[grep("sigma_eta", df$label)] <- "&sigma;&sup2;<sub>&eta;</sub>"
    df$label[grep("mu_eta", df$label)] <- "&mu;<sub>&eta;</sub>"

    # Split by groups and bind -----------------------------------------------------------------------------------------------
    splitDf <- lapply(
      split(df, df$group),
      function(subDf) {
        rbind(cbind(Item = lavaan::lavNames(fittedModel),
                    subDf[grep("lambda", subDf$label), -1],
                    subDf[grep("std", subDf$label), -c(1, 2)],
                    subDf[grep("alpha", subDf$label), -1],
                    subDf[grep("epsilon", subDf$label), -1],
                    subDf[grep("R", subDf$label)[1:nItems], -1]),
              c(Item = NA,
                label = NA,
                est = NA,
                se = NA,
                CI = NA,
                est = NA,
                se = NA,
                CI = NA,
                if (etaIntFree) subDf[subDf$label == "&mu;<sub>&eta;</sub>", -1] else c(label = NA, est = NA, se = NA, CI = NA),
                subDf[subDf$label == "&sigma;&sup2;<sub>&eta;</sub>", -1],
                subDf[grep("R<sub>&Sigma;", subDf$label), -1])
        )
      }
    )

    boundDf <- do.call(rbind, splitDf)

    rownames(boundDf) <- NULL

    boundDf
  }
}
