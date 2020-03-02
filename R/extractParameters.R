extractParameters <- function(fittedModel, alpha = 0.05, display = TRUE) {
  nGroups <- fittedModel@Data@ngroups           # Look for multigroup
  etaIntFree <- fittedModel@Options$int.lv.free # Look for standardization

  nItems <- length(lavaan::lavNames(fittedModel))

  if (nGroups > 1) {
    parDf <- lavaan::parameterEstimates(fittedModel,
                                        zstat = FALSE,
                                        pvalue = FALSE,
                                        rsquare = FALSE)[, -c(1, 2, 3, 4)]

    parDf$group[parDf$group == 0] <- c(rep(1:nGroups, each = nItems), 1:nGroups)

    stdDf <- lavaan::standardizedSolution(fittedModel,
                                          zstat = FALSE,
                                          pvalue = FALSE)[grep("lambda", parDf$label), -c(1, 2, 3)]

    stdDf$label <- rep(paste("std", 1:nItems, sep = "_"), nGroups)
  } else {
    parDf <- cbind(
      group = 1,
      lavaan::parameterEstimates(fittedModel,
                                 zstat = FALSE,
                                 pvalue = FALSE,
                                 rsquare = FALSE)[, -c(1, 2, 3)]
    )
    stdDf <- cbind(
      group = 1,
      lavaan::standardizedSolution(fittedModel,
                                   zstat = FALSE,
                                   pvalue = FALSE)[grep("lambda", parDf$label), -c(1, 2, 3)]
    )

    stdDf$label <- paste("std", 1:nItems, sep = "_")
  }

  names(stdDf)[2] <- "est"
  df <- rbind(parDf, stdDf[, names(parDf)])

  ## CIs for reliabilities: ------------------------------------------------------------------------------------------------
  rels <- df$est[grep("rel_", df$label)]
  relsSE <- df$se[grep("rel_", df$label)]

  relsLogit <- log(rels / (1 - rels))
  relsLogitSE <- relsSE / (rels * (1 - rels))

  relsCiL <- 1 / (1 + exp(-relsLogit + qnorm(1 - alpha / 2) * relsLogitSE))
  relsCiU <- 1 / (1 + exp(-relsLogit - qnorm(1 - alpha / 2) * relsLogitSE))

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
                     "&sigma;&#x302;&sup2;<sub>&epsilon;<sub>\\1</sub></sub>",
                     df$label)
    df$label <- gsub("lambda_(\\d+)",
                     "&lambda;&#x302;<sub>\\1</sub>",
                     df$label)
    df$label <- gsub("alpha_(\\d+)",
                     "&alpha;&#x302;<sub>\\1</sub>",
                     df$label)
    df$label[grep("rel_", df$label)] <- sprintf("R&#x302;<sub>%i</sub>", 1:length(lavaan::lavNames(fittedModel)))
    df$label[grep("sumrel", df$label)] <- "R&#x302;<sub>&Sigma;</sub>"
    df$label[grep("sigma_eta", df$label)] <- "&sigma;&#x302;&sup2;<sub>&eta;</sub>"
    df$label[grep("mu_eta", df$label)] <- "&mu;&#x302;<sub>&eta;</sub>"

    # Split by groups and bind -----------------------------------------------------------------------------------------------
    splitDf <- lapply(
      split(df, df$group),
      function(subDf) {
        rbind(cbind(Item = lavaan::lavNames(fittedModel),
                    subDf[grep("lambda", subDf$label), -1],
                    subDf[grep("std", subDf$label), -c(1, 2)],
                    subDf[grep("alpha", subDf$label), -1],
                    subDf[grep("epsilon", subDf$label), -1],
                    subDf[grep("R&#x302;", subDf$label)[1:nItems], -1]),
              c(Item = NA,
                label = NA,
                est = NA,
                se = NA,
                CI = NA,
                est = NA,
                se = NA,
                CI = NA,
                if (etaIntFree) subDf[subDf$label == "&mu;&#x302;<sub>&eta;</sub>", -1] else c(label = NA, est = NA, se = NA, CI = NA),
                subDf[subDf$label == "&sigma;&#x302;&sup2;<sub>&eta;</sub>", -1],
                subDf[grep("R&#x302;<sub>&Sigma;", subDf$label), -1])
        )
      }
    )

    boundDf <- do.call(rbind, splitDf)

    rownames(boundDf) <- NULL

    boundDf
  }
}
