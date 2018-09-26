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
extractParameters <- function(fittedModel, alpha = 0.05) {
  parDf <- parameterEstimates(fittedModel,
                              zstat = FALSE,
                              pvalue = FALSE,
                              rsquare = TRUE)[, -c(1, 2, 3)]
  stdDf <- standardizedSolution(fittedModel,
                                zstat = FALSE,
                                pvalue = FALSE)[grep("lambda", parDf$label), -c(1, 2, 3)]

  ## CIs for reliabilities: ----------------------------------------------------
  rels <- parDf$est[grep("rel_", parDf$label)]
  relsSE <- parDf$se[grep("rel_", parDf$label)]

  relsLogit <- log(rels / (1 - rels))
  relsLogitSE <- relsSE / (rels * (1 - rels))

  relsCiL <- 1 / (1 + exp(-relsLogit + qnorm(1 - alpha / 2) * relsLogitSE))
  relsCiU <- 1 / (1 + exp(-relsLogit - qnorm(1 - alpha / 2) * relsLogitSE))

  parDf$ci.lower[grep("rel_", parDf$label)] <- relsCiL
  parDf$ci.upper[grep("rel_", parDf$label)] <- relsCiU
  # ----------------------------------------------------------------------------

  parDf <- parDf[grep("epsilon|alpha|lambda|eta|rel", parDf$label), ]
  parDf$CI <- sprintf("[%.3f, %.3f]", parDf$ci.lower, parDf$ci.upper)
  stdDf$CI <- sprintf("[%.3f, %.3f]", stdDf$ci.lower, stdDf$ci.upper)
  parDf <- parDf[, -c(4, 5)]

  # Prepare the names for HTML
  parDf$label <- gsub("sigma_epsilon_(\\d+)",
                           "&sigma;&#x302;&sup2;<sub>&epsilon;<sub>\\1</sub></sub>",
                           parDf$label)
  parDf$label <- gsub("lambda_(\\d+)",
                           "&lambda;&#x302;<sub>\\1</sub>",
                           parDf$label)
  parDf$label <- gsub("alpha_(\\d+)",
                           "&alpha;&#x302;<sub>\\1</sub>",
                           parDf$label)
  parDf$label[grep("rel_", parDf$label)] <- sprintf("R&#x302;<sub>%i</sub>", 1:length(lavNames(fittedModel)))
  parDf$label[grep("sumrel", parDf$label)] <- "R&#x302;<sub>&Sigma;</sub>"
  parDf$label[grep("sigma_eta", parDf$label)] <- "&sigma;&#x302;&sup2;<sub>&eta;</sub>"

  # Bind all! -------------------------------------------------------------------------------
  rbind(cbind(Item = lavNames(fittedModel),
              parDf[grep("lambda", parDf$label), ],
              stdDf[, -c(3, 4)],
              parDf[grep("alpha", parDf$label), ],
              parDf[grep("epsilon", parDf$label), ],
              parDf[grep("R&#x302;", parDf$label)[1:length(lavNames(fittedModel))], ]),
        c(Item = NA,
          label = NA,
          est = NA,
          se = NA,
          CI = NA,
          est.std = NA,
          se = NA,
          CI = NA,
          label = NA,
          est = NA,
          se = NA,
          CI = NA,
          parDf[grep("eta", parDf$label), ],
          parDf[grep("R&#x302;<sub>&Sigma;", parDf$label), ])
  )
}
