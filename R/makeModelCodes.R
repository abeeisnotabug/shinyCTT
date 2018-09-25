#' @export
makeModelCodes <- function(inputData, itemCols, group = FALSE) {
  itemNames <- colnames(inputData[, itemCols])
  nItems <- length(itemCols)

  models <- c("tko", "ete", "teq", "etp", "tpa")

  oneDisc <- rep("lambda_1", nItems)
  multDisc <- paste("lambda", 1:nItems, sep = "_")

  discPar <- list(multDisc,
                  oneDisc,
                  oneDisc,
                  oneDisc,
                  oneDisc)

  oneEas <- rep("alpha_1", nItems)
  multEas <- paste("alpha", 1:nItems, sep = "_")

  easPar <- list(multEas,
                 multEas,
                 oneEas,
                 multEas,
                 oneEas)

  oneErrVar <- rep("sigma_epsilon_1", nItems)
  multErrVar <- paste("sigma_epsilon", 1:nItems, sep = "_")

  errVar <- list(multErrVar,
                 multErrVar,
                 multErrVar,
                 oneErrVar,
                 oneErrVar)

  names(discPar) <- names(easPar) <- names(errVar) <- models

  modelCodes <- lapply(models, function(model) {
    etaDep <- paste("eta =~",
                     paste(sprintf("%s * %s",
                                   discPar[[model]],
                                   itemNames),
                           collapse = " + "))
    alphas <- paste(sprintf("%s ~ %s * 1",
                            itemNames,
                            easPar[[model]]),
                    collapse = "\n")
    if (!isFALSE(group)) {
      nGroups <- length(unique(inputData[, group]))

      errVars <- paste(sprintf("%s ~~ c(%s) * %s",
                               itemNames,
                               apply(
                                 sapply(1:nGroups,
                                        function(group) sprintf("%s_g%i", errVar[[model]], group)),
                                 1,
                                 paste,
                                 collapse = ", "
                               ),
                               itemNames),
                       collapse = "\n")
      etaVar <- sprintf("eta ~~ c(%s) * eta",
                         paste(paste0("sigma_eta_g", 1:nGroups), collapse = ", "))

      paste(etaDep, errVars, alphas, etaVar, sep = "\n")
    } else {
      errVars <- paste(sprintf("%s ~~ %s * %s",
                                itemNames,
                                errVar[[model]],
                                itemNames),
                        collapse = "\n")
      etaVar <- "eta ~~ sigma_eta * eta"

      # Item reliabilities:
      defNames <- gsub("lambda_1", "1", discPar[[model]])
      itemRels <- paste(
        sprintf("rel_%i := 1 / (1 + %s / (%s^2 * sigma_eta))",
                1:nItems,
                errVar[[model]],
                defNames),
        collapse = "\n"
      )
      # Sum reliability:
      discParSumSq <-  paste0(
        "(",
        paste(defNames,
              collapse = " + "),
        ")^2"
      )
      errVarSumBySigma <- paste0(
        "(",
        paste(errVar[[model]],
              collapse = " + "),
        ") / sigma_eta"
      )
      sumRel <- sprintf("sumrel := %s / (%s + %s)",
                        discParSumSq,
                        discParSumSq,
                        errVarSumBySigma)

      paste(etaDep, errVars, alphas, etaVar, itemRels, sumRel, sep = "\n")
    }
  })

  names(modelCodes) <- models

  modelCodes
}
