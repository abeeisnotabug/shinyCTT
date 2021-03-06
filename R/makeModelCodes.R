makeModelCodes <- function(inputData, itemCols, group = FALSE, etaIntFree = FALSE) {
  stdModelCode <- paste(
      "alpha_1 == 0      # Fix first easyness parameter",
      "eta ~ mu_eta * 1  # Allow free estimation of intercept of latent variable\n",
      sep = "\n"
    )

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

      #Item Reliabilities per group:
      defNames <- gsub("lambda_1", "1", discPar[[model]])
      itemRels <- paste(
        sapply(
          1:nGroups,
          function(ig) {
            paste(
              sprintf(
                "rel_%i_g%i := 1 / (1 + %s_g%i / (%s^2 * sigma_eta_g%i))",
                1:nItems,
                ig,
                errVar[[model]],
                ig,
                defNames,
                ig
              ),
              collapse = "\n"
            )
          }
        ),
        collapse = "\n"
      )

      # Sum reliability per group:
      discParSumSq <-  sprintf(
        "(%s)^2",
        paste(defNames,
              collapse = " + ")
      )

      sumRels <- paste(
        sapply(
          1:nGroups,
          function(ig) {
            errVarSumBySigma <- sprintf(
              "(%s) / sigma_eta_g%i",
              paste(
                sprintf(
                  "%s_g%i",
                  errVar[[model]],
                  ig
                ),
                collapse = " + "
              ),
              ig
            )

            sprintf("sumrel_g%i := %s / (%s + %s)",
                    ig,
                    discParSumSq,
                    discParSumSq,
                    errVarSumBySigma)
          }
        ),
        collapse = "\n"
      )
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
        sprintf(
          "rel_%i := 1 / (1 + %s / (%s^2 * sigma_eta))",
          1:nItems,
          errVar[[model]],
          defNames
        ),
        collapse = "\n"
      )

      # Sum reliability:
      discParSumSq <-  sprintf(
        "(%s)^2",
        paste(defNames,
              collapse = " + ")
      )

      errVarSumBySigma <- sprintf(
        "(%s) / sigma_eta",
        paste(errVar[[model]],
              collapse = " + ")
      )

      sumRels <- sprintf(
        "sumrel := %s / (%s + %s)",
        discParSumSq,
        discParSumSq,
        errVarSumBySigma
      )
    }

    paste(etaDep, errVars, alphas, if (etaIntFree) paste0(stdModelCode, etaVar) else etaVar, "\n# Reliability calculation:", itemRels, sumRels, sep = "\n")
  })

  names(modelCodes) <- models

  modelCodes
}
