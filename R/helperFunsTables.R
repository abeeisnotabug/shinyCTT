## Format a fit index that is bounded to [0, 1] (p-values, RMSEA, CFI, SRMR, ...) the
## conventional way: fixed decimal places, no leading zero (".048", not "0.048").
formatBounded <- function(x, digits = 3) {
  sub("^(-?)0\\.", "\\1.", sprintf(paste0("%.", digits, "f"), x))
}

makeKable <- function(table,
                      digits = 3,
                      full_width = FALSE,
                      position = "center",
                      bootstrap_options = "striped",
                      col.names = NA,
                      row.names = NA,
                      bold_cols = integer(0),
                      ...) {

  this_kbl <- kableExtra::kable_styling(
    kableExtra::kbl(table,
                    digits = digits,
                    escape = FALSE,
                    col.names = col.names,
                    row.names = row.names),
    full_width = full_width,
    position = position,
    bootstrap_options = bootstrap_options,
    ...)

  if (length(bold_cols)) {
    kableExtra::column_spec(this_kbl, column = 1, bold = TRUE)
  } else {
    this_kbl
  }
}

makeCorrTableWithCIs <- function(
    rawTable,
    goodColor,
    badColor,
    neutrColor,
    textColor,
    sigLvl,
    itemCols) {

  CIs <- rawTable$test

  corrTableRaw <- rawTable$cor

  corrTableCors <-
    corrTableCIs <-
    matrix(NA, nrow = nrow(corrTableRaw), ncol = ncol(corrTableRaw))

  corrTableComb <- rbind(corrTableCors, corrTableCIs)

  # correlations
  corrTableCors[lower.tri(corrTableCors)] <- kableExtra::cell_spec(

    sprintf("%.3f", corrTableRaw[lower.tri(corrTableRaw)]),

    color = textColor,
    background = ifelse(
      CIs$p[lower.tri(CIs$p)] < sigLvl,
      ifelse(
        corrTableRaw[lower.tri(corrTableRaw)] >= 0,
        yes = goodColor,
        no = badColor),
      neutrColor))

  # diagonal of correlations
  diag(corrTableCors) <- 1

  # confidence intervals
  corrTableCIs[lower.tri(corrTableCIs)] <- kableExtra::cell_spec(

    sprintf(
      "[%.3f, %.3f]",
      CIs$lowCI[lower.tri(CIs$lowCI)],
      CIs$uppCI[lower.tri(CIs$uppCI)]),

    color = textColor,
    background = ifelse(
      CIs$p[lower.tri(CIs$p)] < sigLvl,
      ifelse(
        corrTableRaw[lower.tri(corrTableRaw)] >= 0,
        yes = goodColor,
        no = badColor),
      neutrColor))

  # diagonal of confidence intervals
  diag(corrTableCIs) <- "-"

  corrTableComb[seq(1, nrow(corrTableComb), 2), ] <- corrTableCors
  corrTableComb[seq(2, nrow(corrTableComb), 2), ] <- corrTableCIs

  colnames(corrTableComb) <- itemCols
  rownames(corrTableComb) <- c(rbind(itemCols, "CI"))

  corrTableComb
}

makeHierTable <- function(succTable, CFIs, estimatorName, sigLvl, goodColor, badColor, neutrColor, textColor, modelsAbbrev) {
  hierTable <- succTable
  hierTable$CFI <- CFIs

  ## lavTestLRT() omits the RMSEA-of-the-difference column entirely whenever any compared
  ## model was fitted with missing = "fiml" - regardless of whether the data actually has
  ## missing values. Fill it with NA so the column selection below doesn't error out; it then
  ## renders as a blank, uncoloured cell (see the is.na() guard a few lines down).
  if (!"RMSEA" %in% names(hierTable)) hierTable$RMSEA <- NA_real_

  bgColIfSignif <- ifelse(hierTable[-1, "Pr(>Chisq)"] < sigLvl, yes = badColor, no = goodColor)

  hierTable <- hierTable[, c("Df diff", "Chisq diff", "Pr(>Chisq)", "RMSEA", "CFI", "AIC", "BIC")]

  hierTable[-1, "Df diff"] <- kableExtra::cell_spec(
    sprintf("+%i", hierTable[-1, "Df diff"]),
    color = textColor,
    background = bgColIfSignif)

  hierTable[-1, "Chisq diff"] <- kableExtra::cell_spec(
    sprintf("+%.2f", hierTable[-1, "Chisq diff"]),
    color = textColor,
    background = bgColIfSignif)

  hierTable[-1, "Pr(>Chisq)"] <- kableExtra::cell_spec(
    formatBounded(hierTable[-1, "Pr(>Chisq)"]),
    color = textColor,
    background = bgColIfSignif)

  rmseaD <- hierTable[-1, "RMSEA"]
  hierTable[-1, "RMSEA"] <- kableExtra::cell_spec(
    ifelse(is.na(rmseaD), "NA", formatBounded(rmseaD)),
    color = textColor,
    background = ifelse(is.na(rmseaD), neutrColor, ifelse(rmseaD < 0.05, goodColor, badColor)))

  ## CFI, AIC and BIC rate each model on its own, same as in the fit index table, so unlike
  ## the columns above they are not restricted to rows -1 and compared against reference
  ## values / each other instead of against the row above.
  hierTable$CFI <- kableExtra::cell_spec(
    formatBounded(hierTable$CFI),
    color = textColor,
    background = ifelse(
      hierTable$CFI >= 0.97,
      yes = goodColor,
      no = ifelse(hierTable$CFI >= 0.95, yes = neutrColor, no = badColor)))

  ## Compare the rounded (i.e. displayed) values, not the raw ones - otherwise two AICs that
  ## display identically at one decimal place could still disagree on which is "the minimum".
  aicRounded <- round(as.numeric(hierTable$AIC), 1)
  bicRounded <- round(as.numeric(hierTable$BIC), 1)

  hierTable$AIC <- kableExtra::cell_spec(
    sprintf("%.1f", aicRounded),
    color = textColor,
    background = ifelse(aicRounded == min(aicRounded), goodColor, badColor))

  hierTable$BIC <- kableExtra::cell_spec(
    sprintf("%.1f", bicRounded),
    color = textColor,
    background = ifelse(bicRounded == min(bicRounded), goodColor, badColor))

  names(hierTable) <- c("&Delta;df", paste0(estimatorName, "-&Delta;&chi;&sup2;"), "p",
                        "RMSEA<sub>D</sub>",
                        "CFI",
                        "AIC", "BIC")

  rownames(hierTable) <- modelsAbbrev[rownames(hierTable)]

  makeKable(hierTable, bold_cols = 1) %>%
    kableExtra::row_spec(row = 1, background = "lightgrey")
}

makeFitsTable <- function(fits, estimatorName, sigLvl, rmseaCiLvl, goodColor, badColor, neutrColor, textColor, modelsAbbrev) {
  fitsTable <- fits

  bgColIfSignif <- ifelse(fits$pvalue < sigLvl, badColor, goodColor)

  fitsTable$df <- kableExtra::cell_spec(
    sprintf("%i", fits$df),
    color = textColor,
    background = bgColIfSignif)

  fitsTable$chisq <- kableExtra::cell_spec(
    sprintf("%.2f", fits$chisq),
    color = textColor,
    background = bgColIfSignif)

  fitsTable$pvalue <- kableExtra::cell_spec(
    formatBounded(fits$pvalue),
    color = textColor,
    background = bgColIfSignif)

  fitsTable$rmsea <- kableExtra::cell_spec(
    formatBounded(fits$rmsea),
    color = textColor,
    background = ifelse(fits$rmsea < 0.05, goodColor, badColor))

  fitsTable$rmsea.ci <- kableExtra::cell_spec(
    sprintf("[%s, %s]", formatBounded(fits$rmsea.ci.lower), formatBounded(fits$rmsea.ci.upper)),
    color = textColor,
    background = ifelse(
      fits$rmsea.ci.upper < 0.05,
      yes = goodColor,
      no = ifelse(
        fits$rmsea.ci.lower < 0.05,
        yes = neutrColor,
        no = badColor)))

  fitsTable$rmsea.pvalue <- kableExtra::cell_spec(
    formatBounded(fits$rmsea.pvalue),
    color = textColor,
    background = ifelse(fits$rmsea.pvalue < sigLvl, badColor, goodColor))

  fitsTable$rmsea.notclose.pvalue <- kableExtra::cell_spec(
    formatBounded(fits$rmsea.notclose.pvalue),
    color = textColor,
    background = ifelse(fits$rmsea.notclose.pvalue < sigLvl, goodColor, badColor))

  fitsTable$cfi <- kableExtra::cell_spec(
    formatBounded(fits$cfi),
    color = textColor,
    background = ifelse(
      fits$cfi >= 0.97,
      yes = goodColor,
      no = ifelse(fits$cfi >= 0.95, yes = neutrColor, no = badColor)))

  fitsTable$srmr <- kableExtra::cell_spec(
    formatBounded(fits$srmr),
    color = textColor,
    background = ifelse(fits$srmr < 0.05, goodColor, badColor))

  rownames(fitsTable) <- modelsAbbrev[rownames(fitsTable)]

  makeKable(
    fitsTable[, c("df", "chisq", "pvalue",
                  "rmsea", "rmsea.ci", "rmsea.pvalue", "rmsea.notclose.pvalue",
                  "cfi", "srmr")],
    col.names = c("df", paste0(estimatorName, "-&chi;&sup2;"), "p",
                  "RMSEA", sprintf("%g%%-CI", 100 * rmseaCiLvl),
                  "p<sub>H0:RMSEA<=.05</sub>", "p<sub>H0:RMSEA>=.08</sub>",
                  "CFI", "SRMR"),
    bold_cols = 1) %>%

    kableExtra::column_spec(
      column = c(4, 8),
      border_right = "1px solid lightgrey")
}

makeParTableWithCIs <- function(fitObject, estimatorName, sigLvl, itemCols, Ngroups) {
  SECIestName <- paste0(c("SE", "CI"), "<sub>", estimatorName, "</sub>")

  makeKable(
    extractParameters(
      fitObject,
      alpha = sigLvl),
    col.names = c(
      "Item",
      "&lambda;<sub>i</sub>",
      "Est.", SECIestName,
      "Std. Est.", SECIestName,
      "&alpha;<sub>i</sub>",
      "Est.", SECIestName,
      "&sigma;&sup2;<sub>&epsilon;<sub>i</sub></sub>",
      "Est.", SECIestName,
      "R<sub>i</sub>",
      "Est.", SECIestName),
    bold_cols = 1) %>%

    kableExtra::row_spec(
      row = (length(itemCols) + 1) * 1:Ngroups,
      bold = TRUE) %>%
    kableExtra::add_header_above(
      header =c(" ",
                "Discrimination Parameters (Factor Loadings)" = 7,
                "Easiness Parameters (Intercepts)" = 4,
                "Variances" = 4,
                "Reliabilities" = 4))
}

## Builds the legend under one of the tables.
##
##   rmseaCiLvl : the confidence level of the RMSEA interval. Only the fit index legend
##                shows that interval, so only that call passes it.
makeLegend <- function(whichLegend, estimatorName, sigLvl, goodColor, badColor, neutrColor, textColor,
                       rmseaCiLvl = 0.90) {
  HTML(
    makeKable(
      switch(
        whichLegend,

        "corrTable" = cbind(
          kableExtra::cell_spec("Legend:", bold = TRUE),
          kableExtra::cell_spec(
            "Sig. pos.",
            color = textColor,
            background = goodColor),
          kableExtra::cell_spec(
            "Sig. neg.",
            color = textColor,
            background = badColor),
          kableExtra::cell_spec(
            "Not sig.",
            color = textColor,
            background = neutrColor)),

        "hierTables" = cbind(
          kableExtra::cell_spec("Legend:", bold = TRUE),
          kableExtra::cell_spec(
            paste(c("&Delta;df", paste0(estimatorName, "-&Delta;&chi;&sup2;"), "p:"),
                  collapse = ", "),
            escape = FALSE),
          kableExtra::cell_spec(
            paste0("p >= ", formatBounded(sigLvl)),
            color = textColor,
            background = goodColor),
          kableExtra::cell_spec(
            paste0("p < ", formatBounded(sigLvl)),
            color = textColor,
            background = badColor),

          kableExtra::cell_spec(
            "RMSEA<sub>D</sub>",
            escape = FALSE),
          kableExtra::cell_spec(
            "< .05",
            color = textColor,
            background = goodColor),
          kableExtra::cell_spec(
            ">= .05",
            color = textColor,
            background = badColor),
          kableExtra::cell_spec(
            "NA (FIML, lavaan >= 0.6-21)",
            color = textColor,
            background = neutrColor),

          kableExtra::cell_spec("CFI:"),
          kableExtra::cell_spec(
            ">= .97",
            color = textColor,
            background = goodColor),
          kableExtra::cell_spec(
            ">= .95",
            color = textColor,
            background = neutrColor),
          kableExtra::cell_spec(
            "< .95",
            color = textColor,
            background = badColor),

          kableExtra::cell_spec("AIC, BIC:"),
          kableExtra::cell_spec(
            "min.",
            color = textColor,
            background = goodColor),
          kableExtra::cell_spec(
            "else",
            color = textColor,
            background = badColor)),

        "fitIndexTable" = rbind(
          cbind(
            kableExtra::cell_spec("Legend:", bold = TRUE),
            kableExtra::cell_spec(
              paste(c("&Delta;df", paste0(estimatorName, "-&Delta;&chi;&sup2;"), "p:"),
                    collapse = ", "),
              escape = FALSE),
            kableExtra::cell_spec(
              paste0("p >= ", formatBounded(sigLvl)),
              color = textColor,
              background = goodColor),
            kableExtra::cell_spec(
              paste0("p < ", formatBounded(sigLvl)),
              color = textColor,
              background = badColor),
            kableExtra::cell_spec(""), kableExtra::cell_spec(""), kableExtra::cell_spec(""), kableExtra::cell_spec(""),
            kableExtra::cell_spec("CFI"),
            kableExtra::cell_spec(
              ">= .97",
              color = textColor,
              background = goodColor),
            kableExtra::cell_spec(
              ">= .95",
              color = textColor,
              background = neutrColor),
            kableExtra::cell_spec(
              "< .95",
              color = textColor,
              background = badColor),

            kableExtra::cell_spec("SRMR"),
            kableExtra::cell_spec(
              "< .05",
              color = textColor,
              background = goodColor),
            kableExtra::cell_spec(
              ">= .05",
              color = textColor,
              background = badColor)),

          cbind(
            kableExtra::cell_spec(""),
            kableExtra::cell_spec("RMSEA"),
            kableExtra::cell_spec(
              "< .05",
              color = textColor,
              background = goodColor),
            kableExtra::cell_spec(
              ">= .05",
              color = textColor,
              background = badColor),

            kableExtra::cell_spec(sprintf("%g%%-CI", 100 * rmseaCiLvl)),
            kableExtra::cell_spec(
              "< .05",
              color = textColor,
              background = goodColor),
            kableExtra::cell_spec(
              "> .05",
              color = textColor,
              background = badColor),
            kableExtra::cell_spec(
              "&ni; .05",
              escape = FALSE,
              color = textColor,
              background = neutrColor),

            kableExtra::cell_spec(
              "p<sub>.05</sub>",
              escape = FALSE),
            kableExtra::cell_spec(
              paste0(">= ", formatBounded(sigLvl)),
              color = textColor,
              background = goodColor),
            kableExtra::cell_spec(
              paste0("< ", formatBounded(sigLvl)),
              color = textColor,
              background = badColor),

            kableExtra::cell_spec(
              "p<sub>.08</sub>",
              escape = FALSE),
            kableExtra::cell_spec(
              paste0("< ", formatBounded(sigLvl)),
              color = textColor,
              background = goodColor),
            kableExtra::cell_spec(
              paste0(">= ", formatBounded(sigLvl)),
              color = textColor,
              background = badColor),
            kableExtra::cell_spec(""))),

        "combCompTable" = cbind(
          kableExtra::cell_spec("Legend:", bold = TRUE),
          kableExtra::cell_spec(
            paste0("&Delta;df, ", estimatorName, "-&Delta;&chi;&sup2;:"),
            escape = FALSE),
          kableExtra::cell_spec(
            paste0("p >= ", formatBounded(sigLvl)),
            color = textColor,
            background = goodColor),
          kableExtra::cell_spec(
            paste0("p < ", formatBounded(sigLvl)),
            color = textColor,
            background = badColor),
          kableExtra::cell_spec(
            "* / ** / *** if p < .05 / .01 / .001",
            color = textColor,
            background = neutrColor)),

        "infCompTable" = cbind(
          kableExtra::cell_spec("Legend:", bold = TRUE),
          kableExtra::cell_spec(
            "AIC/BIC",
            escape = FALSE),
          kableExtra::cell_spec(
            "< 0",
            color = textColor,
            background = goodColor),
          kableExtra::cell_spec(
            "> 0",
            color = textColor,
            background = badColor),
          kableExtra::cell_spec(
            "= 0",
            color = textColor,
            background = neutrColor)),

        stop(sprintf("No legend available for table %s.", whichLegend))),

      position = "left",
      bootstrap_options = "condensed")) # HTML(makeKable(
}
