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

extractFitParameters <- function(fittedModel) {
  scaledAddon <- switch(length(fittedModel@test), "", ".scaled")
  rawParams <- lavaan::lavInspect(fittedModel, what = "fit")

  paramsDfLeft <- as.data.frame(t(
    rawParams[c(
      paste0(c("df", "chisq", "pvalue"), scaledAddon),
      paste0(c("rmsea", "rmsea.pvalue"), scaledAddon))]
  ))

  paramsDfRight <- as.data.frame(t(
    rawParams[c(
      paste0("cfi", scaledAddon),
      "srmr",
      "aic",
      "bic")]
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

getPredictedScores <- function(fittedModel, groupVar = FALSE) {
  if (fittedModel@Data@ngroups > 1 && isFALSE(groupVar))
    stop("The data is based on groups but you didn't supply a group variable.")

  n <- do.call(sum, args = fittedModel@Data@nobs)

  out <- data.frame(
    n = 1:n,
    eta.hat = numeric(n),
    se = numeric(n))

  scores <- lavaan::lavPredict(
    fittedModel,
    method = "regression",
    se = "standard")

  if (fittedModel@Data@ngroups > 1) {

    for (group in fittedModel@Data@group.label) {
      out$eta.hat[groupVar == group] <- as.numeric(scores[[which(fittedModel@Data@group.label == group)]])
      out$se[groupVar == group] <- as.numeric(attr(scores, "se")[[which(fittedModel@Data@group.label == group)]])
    }

    out[[fittedModel@Data@group]] <- groupVar

  } else {
    out$eta.hat <- as.numeric(scores)

    out$se <- as.numeric(attr(scores, "se")[[1]])
  }

  out
}

makeRCode <- function(input, modelCode, estimator, isSubset, model, isMg) {
  head <- "library(lavaan)"

  dataInput <- sprintf(
    "rawData <- %s",
    switch(
      input$source,

      "Workspace" = input$objectFromWorkspace,

      "CSV" = sprintf(
"read.csv(
  file = \"%s\",
  header = %s,
  sep = \"%s\",
  quote = \"%s\",
  stringsAsFactors = FALSE)",
        input$CSVFile$name, # file =
        input$header, # header =
        input$sep, # sep =
        ifelse(input$quote == "\"", "\\\"", input$quote)), # quote =

      "SPSS" = sprintf(
        "haven::read_spss(file = %s)",
        input$SPSSFile$name)
    ) # switch(
  ) # sprintf(

  if (isSubset)
    subsetData <- sprintf(
      "subsetData <- subset(rawData, rawData[, \"%s\"] %%in%% c(%s))",
      input$groupCol,
      paste0("\"", paste(input$groups, collapse = "\", \""), "\""))

  modelCodeLine <- sprintf(
"modelCode <- \"
%s
\"",
    modelCode)

  if (isMg) {
    fitLine <- sprintf(
"%sFittedMg <- cfa(
  model = modelCode,
  data = %s,
  meanstructure = TRUE,
  group = \"%s\",
  group.equal = c(\"loadings\", \"intercepts\"),
  estimator = \"%s\")",

      model,
      ifelse(isSubset, "subsetData", "rawData"),
      input$groupCol,
      estimator)

  } else {

    fitLine <- sprintf(
"%sFitted <- cfa(
  model = modelCode,
  data = %s,
  meanstructure = TRUE,
  estimator = \"%s\")",

      model,
      ifelse(isSubset, "subsetData", "rawData"),
      estimator)
  }

  sprintf(
"# Load necesary package: lavaan
%s

# Load the data from the selected source:
%s
%s
# Specify the model syntax:
%s

# Estimate the model:
%s",
    head,
    dataInput,
    ifelse(
      isSubset,
      yes = sprintf("# Select the subset of groups to include:
%s",
                    subsetData),
      no = ""),

  modelCodeLine,
  fitLine)

}
