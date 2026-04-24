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
