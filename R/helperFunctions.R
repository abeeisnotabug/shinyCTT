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

extractFitParameters <- function(fittedModel) {
  scaledAddon <- switch(length(fittedModel@test), "", ".scaled")
  rawParams <- lavaan::lavInspect(fittedModel, what = "fit")

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

getPredictedScores <- function(fittedModel, groupVar = FALSE) {
  if (fittedModel@Data@ngroups > 1 && isFALSE(groupVar))
    stop("The data is based on groups but you didn't supply a group variable.")

  n <- do.call(sum, args = fittedModel@Data@nobs)

  out <- data.frame(
    n = 1:n,
    eta.hat = numeric(n),
    se = numeric(n)
  )

  scores <- lavaan::lavPredict(
    fittedModel,
    method = "regression",
    se = "standard"
  )

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
  stringsAsFactors = FALSE
)",
        input$CSVFile$name,
        input$header,
        input$sep,
        ifelse(input$quote == "\"", "\\\"", input$quote)
      ),
      "SPSS" = sprintf(
        "haven::read_spss(file = %s)",
        input$SPSSFile$name
      )
    )
  )

  if (isSubset)
    subsetData <- sprintf(
      "subsetData <- subset(rawData, rawData[, \"%s\"] %%in%% c(%s))",
      input$groupCol,
      paste0("\"", paste(input$groups, collapse = "\", \""), "\"")
    )

  modelCodeLine <- sprintf(
"modelCode <- \"
%s
\"",
    modelCode
  )

  if (isMg) {
    fitLine <- sprintf(
"%sFittedMg <- cfa(
  model = modelCode,
  data = %s,
  meanstructure = TRUE,
  group = \"%s\",
  group.equal = c(\"loadings\", \"intercepts\"),
  estimator = \"%s\"
)",
      model,
      ifelse(isSubset, "subsetData", "rawData"),
      input$groupCol,
      estimator
    )
  } else {
    fitLine <- sprintf(
"%sFitted <- cfa(
  model = modelCode,
  data = %s,
  meanstructure = TRUE,
  estimator = \"%s\"
)",
      model,
      ifelse(isSubset, "subsetData", "rawData"),
      estimator
    )
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
ifelse(isSubset, sprintf("# Select the subset of groups to include:
%s
", subsetData), ""),
modelCodeLine,
fitLine
)
}

# Code to generate the custum FU theme:
if (FALSE) {
  dashboardthemeFU <- dashboardthemes::shinyDashboardThemeDIY(

    ### general
    appFontFamily = "Arial"
    ,appFontColor = "#000000"
    ,bodyBackColor = "#FFFFFF"

    ### header
    ,logoBackColor = "#99CC00"

    ,headerButtonBackColor = "#99CC00"
    ,headerButtonIconColor = "#FFFFFF"
    ,headerButtonBackColorHover = "#666666"
    ,headerButtonIconColorHover = "#FFFFFF"

    ,headerBackColor = "#99CC00"
    ,headerBoxShadowColor = "#AAAAAA"
    ,headerBoxShadowSize = "2px 2px 2px"

    ### sidebar
    ,sidebarBackColor = "#FFFFFF"
    ,sidebarPadding = 0

    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0

    ,sidebarShadowRadius = "3px 5px 5px"
    ,sidebarShadowColor = "#AAAAAA"

    ,sidebarUserTextColor = "#000000"

    ,sidebarSearchBackColor = "rgb(55,72,80)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "#CCCCCC"

    ,sidebarTabTextColor = "#000000"
    ,sidebarTabTextSize = 13
    ,sidebarTabBorderStyle = "none none solid none"
    ,sidebarTabBorderColor = "#CCCCCC"
    ,sidebarTabBorderWidth = 1

    ,sidebarTabBackColorSelected = "#EEEEEE"
    ,sidebarTabTextColorSelected = "#000000"
    ,sidebarTabRadiusSelected = "0px 0px 0px 0px"

    ,sidebarTabBackColorHover = "#EEEEEE"
    ,sidebarTabTextColorHover = "#000000"
    ,sidebarTabBorderStyleHover = "none none solid none"
    ,sidebarTabBorderColorHover = "#CCCCCC"
    ,sidebarTabBorderWidthHover = 1
    ,sidebarTabRadiusHover = "0px 0px 0px 0px"

    ### boxes
    ,boxBackColor = "#FFFFFF"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "#99CC00"

    ,boxPrimaryColor = "rgba(44,222,235,1)"
    ,boxInfoColor = "rgb(210,214,220)"
    ,boxSuccessColor = "rgba(0,255,213,1)"
    ,boxWarningColor = "rgb(244,156,104)"
    ,boxDangerColor = "rgb(255,88,55)"

    ,tabBoxTabColor = "#FFFFFF"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "#000000"
    ,tabBoxTabTextColorSelected = "#000000"
    ,tabBoxBackColor = "#FFFFFF"
    ,tabBoxHighlightColor = "#99CC00"
    ,tabBoxBorderRadius = 5

    ### inputs
    ,buttonBackColor = "#FFFFFF"
    ,buttonTextColor = "#000000"
    ,buttonBorderColor = "#DDDDDD"
    ,buttonBorderRadius = 5

    ,buttonBackColorHover = "#FFFFFF"
    ,buttonTextColorHover = "#000000"
    ,buttonBorderColorHover = "#999999"

    ,textboxBackColor = "rgb(255,255,255)"
    ,textboxBorderColor = "rgb(200,200,200)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(245,245,245)"
    ,textboxBorderColorSelect = "rgb(200,200,200)"

    ### tables
    ,tableBackColor = "rgb(255,255,255)"
    ,tableBorderColor = "rgb(240,240,240)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1

  )
}
