server <- function(input, output, session) {
  # Preparation ----
  ## Names and colors ----
  if (TRUE) {
    goodColor <- "darkgreen"
    badColor <- "darkred"
    textColor <- "white"
    neutrColor <- "grey"
  } else {
    goodColor <- "white"
    badColor <- "white"
    textColor <- "black"
    neutrColor <- "white"
  }

  # Everything about the five models comes from one place: cttModelFamily(), defined in
  # R/modelFamily.R. Read that file to see what each of these contains.
  family <- cttModelFamily()

  models       <- family$names     # c(tko = "tko", ete = "ete", ...)
  modelsLong   <- family$long      # "&tau;-kongeneric", ... - for tab titles
  modelsAbbrev <- family$abbrev    # "&#964;-kong.", ...     - for table headers
  minItems     <- family$minItems  # c(tko = 4, ete = 3, ...) - fewest testable items
  possComps    <- family$comparable # "etetko", "teqtko", ... - the 9 valid comparisons

  # Model labels plus the coordinates the hierarchical plot draws them at.
  modelTestDF <- family$plot

  ## Reactive values ----
  notifications <- reactiveValues(notList = list())

  userDataRaw <- reactiveVal()
  userDataChosen <- reactiveVal()
  userDataNA <- reactiveVal()
  userDataGroup <- reactiveVal()

  itemColsRV <- reactiveVal()
  groupColRV <- reactiveVal()
  validGroupsRV <- reactiveVal()

  fimlRV <- reactiveVal(FALSE)

  # "ML" or "MLR", with "FI" in front when the fits use full information maximum
  # likelihood. Shown in table headers and legends.
  estimatorName <- reactive(paste0(if (fimlRV()) "FI", input$estimator))

  # The fitted models, written by "Test the models" and read by everything on the results
  # tabs. NULL until the button has been pressed.
  modelFitsRV <- reactiveVal(NULL)

  ## The two display settings ----
  # The significance level and the confidence level of the RMSEA interval. Neither is used to
  # fit anything, so both stay live after a run and the tables follow them.
  #
  # What the user typed is never written over: an empty box, or a number out of range, simply
  # is not taken. The tables go on showing the last usable value and a red note appears under
  # the box. (Writing the box back would rewrite it mid-keystroke - see GOTCHAS.md.)
  sigLvlRV <- reactiveVal(0.05)
  rmseaCiLvlRV <- reactiveVal(0.90)

  # An emptied box sends NA, and a box that has not been drawn yet sends NULL.
  sigLvlUsable <- reactive(
    isTRUE(is.numeric(input$sigLvl) && !is.na(input$sigLvl) &&
             input$sigLvl >= 0.001 && input$sigLvl <= 1))

  rmseaCiLvlUsable <- reactive(
    isTRUE(is.numeric(input$rmseaCiLvl) && !is.na(input$rmseaCiLvl) &&
             input$rmseaCiLvl >= 0.5 && input$rmseaCiLvl <= 0.999))

  observeEvent(input$sigLvl, if (sigLvlUsable()) sigLvlRV(input$sigLvl))

  observeEvent(input$rmseaCiLvl, if (rmseaCiLvlUsable()) rmseaCiLvlRV(input$rmseaCiLvl))

  output$sigLvlNote <- renderUI({
    if (sigLvlUsable()) return(NULL)

    sprintf("Enter a number between 0.001 and 1. The tables still use %s.", sigLvlRV()) %>%
      div(style = "color:red")
  })

  output$rmseaCiLvlNote <- renderUI({
    if (rmseaCiLvlUsable()) return(NULL)

    sprintf("Enter a number between 0.5 and 0.999. The tables still use %s.", rmseaCiLvlRV()) %>%
      div(style = "color:red")
  })

  # The estimator the models are fitted with. It mirrors the radio buttons; the normality
  # test reaches it by moving those, not by writing here.
  estimatorRV <- reactiveVal("ML")

  ## Workflow stage ----

  appStage <- reactiveVal("data")

  # Frozen at the transition to "results" so the sidebar re-renders only on stage changes.
  doMgRV <- reactiveVal(FALSE)


  # Which controls belong to which stage: live while that stage is current, frozen once
  # the user moves past it.
  stageControls <- list(
    data       = c("source", "CSVFile", "header", "sep", "quote", "objectFromWorkspace",
                   "dataSelectButton"),
    subset     = c("itemCols", "selectall", "deselectall", "groupCol", "groups",
                   "subsetSelectButton", "useFIML"),
    statistics = c("doMg", "etaIntFree"))

  observeEvent(appStage(), {

    # Every stage before the current one. seq_len(... - 1) stops one short, so the stage the
    # user is actually on keeps its own controls live.
    stagesAlreadyPassed <- stages[seq_len(match(appStage(), stages) - 1)]

    # stageControls[stagesAlreadyPassed] is a list of character vectors, one per stage;
    # unlist() runs them together into a single vector of input ids to freeze.
    controlsToFreeze <- unlist(stageControls[stagesAlreadyPassed], use.names = FALSE)

    for (controlId in controlsToFreeze) shinyjs::disable(controlId)
  })

  ## Group colours ----
  # ggplot2's default discrete palette, but pinned to the group *by name*: a discrete
  # scale hands out its palette to whichever levels are still in the data, so
  # de-selecting a group in a plot tab used to recolour the ones that remain. The light
  # variants - the same colours mixed 40% toward white - are for the density curves,
  # which are drawn on top of bars in the solid colour.
  groupColors <- reactive({
    groupLevels <- sort(unique(userDataGroup()[, input$groupCol]))
    solid <- grDevices::hcl(
      h = seq(15, 375, length.out = length(groupLevels) + 1)[seq_along(groupLevels)],
      c = 100,
      l = 65)

    list(
      solid = stats::setNames(solid, groupLevels),
      light = stats::setNames(
        grDevices::rgb(t(0.6 * grDevices::col2rgb(solid) + 0.4 * 255), maxColorValue = 255),
        groupLevels))
  })

  ## Notifications ----
  output$infoMenu <- shinydashboard::renderMenu({
    if (any(sapply(notifications$notList, grepl, pattern = "danger"))) {
      status <- "danger"
    } else {
      status <- "primary"
    }

    shinydashboard::dropdownMenu(
      type = "notifications",
      .list = notifications$notList,
      badgeStatus = status)
  })

  output$dataMenuOut <- shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(
      id = "dataMenu",
      .list = sidebarGroups(appStage(), doMgRV()))})

  ## observeEvent reload button ----
  observeEvent(input$dataMenu, {
    if (input$dataMenu == "reloadTab")
      shinyjs::runjs("location.reload()")
  })

  ## Display NAs correctly in datatable ----
  options(htmlwidgets.TOJSON_ARGS = list(na = "string"))

  observeEvent(userDataRaw(), {
    output$dataOverview <- userDataRaw() %>%
      DT::datatable() %>%
      DT::formatRound(
        columns = seq_along(userDataRaw())[sapply(userDataRaw(), is.numeric)],
        digits = 3) %>%
      DT::renderDataTable()
  })

  # dataSelectionTab ----
  ## dataSelectionTab objectsInWorkspace ----
  output$objectsInWorkspace <- renderUI({
    selectInput(
      "objectFromWorkspace",
      "1b. Choose data object from Workspace",
      Filter(
        function(object) !is.null(dim(get(object))) && typeof(get(object)) != "character",
        ls(envir = globalenv())))
  })

  ## dataSelectionTab observeEvent data properties ----
  observeEvent(
    list(input$source,
         input$objectFromWorkspace,
         input$CSVFile,
         input$SPSSFile,
         input$header,
         input$sep,
         input$quote), {
    req(identical(appStage(), "data"))

    userDataRaw(NULL)

    shinyjs::disable("dataSelectButton")

    notifications$notList$noData <- shinydashboard::notificationItem(
      text = "No data selected",
      icon = icon("times"),
      status = "danger")

    ### choose data source ----
    if (input$source == "CSV") {
      req(input$CSVFile)

      userDataTmp <- utils::read.csv(
        file = input$CSVFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        stringsAsFactors = FALSE)
    } else if (input$source == "SPSS") {
      req(input$SPSSFile)

      userDataTmp <- haven::read_spss(file = input$SPSSFile$datapath)
    } else if (input$source == "Workspace") {
      req(input$objectFromWorkspace)

      userDataTmp <- get(input$objectFromWorkspace)
    }

    if (any(sapply(userDataTmp, is.factor))) {
      userDataTmp[sapply(userDataTmp, is.factor)] <- lapply(
        userDataTmp[sapply(userDataTmp, is.factor)],
        as.character)
    }

    userDataRaw(data.frame(userDataTmp, stringsAsFactors = FALSE))

    notifications$notList$noData <- NULL

    ### Test the data for problems ----
    if (!any(sapply(userDataRaw(), is.numeric))) {
      notifications$notList$noNumeric <- shinydashboard::notificationItem(
        text = "No numeric columns found",
        icon = icon("times"),
        status = "danger")
      showNotification(
        "No numeric columns found",
        duration = 5,
        id = "noNumericNot",
        type = "error")

    } else {
      notifications$notList$noNumeric <- NULL
      removeNotification("noNumericNot")
    }

    if (length(userDataRaw()) <= 1) {
      notifications$notList$oneCol <- shinydashboard::notificationItem(
        text = "Only one column found",
        icon = icon("times"),
        status = "danger")
      showNotification(
        "Only one column found",
        duration = 5,
        id = "oneColNot",
        type = "error")

    } else {
      notifications$notList$oneCol <- NULL
      removeNotification("oneColNot")
    }

    #### If all is good, enable the select button ----
    if (all(
      is.null(notifications$notList$noNumeric),
      is.null(notifications$notList$oneCol))) {

      shinyjs::enable("dataSelectButton")
    }
  })

  ## dataSelectionTab observeEvent dataSelectButton ----
  observeEvent(input$dataSelectButton, {
    appStage("subset")

    userDataChosen(isolate(userDataRaw()))
    userDataNA(isolate(userDataRaw()))
  })

  # subsetSelectionTab ----
  ## subsetSelectionTab itemColsChooser ----
  output$itemColsChooser <- renderUI({
    possibleItemColumns <- colnames(userDataChosen())[sapply(userDataChosen(), is.numeric)]
    itemColsRV(length(possibleItemColumns))

    tagList(
      checkboxGroupInput(
        "itemCols",
        "2a. Select the item columns:",
        choices = possibleItemColumns,
        selected = possibleItemColumns,
        inline = TRUE),
      fluidRow(actionLink("selectall", "Select all", style = "margin-left: 15px"),
               actionLink("deselectall", "Unselect all", style = "margin-left: 15px")))
  })

  ## subsetSelectionTab groupColChooser ----
  output$groupColChooser <- renderUI({
    possibleGroupCols <- colnames(userDataChosen())[!(colnames(userDataChosen()) %in% input$itemCols)]
    groupColRV(length(possibleGroupCols))

    selectInput(
        "groupCol",
        "2b. Select the group column:",
        choices = c(
          "No group column selected" = "noGroupSelected",
          possibleGroupCols))
  })

  ## subsetSelectionTab groupChooser ----
  output$groupChooser <- renderUI({
    req(input$groupCol)

    if (input$groupCol != "noGroupSelected" && input$groupCol %in% colnames(userDataChosen())) {
      possibleGroups <- unique(stats::na.omit(userDataChosen()[, input$groupCol]))

      if (any(c(table(userDataChosen()[, input$groupCol])) == 1)) {
        groupWarning <- "There are groups with only one observation,
                         you might have selected an item as group column."
        possibleGroups <- NULL

        notifications$notList$invalGroups <- shinydashboard::notificationItem(
          text = "Invalid groups found.",
          icon = icon("times"),
          status = "danger")
        showNotification(
          "Invalid groups found.",
          duration = 5,
          id = "invalGroups",
          type = "error")

      } else {
        groupWarning <- ""

        notifications$notList$invalGroups <- NULL
        removeNotification("invalGroups")
      }

      tagList(
        checkboxGroupInput(
          "groups",
          "2c. Select which groups to include",
          choices = possibleGroups,
          selected = possibleGroups,
          inline = TRUE),
        helpText(groupWarning))
    }
  })

  ## subsetSelectionTab observeEvent for userDataNA ----
  observeEvent(
    list(input$groupCol,
         input$groups,
         input$itemCols), {

    if (atLeastStage(appStage(), "subset")) {

      if (input$groupCol != "noGroupSelected") {

        subset <- userDataChosen()[, input$groupCol] %in% input$groups
        select <- c(input$groupCol, input$itemCols)
      } else {

        subset <- rep(TRUE, nrow(userDataChosen()))
        select <- input$itemCols
      }

      userDataNA(
        subset(
          userDataChosen(),
          subset = subset,
          select = select))
    }
  })

  ## subsetSelectionTab observeEvent selectall ----
  observeEvent(input$selectall, {
    # Only act while the user is still choosing items (see GOTCHAS.md).
    if (input$selectall != 0 && identical(appStage(), "subset")) {
      possibleItemColumns <- colnames(userDataChosen())[sapply(userDataChosen(), is.numeric)]
      itemColsRV(length(possibleItemColumns))

      updateCheckboxGroupInput(
        session,
        "itemCols",
        inline = TRUE,
        choices = possibleItemColumns,
        selected = possibleItemColumns)
    }
  })

  ## subsetSelectionTab observeEvent deselectall ----
  observeEvent(input$deselectall, {
    if (input$deselectall != 0 && identical(appStage(), "subset")) {
      possibleItemColumns <- colnames(userDataChosen())[sapply(userDataChosen(), is.numeric)]
      itemColsRV(length(possibleItemColumns))

      updateCheckboxGroupInput(
        session,
        "itemCols",
        inline = TRUE,
        choices = possibleItemColumns)
    }
  })

  ## subsetSelectionTab observeEvent valid subset and notifications ----
  observeEvent(
    list(input$groupCol,
         input$groups,
         input$itemCols), {
    #req(input$itemCols)

    # Only while the subset stage is current: once the app has moved on, the item and
    # group selections are frozen and this must not hand the button back.
    if (identical(appStage(), "subset")) {

      if (length(input$itemCols) <= 1 ||
          (input$groupCol != "noGroupSelected" && length(input$groups) == 0)) {

        shinyjs::disable("subsetSelectButton") # subset of items
      } else {
        shinyjs::enable("subsetSelectButton")
      }

      ### keep the model selection in step with the item count ----
      # TRUE for each model the current item count is enough to test.
      enoughItems <- minItems <= length(input$itemCols)

      for (thisModel in models) {
        updateCheckboxInput(session, thisModel, value = unname(enoughItems[thisModel]))
      }

      for (thisComp in possComps) {
        updateCheckboxInput(
          session,
          thisComp,
          value = enoughItems[substr(thisComp, 1, 3)] && enoughItems[substr(thisComp, 4, 6)])
      }

      notifications$notList$numItems <- switch(
        as.character(length(input$itemCols)),
        "0" = shinydashboard::notificationItem(
          text = "No item selected. No analysis possible.",
          icon = icon("times"),
          status = "danger"),
        "1" = shinydashboard::notificationItem(
          text = "Only one item selected. No analysis possible.",
          icon = icon("times"),
          status = "danger"),
        "2" = shinydashboard::notificationItem(
          text = HTML("Only two items selected. Unable to test the &tau;-kongeneric and
                      the ess. &tau;-equivalent model."),
          icon = icon("exclamation-triangle"),
          status = "warning"),
        "3" = shinydashboard::notificationItem(
          text = HTML("Only three items selected. Unable to test the &tau;-kongeneric model."),
          icon = icon("exclamation-triangle"),
          status = "warning"),
        NULL)

      if (!is.null(notifications$notList$numItems)) {
        showNotification(
          ui = notifications$notList$numItems$children[[1]]$children[[2]],
          duration = 5,
          id = "numItemsNot",
          type = ifelse(notifications$notList$numItems$children[[1]]$children[[1]]$attribs[[4]] == "text-danger",
                        yes = "error",
                        no = "warning"))
      } else {
        removeNotification("numItemsNot")
      }
    }
  })

  ## subsetSelectionTab itemInfoBox ----
  output$itemInfoBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = itemColsRV(),
      color = switch(
          as.character(itemColsRV()),
          "0" = "red",
          "1" = "red",
          "2" = "orange",
          "3" = "orange",
          "green"),
      subtitle = "possible item column(s) found",
      icon = icon("list"))
  })

  ## subsetSelectionTab groupInfoBox ----
  output$groupInfoBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = groupColRV(),
      color = "blue",
      subtitle = "possible group column(s) found",
      icon = icon("users"))
  })

  ## subsetSelectionTab incomplete cases ----
  # TRUE for every row of the chosen subset that has a missing value somewhere.
  # Used by: the yellow NA box, the observations table, and the FIML checkbox.
  incompleteCases <- reactive({
    req(userDataNA())
    !stats::complete.cases(userDataNA())
  })

  output$incompleteCasesBoolRV <- reactive(any(incompleteCases()))
  outputOptions(output, "incompleteCasesBoolRV", suspendWhenHidden = FALSE)

  ## subsetSelectionTab naInfoBox ----
  output$naInfoBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = sum(incompleteCases()),
      color = if (any(incompleteCases())) "yellow" else "green",
      subtitle = "rows with missing values in this subset",
      icon = icon("exclamation-triangle"))
  })

  ## subsetSelectionTab naTable ----
  output$naTable <- renderUI({
    HTML(makeKable(data.frame(NAs = colSums(is.na(userDataChosen())))))
  })

  ## subsetSelectionTab obsTable ----
  output$obsTable <- renderUI({
    nTotal <- nrow(userDataNA())
    nComplete <- sum(!incompleteCases())

    HTML(makeKable(data.frame(Total = nTotal, Complete = nComplete)))
  })

  ## subsetSelectionTab obsPerGroupTable ----
  output$obsPerGroupTable <- renderUI({
      req(input$groupCol)

      if (input$groupCol != "noGroupSelected") {
        HTML(makeKable(t(table(userDataChosen()[, input$groupCol], useNA = "ifany"))))
      } else {
        helpText("No group column selected.")
      }
  })

  ## subsetSelectionTab observeEvent subsetSelectButton ----
  observeEvent(input$subsetSelectButton, {
    appStage("statistics")

    fimlRV(any(incompleteCases()) && isTRUE(input$useFIML))

    if (input$groupCol != "noGroupSelected") {

      subset <- userDataChosen()[, input$groupCol] %in% input$groups
      select <- c(input$groupCol, input$itemCols)
    } else {

      subset <- rep(TRUE, nrow(userDataChosen()))
      select <- input$itemCols
    }

    userDataGroup(
      subset(
        userDataChosen(),
        subset = subset,
        select = select))

    validGroupsRV(
      input$groupCol != "noGroupSelected" &&
        !any(c(table(userDataGroup()[, input$groupCol])) == 1) &&
        length(input$groups) > 1)

    if (validGroupsRV()) {
      shinyjs::enable("doMg")

      updateCheckboxInput(
        session,
        "doMg",
        value = TRUE)
    }

    # The two boxes on the Correlations tab relabel their own controls; this is the one on
    # the Testing Parameters tab.
    if (fimlRV()) {
      updateRadioButtons(
        inputId = "estimator",
        choices = c("(Full Information) Maximum Likelihood" = "ML",
                    "Robust (Full Information) Maximum Likelihood" = "MLR"))
    }

    if (any(incompleteCases())) {
      notifications$notList$NAhand <- shinydashboard::notificationItem(
        text = HTML("For all plots and the multivariate normality analyses<br/>
                      rows with missing values have been removed."),
        icon = icon("exclamation-triangle"),
        status = "warning")

      showNotification(
        ui = "For all plots and the multivariate normality analyses
                rows with missing values have been removed.",
        duration = 5,
        id = "NAremovedNot",
        type = "warning")
    }
  })

  # statisticsTab ----
  ## statisticsTab descriptive statistics ----
  # The whole box and both tables live in R/mod-descriptives.R.
  descriptivesServer(
    "descriptives",
    data = userDataGroup,
    itemCols = reactive(input$itemCols),
    groupCol = reactive(input$groupCol),
    hasGroups = validGroupsRV)

  ## statisticsTab histogram ----
  # The whole box, its controls and both plots live in R/mod-histogram.R.
  histogramServer(
    "histogram",
    data = userDataGroup,
    itemCols = reactive(input$itemCols),
    groupCol = reactive(input$groupCol),
    hasGroups = validGroupsRV,
    groupColors = groupColors)

  ## statisticsTab covariance matrix ----
  # The whole box and both tables live in R/mod-covmatrix.R.
  covMatrixServer(
    "covmatrix",
    data = userDataGroup,
    itemCols = reactive(input$itemCols),
    groupCol = reactive(input$groupCol),
    hasGroups = validGroupsRV)

  # corrTab ----
  ## corrTab test on correlative independence ----
  # The whole box, its two controls and the test live in R/mod-corr-independence.R.
  corrIndependenceServer(
    "corrIndependence",
    data = userDataGroup,
    itemCols = reactive(input$itemCols),
    useFIML = fimlRV)

  ## mvnTab multivariate plot ----
  output$mvnPlot <- renderPlot({
    req(userDataGroup(), input$mvnPlotType)
    if (input$mvnPlotType != "qq") req(input$mvnItemX, input$mvnItemY)

    userDataNAOmit <- stats::na.omit(userDataGroup())

    if (input$mvnPlotType == "qq") {
      MVN::multivariate_diagnostic_plot(
        stats::na.omit(userDataNAOmit[, input$itemCols]),
        type = "qq")

    } else if (input$mvnPlotType == "persp") {
      graphics::persp(x = MASS::kde2d(userDataNAOmit[, input$mvnItemX],
                            userDataNAOmit[, input$mvnItemY],
                            n = 100),
            theta = 1, phi = 30, border = NA, shade = 0.5, box = T,
            xlab = input$mvnItemX,
            ylab = input$mvnItemY,
            zlab = "Density")

    } else if (input$mvnPlotType == "contour") {
      graphics::contour(x = MASS::kde2d(userDataNAOmit[, input$mvnItemX],
                              userDataNAOmit[, input$mvnItemY],
                              n = 100),
              nlevels = 20,
              xlab = input$mvnItemX,
              ylab = input$mvnItemY)
    }
  })

  ## corrTab scatter plot ----
  # The whole box, its controls and both plots live in R/mod-scatter.R.
  scatterServer(
    "scatter",
    data = userDataGroup,
    itemCols = reactive(input$itemCols),
    groupCol = reactive(input$groupCol),
    hasGroups = validGroupsRV,
    groupColors = groupColors)

  ## corrTab correlation table ----
  # The whole box, its two controls and the table live in R/mod-corr-table.R.
  corrTableServer(
    "corrTable",
    data = userDataGroup,
    itemCols = reactive(input$itemCols),
    groupCol = reactive(input$groupCol),
    hasGroups = validGroupsRV,
    estimatorName = estimatorName,
    sigLvl = sigLvlRV,
    useFIML = fimlRV,
    goodColor = goodColor,
    badColor = badColor,
    neutrColor = neutrColor,
    textColor = textColor)

  # observeEvent input$estimator ----
  observeEvent(input$estimator, estimatorRV(input$estimator))

  # The button ----
  # TRUE when the models on screen were fitted with a different estimator from the one now
  # chosen, so they no longer match it.
  refitPending <- reactive(
    !is.null(modelFitsRV()) &&
      !identical(modelFitsRV()$single$estimator, estimatorRV()))

  # Live before the first run, and after it only while a refit would change something.
  # Pressing it with the same settings would give the same results, so it is switched off.
  observe({
    if (is.null(modelFitsRV()) || refitPending()) {
      shinyjs::enable("goModels")
    } else {
      shinyjs::disable("goModels")
    }

    updateActionButton(
      session,
      "goModels",
      label = paste0("Fit and compare models", if (refitPending()) "*"))
  })

  output$refitPendingNote <- renderUI({
    if (!refitPending()) return(NULL)

    tagList(
      br(),
      HTML("The estimator has changed. The results still come from the estimator that
            was chosen when the models were last fitted. Press
            <b>Fit and compare models*</b> to fit them again.") %>%
        div(style = "color:orange"))
  })

  # mvnTab ----
  # The whole tab lives in R/mod-mvn.R. It reports which estimator the normality test points
  # to; moving the radio buttons and saying so is done here.
  recommendedEstimator <- mvnServer(
    "mvn",
    data = userDataGroup,
    itemCols = reactive(input$itemCols))

  ## act on what the normality test found ----
  # An observer, not an output, so the test runs as soon as the data is ready rather than
  # waiting for the tab to be opened.
  observeEvent(recommendedEstimator(), {

    updateRadioButtons(session, "estimator", selected = recommendedEstimator())

    notifications$notList$estUpdate <- shinydashboard::notificationItem(
      text = "Updated estimator based on MVN test result.",
      icon = icon("wrench"),
      status = "warning")

    showNotification(
      ui = "Updated estimator based on MVN test result.",
      duration = 5,
      id = "estUpdateNot",
      type = "warning")

    notifications$notList$mvnApp <- shinydashboard::notificationItem(
      text = HTML("For more extensive analyses on multivariate normality,<br/>
                    load() the MVN package and open its shiny app via run_mvn_app()!"),
      icon = icon("lightbulb"),
      status = "success")
  })

  ## the recommendation, next to the estimator buttons ----
  output$estimatorNote <- renderUI({

    req(recommendedEstimator())

    estimatorLongName <- c(ML = "Maximum Likelihood",
                           MLR = "Robust Maximum Likelihood")[recommendedEstimator()]

    sprintf("The test on multivariate normality recommends %s. See <i>3. Statistics</i>
             &rarr; <i>Test on Multivariate Normality</i>.",
            estimatorLongName) %>%
      HTML() %>%
      div(style = "color:orange; font-size: 90%")
  })

  # observeEvent input$goModels ----
  # Pressing "Test the models" fits the models and stores them, and does nothing else.
  # Everything drawn from them is built under "Results" below and redraws on its own.
  observeEvent(input$goModels, tryCatch({
    output$goModelsError <- renderUI(NULL)

    # Whatever is on screen was fitted with the settings as they were before this press.
    modelFitsRV(NULL)

    # Freeze the multigroup choice, then reveal the results entries in the sidebar.
    doMgRV(isTRUE(input$doMg))
    appStage("results")

    modelsToTest <- models[sapply(models, function(thisModel) input[[thisModel]])]
    comps <- possComps[sapply(possComps, function(thisComp) input[[thisComp]])]

    # Where the data came from, so makeRCode() can write the matching
    # read.csv() / read_spss() / workspace line into the exported script.
    dataSource <- switch(
      input$source,
      "Workspace" = list(type = "Workspace", object = input$objectFromWorkspace),
      "CSV" = list(type = "CSV",
                   name = input$CSVFile$name,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote),
      "SPSS" = list(type = "SPSS", name = input$SPSSFile$name))

    # The name of the data set, used as the first part of the factor score filename.
    dataName <- switch(
      input$source,
      "Workspace" = input$objectFromWorkspace,
      "CSV" = gsub("\\.csv", "", input$CSVFile$name),
      "SPSS" = gsub("\\.sav|\\.zsav|\\.por", "", input$SPSSFile$name))

    # TRUE when the user left some of the groups out, so the exported script has to
    # subset the data before fitting.
    isSubset <- (
      validGroupsRV() &&
        (length(unique(userDataGroup()[, input$groupCol])) <
           length(unique(userDataRaw()[, input$groupCol]))))

    # Which group each row belongs to, for the predicted factor scores. FALSE when the
    # user chose no group column, because then the data has no such column to read.
    groupValues <- if (input$groupCol == "noGroupSelected") {
      FALSE
    } else {
      userDataGroup()[, input$groupCol]
    }

    ## test the models! ----
    # One pass over the whole sample, plus one fitting the groups separately if the user
    # asked for that.
    passes <- list(single = FALSE)
    if (isTRUE(input$doMg)) passes$multigroup <- input$groupCol

    modelFitsRV(lapply(passes, function(groupName) {

      ### try fitting and capture warning and error messages ----
      modelCodes <- makeModelCodes(inputData = userDataGroup(),
                                              itemCols = input$itemCols,
                                              group = groupName,
                                              etaIntFree = as.logical(input$etaIntFree))

      #### fit each model once, keeping both a warning and the completed fit ----
      # Fit each chosen model:
      #   warning -> note it, carry on, keep the fit
      #   error   -> keep the error message instead of a fit
      # group and group.equal are NULL without a grouping column, so one call does both.
      fittedModelsWarns <- lapply(
        modelCodes[modelsToTest],
        FUN = function(model) {
          warnCond <- NULL
          fit <- withCallingHandlers(
            tryCatch(lavaan::lavaan(model = model,
                                    data = userDataGroup(),
                                    meanstructure = TRUE,
                                    group = if (isFALSE(groupName)) NULL else groupName,
                                    group.equal = if (isFALSE(groupName)) NULL else c("loadings", "intercepts"),
                                    estimator = estimatorRV(),
                                    missing = ifelse(fimlRV(), "fiml", "listwise"),
                                    int.ov.free = TRUE,
                                    int.lv.free = as.logical(input$etaIntFree),
                                    auto.fix.first = TRUE,
                                    auto.fix.single = TRUE,
                                    auto.var = TRUE,
                                    auto.cov.lv.x = TRUE,
                                    auto.efa = TRUE,
                                    auto.th = TRUE,
                                    auto.delta = TRUE,
                                    auto.cov.y = TRUE),
                     error = function(e) e),
            warning = function(w) {
              warnCond <<- w
              invokeRestart("muffleWarning")
            })
          attr(fit, "shinyCTTwarning") <- warnCond
          fit
        })

      #### warning and error counting and capturing ----
      errs <- sapply(fittedModelsWarns, inherits, what = "error")
      warns <- sapply(fittedModelsWarns, function(f) !is.null(attr(f, "shinyCTTwarning")))

      # A model that only warns is still usable - its fit was kept above - so only a
      # genuine error excludes it from goodModels. warnModels is purely informational now.
      goodModels <- modelsToTest[!errs]
      errModels <- modelsToTest[errs]
      warnModels <- modelsToTest[warns]

      #### the successive comparisons along the hierarchy ----
      succTable <- list()

      if (length(goodModels) > 1 && !identical(goodModels, c(teq = "teq", etp = "etp"))) {

        if ("teq" %in% goodModels) {

          succTable$teq <- do.call(
            lavaan::lavTestLRT,
            args = c(object = fittedModelsWarns[[goodModels[1]]],
                     ... = fittedModelsWarns[goodModels[-c(1, which(goodModels == "etp"))]]))

          rownames(succTable$teq) <- goodModels[which(goodModels != "etp")]
        }

        if ("etp" %in% goodModels) {
          succTable$etp <- do.call(
            lavaan::lavTestLRT,
            args = c(object = fittedModelsWarns[[goodModels[1 + (goodModels[1] == "teq")]]],
                     ... = fittedModelsWarns[goodModels[-c(1 + (goodModels[1] == "teq"),
                                                           which(goodModels == "teq"))]]))

          rownames(succTable$etp) <- goodModels[which(goodModels != "teq")]
        }

        if (!any(c("teq", "etp") %in% goodModels)) {
          succTable$teq <- do.call(
            lavaan::lavTestLRT,
            args = c(object = fittedModelsWarns[[goodModels[1]]],
                     ... = fittedModelsWarns[goodModels[-1]]))

          rownames(succTable$teq) <- goodModels
        }
      }

      #### everything the results tabs need from this pass ----
      # The settings are kept alongside the fits. The estimator can be changed after a
      # run, and the tables must go on reporting the one that actually produced them.
      list(
        groupName     = groupName,
        modelCodes    = modelCodes,
        fittedModels  = fittedModelsWarns,
        errs          = errs,
        warns         = warns,
        goodModels    = goodModels,
        errModels     = errModels,
        warnModels    = warnModels,
        comps         = comps,
        succTable     = succTable,
        estimator     = estimatorRV(),
        estimatorName = estimatorName(),
        missingMethod = ifelse(fimlRV(), "fiml", "listwise"),
        itemCols      = input$itemCols,
        groupCol      = input$groupCol,
        groups        = input$groups,
        groupValues   = groupValues,
        dataSource    = dataSource,
        dataName      = dataName,
        isSubset      = isSubset)
    }))

    # Land on the model comparison tests, the same as after the first run. On the first run
    # the sidebar does this by itself, because the block it has just revealed comes up
    # selected; on a later one the sidebar does not change, so say it here.
    shinydashboard::updateTabItems(session, "dataMenu", selected = "modelTests")

  },

  ## observeEvent input$goModels error handler ----
  # Anything goes wrong above -> show the message under the button, go back one stage.
  error = function(e) {
    # Back to "statistics", so the results entries disappear from the sidebar again.
    appStage("statistics")
    doMgRV(FALSE)

    # The lockout only ever disables, so switch these back on by hand. The multigroup box
    # only if there is a usable group column.
    shinyjs::enable("etaIntFree")
    if (isTRUE(validGroupsRV())) shinyjs::enable("doMg")

    output$goModelsError <- renderUI(
      tagList(
        br(),
        strong("The model tests failed:"),
        paste("There was an ERROR:", conditionMessage(e)) %>%
          HTML() %>%
          div(style = "color:red")))
  })) # observeEvent(input$goModels, {

  # Results ----
  # Everything the run produces is drawn by R/mod-ctt-results.R, once per pass: the whole
  # sample, and the group-wise fit if the user asked for one. The group-wise pages stay
  # blank until there is a group-wise fit to draw.
  cttResultsServer(
    "single",
    fit = reactive(req(modelFitsRV()$single)),
    sigLvl = sigLvlRV,
    rmseaCiLvl = rmseaCiLvlRV,
    goodColor = goodColor,
    badColor = badColor,
    neutrColor = neutrColor,
    textColor = textColor)

  cttResultsServer(
    "multigroup",
    fit = reactive(req(modelFitsRV()$multigroup)),
    sigLvl = sigLvlRV,
    rmseaCiLvl = rmseaCiLvlRV,
    goodColor = goodColor,
    badColor = badColor,
    neutrColor = neutrColor,
    textColor = textColor)
}
