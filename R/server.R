server <- function(input, output, session) {
  # Preparation ----

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




  # "ML" or "MLR", with "FI" in front when the fits use full information maximum
  # likelihood. Shown in table headers and legends.
  estimatorName <- reactive(paste0(if (subset$useFIML()) "FI", input$estimator))

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

    sprintf(tr("params.siglvl.hint"), sigLvlRV()) %>%
      div(style = "color:red")
  })

  output$rmseaCiLvlNote <- renderUI({
    if (rmseaCiLvlUsable()) return(NULL)

    sprintf(tr("params.rmsea.ci.hint"), rmseaCiLvlRV()) %>%
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
  stageControls <- list(statistics = c("doMg", "etaIntFree"))

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
    groupLevels <- sort(unique(subset$data()[, subset$groupCol()]))
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
    if (any(vapply(notifications$notList, grepl, logical(1), pattern = "danger"))) {
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

  # dataSelectionTab ----
  # Step 1 lives in R/mod-data-source.R. It hands back the data it loaded and the copy taken
  # when Select was pressed.
  dataSource <- dataSourceServer(
    "dataSource",
    notifications = notifications,
    frozen = reactive(atLeastStage(appStage(), "subset")))

  # Pressing Select is the only thing that fills dataSource$chosen(), so this is where
  # step 1 ends and step 2 begins.
  observeEvent(dataSource$chosen(), appStage("subset"))

  # subsetSelectionTab ----
  # Step 2 lives in R/mod-data-subset.R. It hands back the six answers the rest of the app
  # works from.
  subset <- dataSubsetServer(
    "subset",
    chosenData = dataSource$chosen,
    notifications = notifications,
    frozen = reactive(atLeastStage(appStage(), "statistics")))

  # Pressing Select is the only thing that fills subset$data(), so this is where step 2
  # ends and step 3 begins. The three controls switched on here live on later tabs, which
  # is why they are not the subset box's own business.
  observeEvent(subset$data(), {
    appStage("statistics")

    if (isTRUE(subset$hasGroups())) {
      shinyjs::enable("doMg")

      updateCheckboxInput(
        session,
        "doMg",
        value = TRUE)
    }

    # The two boxes on the Correlations tab relabel their own controls; this is the one on
    # the Testing Parameters tab.
    if (subset$useFIML()) {
      updateRadioButtons(
        inputId = "estimator",
        choiceNames = list(
          tr("common.estimator.fiml"),
          tr("common.estimator.fimlr")),
        choiceValues = c("ML", "MLR"))
    }
  })

  ## how many items are ticked, for the model grid ----
  # ui.R draws the grid, so its conditions cannot see input$itemCols any more - that tick
  # box lives inside the subset box and its id carries that box's name. The count goes out
  # as a value instead. suspendWhenHidden = FALSE because the grid's conditions have to be
  # answerable while the Testing Parameters tab is still hidden.
  output$nItemsChosen <- reactive(length(subset$itemCols()))
  outputOptions(output, "nItemsChosen", suspendWhenHidden = FALSE)

  ## keep the model selection in step with the item count ----
  # The checkboxes are on the Testing Parameters tab, so this is the app's job rather than
  # the subset box's.
  observeEvent(subset$itemCols(), {
    req(identical(appStage(), "subset"))

    # TRUE for each model the current item count is enough to test.
    enoughItems <- minItems <= length(subset$itemCols())

    for (thisModel in models) {
      updateCheckboxInput(session, thisModel, value = unname(enoughItems[thisModel]))
    }

    for (thisComp in possComps) {
      updateCheckboxInput(
        session,
        thisComp,
        value = enoughItems[substr(thisComp, 1, 3)] && enoughItems[substr(thisComp, 4, 6)])
    }
  })

  # statisticsTab ----
  ## statisticsTab descriptive statistics ----
  # The whole box and both tables live in R/mod-descriptives.R.
  descriptivesServer(
    "descriptives",
    data = subset$data,
    itemCols = reactive(subset$itemCols()),
    groupCol = reactive(subset$groupCol()),
    hasGroups = subset$hasGroups)

  ## statisticsTab histogram ----
  # The whole box, its controls and both plots live in R/mod-histogram.R.
  histogramServer(
    "histogram",
    data = subset$data,
    itemCols = reactive(subset$itemCols()),
    groupCol = reactive(subset$groupCol()),
    hasGroups = subset$hasGroups,
    groupColors = groupColors)

  ## statisticsTab covariance matrix ----
  # The whole box and both tables live in R/mod-covmatrix.R.
  covMatrixServer(
    "covmatrix",
    data = subset$data,
    itemCols = reactive(subset$itemCols()),
    groupCol = reactive(subset$groupCol()),
    hasGroups = subset$hasGroups)

  # corrTab ----
  ## corrTab test on correlative independence ----
  # The whole box, its two controls and the test live in R/mod-corr-independence.R.
  corrIndependenceServer(
    "corrIndependence",
    data = subset$data,
    itemCols = reactive(subset$itemCols()),
    useFIML = subset$useFIML)

  ## corrTab scatter plot ----
  # The whole box, its controls and both plots live in R/mod-scatter.R.
  scatterServer(
    "scatter",
    data = subset$data,
    itemCols = reactive(subset$itemCols()),
    groupCol = reactive(subset$groupCol()),
    hasGroups = subset$hasGroups,
    groupColors = groupColors)

  ## corrTab correlation table ----
  # The whole box, its two controls and the table live in R/mod-corr-table.R.
  corrTableServer(
    "corrTable",
    data = subset$data,
    itemCols = reactive(subset$itemCols()),
    groupCol = reactive(subset$groupCol()),
    hasGroups = subset$hasGroups,
    estimatorName = estimatorName,
    sigLvl = sigLvlRV,
    useFIML = subset$useFIML)

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
      label = paste0(tr("params.go.button"), if (refitPending()) "*"))
  })

  output$refitPendingNote <- renderUI({
    if (!refitPending()) return(NULL)

    tagList(
      br(),
      sprintf(tr("params.refit.pending"),
              paste0("<b>", tr("params.go.button"), "*</b>")) %>%
        HTML() %>%
        div(style = "color:orange"))
  })

  # mvnTab ----
  # The whole tab lives in R/mod-mvn.R. It reports which estimator the normality test points
  # to; moving the radio buttons and saying so is done here.
  recommendedEstimator <- mvnServer(
    "mvn",
    data = subset$data,
    itemCols = reactive(subset$itemCols()))

  ## act on what the normality test found ----
  # An observer, not an output, so the test runs as soon as the data is ready rather than
  # waiting for the tab to be opened.
  observeEvent(recommendedEstimator(), {

    updateRadioButtons(session, "estimator", selected = recommendedEstimator())

    notifications$notList$estUpdate <- shinydashboard::notificationItem(
      text = tr("params.estimator.updated"),
      icon = icon("wrench"),
      status = "warning")

    showNotification(
      ui = tr("params.estimator.updated"),
      duration = 5,
      id = "estUpdateNot",
      type = "warning")

    notifications$notList$mvnApp <- shinydashboard::notificationItem(
      text = HTML(tr("stats.mvn.app.hint")),
      icon = icon("lightbulb"),
      status = "success")
  })

  ## the recommendation, next to the estimator buttons ----
  output$estimatorNote <- renderUI({

    req(recommendedEstimator())

    estimatorLongName <- c(ML = tr("common.estimator.ml"),
                           MLR = tr("common.estimator.mlr"))[recommendedEstimator()]

    sprintf(tr("params.estimator.mvn.note"),
            estimatorLongName,
            paste0("<i>", tr("stats.nav"), "</i>"),
            paste0("<i>", tr("stats.mvn.title"), "</i>")) %>%
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

    modelsToTest <- models[vapply(models, function(thisModel) input[[thisModel]], logical(1))]
    comps <- possComps[vapply(possComps, function(thisComp) input[[thisComp]], logical(1))]


    # TRUE when the user left some of the groups out, so the exported script has to
    # subset the data before fitting.
    isSubset <- (
      subset$hasGroups() &&
        (length(unique(subset$data()[, subset$groupCol()])) <
           length(unique(dataSource$raw()[, subset$groupCol()]))))

    # Which group each row belongs to, for the predicted factor scores. FALSE when the
    # user chose no group column, because then the data has no such column to read.
    groupValues <- if (subset$groupCol() == "noGroupSelected") {
      FALSE
    } else {
      subset$data()[, subset$groupCol()]
    }

    ## test the models! ----
    # One pass over the whole sample, plus one fitting the groups separately if the user
    # asked for that.
    passes <- list(single = FALSE)
    if (isTRUE(input$doMg)) passes$multigroup <- subset$groupCol()

    modelFitsRV(lapply(passes, function(groupName) {

      ### try fitting and capture warning and error messages ----
      modelCodes <- makeModelCodes(inputData = subset$data(),
                                              itemCols = subset$itemCols(),
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
                                    data = subset$data(),
                                    meanstructure = TRUE,
                                    group = if (isFALSE(groupName)) NULL else groupName,
                                    group.equal = if (isFALSE(groupName)) NULL else c("loadings", "intercepts"),
                                    estimator = estimatorRV(),
                                    missing = ifelse(subset$useFIML(), "fiml", "listwise"),
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
      errs <- vapply(fittedModelsWarns, inherits, logical(1), what = "error")
      warns <- vapply(fittedModelsWarns, function(f) !is.null(attr(f, "shinyCTTwarning")), logical(1))

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
        missingMethod = ifelse(subset$useFIML(), "fiml", "listwise"),
        itemCols      = subset$itemCols(),
        groupCol      = subset$groupCol(),
        groups        = subset$groups(),
        groupValues   = groupValues,
        dataSource    = dataSource$descriptor(),
        dataName      = dataSource$name(),
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
    if (isTRUE(subset$hasGroups())) shinyjs::enable("doMg")

    output$goModelsError <- renderUI(
      tagList(
        br(),
        strong(tr("params.error.tests.failed")),
        paste(tr("params.error.prefix"), conditionMessage(e)) %>%
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
    rmseaCiLvl = rmseaCiLvlRV)

  cttResultsServer(
    "multigroup",
    fit = reactive(req(modelFitsRV()$multigroup)),
    sigLvl = sigLvlRV,
    rmseaCiLvl = rmseaCiLvlRV)
}
