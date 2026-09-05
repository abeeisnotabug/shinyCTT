server <- function(input, output, session) {
  ## Language ----
  # Which language this visitor is being shown. It is read out of the address the browser
  # asked for (...?lang=de) and kept in the session, so two people using the app at the
  # same time can be reading it in different languages. tr() looks here for every piece of
  # text rendered while the app runs; the page itself was already built by ui().
  #
  # isolate() because clientData is a reactive value and this line is not inside a reactive
  # block - the address does not change while the page is open.
  #
  # What is stored is the resolved language, not what the address literally said. That is
  # what stops the observer below reloading the page once on every first visit: the chooser
  # reports "en", so "en" is what this has to hold for the two to match.
  session$userData$lang <- resolveLanguage(
    isolate(parseQueryString(session$clientData$url_search)$lang))

  # Picking a language puts ?lang= into the address and reloads, which runs ui() again and
  # builds the whole page in the new language. The req() stops it firing when the chooser
  # merely reports the language already in use, which it does once on every page load.
  observeEvent(input$language, {
    req(!identical(input$language, session$userData$lang))

    updateQueryString(paste0("?lang=", input$language), mode = "push")
    session$reload()
  })

  ## Reactive values ----
  # Each entry is list(text = , icon = , status = ). The boxes put those three pieces in
  # the list; output$infoMenu below writes the markup, in one place.
  notifications <- reactiveValues(notList = list())




  # "ML" or "MLR", with "FI" in front when the fits use full information maximum
  # likelihood. Shown in table headers and legends.
  estimatorName <- reactive(paste0(if (subset$useFIML()) "FI", params$estimator()))

  # The fitted models, written by "Test the models" and read by everything on the results
  # tabs. NULL until the button has been pressed.
  modelFitsRV <- reactiveVal(NULL)

  # What a failed run has to say, and nothing when the last one worked. The Testing
  # Parameters box draws it under its button.
  fitErrorRV <- reactiveVal(NULL)

  ## Workflow stage ----

  appStage <- reactiveVal("data")

  # Frozen at the transition to "results" so the sidebar re-renders only on stage changes.
  doMgRV <- reactiveVal(FALSE)

  # The stage does one thing here now. Each of the three boxes with controls freezes its
  # own, from a `frozen` reactive it is handed below.
  observeEvent(appStage(), {

    # The block a stage reveals is the one the user is moved onto. sidebarGroups() paints
    # that entry; this is what actually shows the panel.
    bslib::nav_select("dataMenu", selected = stageTabs[[appStage()]])
  })

  ## Group colours ----
  # ggplot2's default discrete palette, but pinned to the group *by name*: a discrete
  # scale hands out its palette to whichever levels are still in the data, so
  # de-selecting a group in a plot tab used to recolour the ones that remain. The light
  # variants - the same colours mixed 40% toward white - are for the density curves,
  # which are drawn on top of bars in the solid colour.
  groupColors <- reactive(
    groupPalette(sort(unique(subset$data()[, subset$groupCol()]))))

  ## Notifications ----
  # The bell in the green bar. There is no bslib dropdownMenu(), so this is Bootstrap 5's
  # own dropdown markup: a link that opens the list beneath it.
  output$infoMenu <- renderUI({

    entries <- notifications$notList

    # Nothing to report -> the bell alone, with no list to open and no count on it.
    if (length(entries) == 0)
      return(tags$span(class = "cttBell", icon("bell")))

    anyDanger <- any(vapply(
      entries,
      function(entry) identical(entry$status, "danger"),
      logical(1)))

    # unname(), because a named list handed to a tag turns its names into HTML attributes
    # instead of children and the entries vanish.
    items <- unname(lapply(entries, function(entry) tags$li(
      tags$span(
        class = "dropdown-item",
        tags$span(class = paste0("text-", entry$status), icon(entry$icon)),
        " ",
        entry$text))))

    tags$div(
      class = "dropdown",

      tags$a(
        class = "cttBell dropdown-toggle",
        href = "#",
        `data-bs-toggle` = "dropdown",
        icon("bell"),
        tags$span(
          class = paste("badge rounded-pill", if (anyDanger) "bg-danger" else "bg-primary"),
          length(entries))),

      tags$ul(class = "dropdown-menu dropdown-menu-end cttNotifications", items))
  })

  output$dataMenuOut <- renderUI(sidebarGroups(appStage(), doMgRV()))

  ## which tab the menu is asking for ----
  # One link per name in tabNames, all of them built by sidebarGroups(). A click switches
  # the panel and moves the highlight; the reload link switches nothing and reloads the
  # page instead.
  lapply(tabNames, function(thisTab) {
    observeEvent(input[[paste0("nav_", thisTab)]], {

      if (identical(thisTab, "reloadTab")) {
        shinyjs::runjs("location.reload()")
        return()
      }

      bslib::nav_select("dataMenu", selected = thisTab)

      shinyjs::removeClass(selector = ".cttMenu a", class = "cttSelected")
      shinyjs::addClass(paste0("nav_", thisTab), "cttSelected")
    })
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
  # ends and step 3 begins. The controls it unlocks are on the Testing Parameters tab and
  # are that box's own business; it is handed subset$data() as the signal.
  observeEvent(subset$data(), appStage("statistics"))

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
    sigLvl = params$sigLvl,
    useFIML = subset$useFIML)

  # mvnTab ----
  # The whole tab lives in R/mod-mvn.R. It reports which estimator the normality test points
  # to and nothing more; the Testing Parameters box is handed that and moves its own radio
  # buttons.
  recommendedEstimator <- mvnServer(
    "mvn",
    data = subset$data,
    itemCols = reactive(subset$itemCols()))

  # testParamTab ----
  # The whole tab lives in R/mod-testing-params.R: the estimator, the mean structure, the
  # multigroup box, the two display settings, the 5x5 grid and the button. It fits nothing
  # and hands all of it back.
  params <- testingParamsServer(
    "params",
    nItems = reactive(length(subset$itemCols())),
    subsetChosen = subset$data,
    hasGroups = subset$hasGroups,
    useFIML = subset$useFIML,
    recommended = recommendedEstimator,
    modelFits = modelFitsRV,
    fitError = fitErrorRV,
    notifications = notifications,
    frozen = reactive(atLeastStage(appStage(), "results")))

  # the run ----
  # Pressing "Test the models" fits the models and stores them, and does nothing else.
  # Everything drawn from them is built under "Results" below and redraws on its own. The
  # button and every setting it reads are the Testing Parameters box's; the fitting is
  # here, because it reads steps 1 and 2 just as much.
  observeEvent(params$goModels(), tryCatch({
    fitErrorRV(NULL)

    # Whatever is on screen was fitted with the settings as they were before this press.
    modelFitsRV(NULL)

    # Freeze the multigroup choice, then reveal the results entries in the sidebar.
    doMgRV(isTRUE(params$doMg()))
    appStage("results")

    modelsToTest <- params$modelsToTest()
    comps <- params$comps()


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
    if (isTRUE(params$doMg())) passes$multigroup <- subset$groupCol()

    modelFitsRV(lapply(passes, function(groupName) {

      ### try fitting and capture warning and error messages ----
      modelCodes <- makeModelCodes(inputData = subset$data(),
                                              itemCols = subset$itemCols(),
                                              group = groupName,
                                              etaIntFree = as.logical(params$etaIntFree()))

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
                                    estimator = params$estimator(),
                                    missing = ifelse(subset$useFIML(), "fiml", "listwise"),
                                    int.ov.free = TRUE,
                                    int.lv.free = as.logical(params$etaIntFree()),
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
        estimator     = params$estimator(),
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
    bslib::nav_select("dataMenu", selected = "modelTests")

  },

  ## the run failed ----
  # Anything goes wrong above -> go back one stage and hand the message to the Testing
  # Parameters box, which draws it under its button and switches its own controls back on.
  error = function(e) {
    # Back to "statistics", so the results entries disappear from the sidebar again.
    appStage("statistics")
    doMgRV(FALSE)

    fitErrorRV(conditionMessage(e))
  })) # observeEvent(params$goModels(), {

  # Results ----
  # Everything the run produces is drawn by R/mod-ctt-results.R, once per pass: the whole
  # sample, and the group-wise fit if the user asked for one. The group-wise pages stay
  # blank until there is a group-wise fit to draw.
  cttResultsServer(
    "single",
    fit = reactive(req(modelFitsRV()$single)),
    sigLvl = params$sigLvl,
    rmseaCiLvl = params$rmseaCiLvl)

  cttResultsServer(
    "multigroup",
    fit = reactive(req(modelFitsRV()$multigroup)),
    sigLvl = params$sigLvl,
    rmseaCiLvl = params$rmseaCiLvl)
}
