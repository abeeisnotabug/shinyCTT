## The "4. Testing Parameters" tab: every setting the run needs, and the button.
##
## Left column: how the models are fitted (estimator, mean structure, multigroup) and what
## the tables show (the significance level and the confidence level of the RMSEA interval).
## Right column: the 5x5 grid saying which models to fit and which pairs to compare.
##
## The box hands all of that back and fits nothing. The fitting stays in server.R, because
## it reads steps 1 and 2 as much as it reads this tab.
##
## Like mod-ctt-results.R, this one knows the five models exist and asks cttModelFamily()
## for them itself.

testingParamsUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    ## left column ----
    column(
      width = 5,

      ### left column, how the models are fitted ----
      cttCard(
        title = tr("params.fit.box.title"),
        fluidRow(

          column(
            width = 6,
            radioButtons(
              ns("estimator"),
              tr("params.estimator.label"),
              choiceNames = list(
                tr("common.estimator.ml"),
                tr("common.estimator.mlr")),
              choiceValues = c("ML", "MLR"),
              selected = "ML")),

          column(
            width = 6,
            radioButtons(
              ns("etaIntFree"),
              tr("params.parameterization.label"),
              choiceNames = list(
                HTML(sprintf(tr("params.parameterization.mean"),
                             tr("sym.mu.eta"))),
                HTML(sprintf(tr("params.parameterization.intercept"),
                             tr("sym.alpha.1")))),
              choiceValues = c(FALSE, TRUE)))
        ), # fluidRow

        # Full width rather than under the estimator buttons: in half a column this
        # is four words per line.
        htmlOutput(ns("estimatorNote")),

        hr(),

        shinyjs::disabled(
          checkboxInput(
            ns("doMg"),
            tr("params.multigroup.checkbox"),
            value = FALSE))
      ), # card

      ### left column, what the tables show ----
      # Stacked rather than side by side: the two labels are different lengths, and in a
      # column this narrow they would sit at different heights and look cramped.
      cttCard(
        title = tr("params.tables.box.title"),
        helpText(tr("params.tables.hint")),

        numericInput(
          ns("sigLvl"),
          tr("params.siglvl.label"),
          value = 0.05,
          min = 0.001,
          max = 1,
          step = 0.001),
        htmlOutput(ns("sigLvlNote")),

        # A confidence level, not a significance level, and set on its own. 0.90 is the
        # interval lavaan reports by default.
        numericInput(
          ns("rmseaCiLvl"),
          tr("params.rmsea.ci.label"),
          value = 0.90,
          min = 0.5,
          max = 0.999,
          step = 0.01),
        htmlOutput(ns("rmseaCiLvlNote"))
      ), # card

      ### left column, the button ----
      cttCard(
        actionButton(ns("goModels"), tr("params.go.button"), width = "100%"),
        htmlOutput(ns("goModelsError")),
        htmlOutput(ns("refitPendingNote")))
    ), # column

    ## right column, the model grid ----
    column(
      width = 7,
      cttCard(
        title = tr("params.grid.box.title"),
        comparisonGrid(cttModelFamily(), ns)))
  ) # fluidRow
}

## Arguments:
##   nItems       : reactive, how many item columns step 2 has ticked
##   subsetChosen : reactive, filled when step 2's Select is pressed. Read as a signal only:
##                  it is what switches the multigroup box on and relabels the estimator
##                  buttons at the right moment.
##   hasGroups    : reactive, TRUE when the group column gives usable groups
##   useFIML      : reactive, TRUE when the fits should use full information maximum likelihood
##   recommended  : reactive, "ML" or "MLR" from the normality test, NULL until it has run
##   modelFits    : reactive, what the last run produced. NULL before the first one.
##   fitError     : reactive, the message from a run that failed, NULL otherwise
##   notifications: the app's notification list, the one behind the bell in the header
##   frozen       : reactive, TRUE once the models have been run
##
## Returns a list of reactives:
##   estimator, etaIntFree, sigLvl, rmseaCiLvl, doMg : what the controls say
##   modelsToTest, comps : which models are ticked, and which comparisons
##   goModels            : the button. An observer on it is how the app knows to fit.
testingParamsServer <- function(id, nItems, subsetChosen, hasGroups, useFIML, recommended,
                                modelFits, fitError, notifications, frozen) {
  moduleServer(id, function(input, output, session) {

    family <- cttModelFamily()
    models <- family$names
    possComps <- family$comparable

    ## the two display settings ----
    # Neither is used to fit anything, so both stay live after a run and the tables follow
    # them. An empty box, or a number out of range, is simply not taken: the tables keep
    # the last usable value and a red note appears under the box. The box itself is never
    # written back to (see GOTCHAS.md).
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

      sprintf(tr("params.siglvl.hint"), sigLvlRV()) |>
        div(style = "color:red")
    })

    output$rmseaCiLvlNote <- renderUI({
      if (rmseaCiLvlUsable()) return(NULL)

      sprintf(tr("params.rmsea.ci.hint"), rmseaCiLvlRV()) |>
        div(style = "color:red")
    })

    ## the controls step 2 unlocks ----
    # Both belong to this box, but neither can be set until step 2 has been answered.
    observeEvent(subsetChosen(), {

      if (isTRUE(hasGroups())) {
        shinyjs::enable("doMg")
        updateCheckboxInput(session, "doMg", value = TRUE)
      }

      # The two boxes on the Correlations tab relabel their own controls; this is the one
      # on this tab.
      if (useFIML()) {
        updateRadioButtons(
          session,
          inputId = "estimator",
          choiceNames = list(
            tr("common.estimator.fiml"),
            tr("common.estimator.fimlr")),
          choiceValues = c("ML", "MLR"))
      }
    })

    ## how many items are ticked, for the grid ----
    # The grid's conditions cannot see step 2's tick boxes, whose ids carry that box's name
    # (see GOTCHAS.md). The count goes out as a value instead. suspendWhenHidden = FALSE
    # because those conditions have to be answerable while this tab is still hidden.
    output$nItemsChosen <- reactive(nItems())
    outputOptions(output, "nItemsChosen", suspendWhenHidden = FALSE)

    ## keep the model selection in step with the item count ----
    # !frozen(), so a run cannot be followed by the grid re-ticking itself.
    observeEvent(nItems(), {
      req(!frozen())

      # TRUE for each model the current item count is enough to test.
      enoughItems <- family$minItems <= nItems()

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

    ## what the normality test found ----
    # An observer and not an output, so the test is acted on as soon as the data is ready
    # rather than waiting for the normality tab to be opened. mod-mvn.R reports; this moves
    # the radio buttons and says so.
    observeEvent(recommended(), {

      updateRadioButtons(session, "estimator", selected = recommended())

      notifications$notList$estUpdate <- list(
        text = tr("params.estimator.updated"),
        icon = "wrench",
        status = "warning")

      showNotification(
        ui = tr("params.estimator.updated"),
        duration = 5,
        id = "estUpdateNot",
        type = "warning")

      notifications$notList$mvnApp <- list(
        text = HTML(tr("stats.mvn.app.hint")),
        icon = "lightbulb",
        status = "success")
    })

    output$estimatorNote <- renderUI({

      req(recommended())

      estimatorLongName <- c(ML = tr("common.estimator.ml"),
                             MLR = tr("common.estimator.mlr"))[recommended()]

      sprintf(tr("params.estimator.mvn.note"),
              estimatorLongName,
              paste0("<i>", tr("stats.nav"), "</i>"),
              paste0("<i>", tr("stats.mvn.title"), "</i>")) |>
        HTML() |>
        div(style = "color:orange; font-size: 90%")
    })

    ## the button ----
    # TRUE when the models on screen were fitted with a different estimator from the one
    # now chosen, so they no longer match it.
    refitPending <- reactive(
      !is.null(modelFits()) &&
        !identical(modelFits()$single$estimator, input$estimator))

    # Live before the first run, and after it only while a refit would change something.
    # Pressing it with the same settings would give the same results, so it is switched off.
    observe({
      if (is.null(modelFits()) || refitPending()) {
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
                paste0("<b>", tr("params.go.button"), "*</b>")) |>
          HTML() |>
          div(style = "color:orange"))
    })

    ## a run that failed ----
    # server.R catches it and puts the message here; the red box and switching the two
    # controls back on are this box's own business. The lockout below only ever disables.
    observeEvent(fitError(), {
      req(fitError())

      shinyjs::enable("etaIntFree")
      if (isTRUE(hasGroups())) shinyjs::enable("doMg")
    })

    output$goModelsError <- renderUI({
      if (is.null(fitError())) return(NULL)

      tagList(
        br(),
        strong(tr("params.error.tests.failed")),
        paste(tr("params.error.prefix"), fitError()) |>
          HTML() |>
          div(style = "color:red"))
    })

    ## freeze this box's controls once the models have been run ----
    observeEvent(frozen(), {
      req(frozen())

      # shinyjs adds this box's name to the id itself (see GOTCHAS.md), so these are plain.
      # The two display settings and the button stay usable after a run, so they are not
      # in this list.
      for (controlId in c("doMg", "etaIntFree")) shinyjs::disable(controlId)
    })

    ## what the rest of the app gets back ----
    list(
      estimator = reactive(input$estimator),
      etaIntFree = reactive(input$etaIntFree),
      sigLvl = sigLvlRV,
      rmseaCiLvl = rmseaCiLvlRV,
      doMg = reactive(input$doMg),

      modelsToTest = reactive(
        models[vapply(models, function(thisModel) input[[thisModel]], logical(1))]),

      comps = reactive(
        possComps[vapply(possComps, function(thisComp) input[[thisComp]], logical(1))]),

      goModels = reactive(input$goModels))
  })
}
