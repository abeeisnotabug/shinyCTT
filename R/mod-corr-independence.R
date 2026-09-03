## The "Test on correlative independence:" box on the Correlations tab.
##
## It tests whether all the correlations between the items are zero, by fitting a lavaan
## model with every item's variance free and every covariance fixed to zero and reading off
## its chi-square test.

corrIndependenceUI <- function(id) {
  ns <- NS(id)

  shinydashboard::box(
    width = NULL,
    title = tr("stats.corrind.title"),
    radioButtons(
      ns("corrIndEst"),
      tr("stats.corrind.estimator.label"),
      choiceNames = list(tr("common.estimator.ml"), tr("common.estimator.mlr")),
      choiceValues = c("ML", "MLR"),
      selected = "ML"),
    numericInput(
      ns("corrIndSL"),
      tr("stats.corrind.siglvl.label"),
      value = 0.05,
      min = 0.001,
      max = 1,
      step = 0.001),
    htmlOutput(ns("result")))
}

## Arguments, all reactives:
##   data     : the chosen items, and the group column if there is one
##   itemCols : the names of the item columns
##   useFIML  : TRUE when the fit should use full information maximum likelihood
corrIndependenceServer <- function(id, data, itemCols, useFIML) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## the labels say so when the fit uses full information maximum likelihood ----
    observeEvent(useFIML(), {
      if (!useFIML()) return()

      updateRadioButtons(
        inputId = "corrIndEst",
        choiceNames = list(
          tr("common.estimator.fiml"),
          tr("common.estimator.fimlr")),
        choiceValues = c("ML", "MLR"))
    })

    ## the test result ----
    output$result <- renderUI({

      req(data())
      req(input$corrIndEst)

      dummyModel <- paste(
        sprintf("%s ~ 1", colnames(data()[, itemCols()])),
        collapse = "\n")

      corrIndRaw <- tryCatch(
        lavaan::cfa(
          model = dummyModel,
          data = data(),
          estimator = input$corrIndEst,
          missing = ifelse(useFIML(), "fiml", "listwise")),
        warning = function(w) w,
        error = function(e) e)

      ## result if (class(corrIndRaw)[1] == "lavaan") ----
      if (class(corrIndRaw)[1] == "lavaan") {

        corrInd <- unlist(extractFitIndices(corrIndRaw)[, c(2, 1, 3)])

        if (!is.na(input$corrIndSL) && input$corrIndSL < 1 && input$corrIndSL > 0) {

          tagList(
            strong(tr("stats.test.result")),

            sprintf(
              ifelse(
                corrInd[3] < input$corrIndSL,
                yes = tr("stats.corrind.result.dependent"),
                no = tr("stats.corrind.result.independent")),
              input$corrIndSL, # %s
              paste0(if (useFIML()) "FI", input$corrIndEst), # %s
              corrInd[1], # %.3f
              corrInd[2], # %i
              ifelse(corrInd[3] < 0.001, "< 0.001", sprintf("= %.3f", corrInd[3]))) %>%

              HTML() %>%
              p()

          ) # tagList

        } else {
          HTML(tr("stats.corrind.siglvl.invalid")) %>%
            div(style = "color:red")
        }

      } ## result if (class(corrIndRaw)[1] != "lavaan") ----
      else {
        tagList(
          strong(tr("stats.test.result")),
          paste(tr("stats.error.prefix"), corrIndRaw$message) %>%
            HTML() %>%
            div(style = "color:red"))
      }
    })
  })
}
