## The "Test on correlative independence:" box on the Correlations tab.
##
## It tests whether all the correlations between the items are zero, by fitting a lavaan
## model with every item's variance free and every covariance fixed to zero and reading off
## its chi-square test.

corrIndependenceUI <- function(id) {
  ns <- NS(id)

  shinydashboard::box(
    width = NULL,
    title = tr("Test on correlative independence:"),
    radioButtons(
      ns("corrIndEst"),
      tr("Choose the estimator for this test:"),
      choiceNames = list(tr("Maximum Likelihood"), tr("Robust Maximum Likelihood")),
      choiceValues = c("ML", "MLR"),
      selected = "ML"),
    numericInput(
      ns("corrIndSL"),
      tr("Enter the significance level for this test:"),
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
          tr("(Full Information) Maximum Likelihood"),
          tr("Robust (Full Information) Maximum Likelihood")),
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
            strong(tr("Test result:")),

            sprintf(
              ifelse(
                corrInd[3] < input$corrIndSL,
                yes = tr("The hypothesis that all correlations are equal to zero has to be discarded on a significance level of %s (%s-&chi;&sup2; = %.3f, df = %i, p %s)."),
                no = tr("The hypothesis that all correlations are equal to zero can be maintained on a significance level of %s (%s-&chi;&sup2; = %.3f, df = %i, p %s).")),
              input$corrIndSL, # %s
              paste0(if (useFIML()) "FI", input$corrIndEst), # %s
              corrInd[1], # %.3f
              corrInd[2], # %i
              ifelse(corrInd[3] < 0.001, "< 0.001", sprintf("= %.3f", corrInd[3]))) %>%

              HTML() %>%
              p()

          ) # tagList

        } else {
          HTML(tr("Please enter a valid significance level")) %>%
            div(style = "color:red")
        }

      } ## result if (class(corrIndRaw)[1] != "lavaan") ----
      else {
        tagList(
          strong(tr("Test result:")),
          paste(tr("There was an ERROR/WARNING:"), corrIndRaw$message) %>%
            HTML() %>%
            div(style = "color:red"))
      }
    })
  })
}
