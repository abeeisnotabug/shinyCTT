## The "Test on Multivariate Normality" tab.
##
## It runs Mardia's test for multivariate normality and shows the result, the item-by-item
## tests, and a plot. It also *reports* which estimator that points to - it does not choose
## one. What to do with the recommendation is decided in server.R.

mvnUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,

      shinydashboard::box(
        width = NULL,
        title = tr("Normality tests:"),
        numericInput(
          ns("mvnSL"),
          tr("Enter the significance level for the tests:"),
          value = 0.05,
          min = 0.001,
          max = 1,
          step = 0.001)),

      shinydashboard::box(
        width = NULL,
        title = tr("Test on multivariate normality:"),
        uiOutput(ns("comment")),
        reactable::reactableOutput(ns("mvTable")),
        uiOutput(ns("recommendation"))),

      shinydashboard::box(
        width = NULL,
        title = tr("Tests on univariate normality:"),
        reactable::reactableOutput(ns("table")),
        uiOutput(ns("tableNote")))
    ), # column

    column(
      width = 8,
      fluidRow(htmlOutput(ns("plotBox"))),
      fluidRow(shinydashboard::infoBox(
        title = tr("Hint:"),
        subtitle = tr("For more extensive analyses on multivariate normality, load() the MVN package and open its shiny app via run_mvn_app()!"),
        icon = icon("lightbulb"),
        color = "green",
        width = 12,
        fill = TRUE)))
  ) # fluidRow
}

## Arguments, both reactives:
##   data     : the chosen items, and the group column if there is one
##   itemCols : the names of the item columns
##
## Returns a reactive holding "ML" or "MLR" - the estimator the test points to - or NULL
## when the test could not be run.
mvnServer <- function(id, data, itemCols) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## the test ----
    # A plain reactive, so the tab's three outputs share one run of it, and so server.R can
    # read the recommendation below without the tab ever having been opened.
    mvnResult <- reactive({
      req(data(), itemCols())

      tryCatch(
        MVN::mvn(stats::na.omit(data()[, itemCols()]),
                 mvn_test = "mardia"),
        warning = function(w) w,
        error = function(e) e)
    })

    ## what the test points to ----
    # MVN reports a p-value either as a number or as the string "<0.001", so both have to be
    # read. Either Mardia statistic coming out significant -> the robust estimator.
    recommendedEstimator <- reactive({
      req(input$mvnSL)

      if (!is.data.frame(mvnResult()$multivariate_normality)) return(NULL)

      pValues <- mvnResult()$multivariate_normality[, "p.value"]

      notNormal <- if (is.numeric(pValues)) {
        any(pValues < input$mvnSL)
      } else {
        any(pValues == "<0.001")
      }

      if (notNormal) "MLR" else "ML"
    })

    ## the two result tables ----
    # The numbers the two boxes show, built once each so the words around them and the
    # table itself can be separate outputs. Both give back NULL when the test did not run.
    #
    # The column names stay English here and are translated in the colDef()s below, so
    # nothing downstream has to look a column up by its translated name.
    multivariateTable <- reactive({
      req(input$mvnSL)

      if (!is.data.frame(mvnResult()$multivariate_normality)) return(NULL)

      mvnMV <- data.frame(Test = mvnResult()$multivariate_normality$Test,
                          Statistic = mvnResult()$multivariate_normality$Statistic,
                          p = suppressWarnings(as.numeric(mvnResult()$multivariate_normality$p.value)),
                          stringsAsFactors = FALSE)

      mvnMV$p[is.na(mvnMV$p)] <- 0
      mvnMV$Signif. <- ifelse(mvnMV$p < input$mvnSL, "*", "")
      mvnMV$p <- ifelse(mvnMV$p < 0.001, "< 0.001", sprintf("%.3f", round(mvnMV$p, 3)))
      mvnMV
    })

    univariateTable <- reactive({
      req(input$mvnSL)

      if (!identical(class(mvnResult())[1], "mvn")) return(NULL)

      mvnUV <- data.frame(Test = mvnResult()$univariate_normality$Test,
                          Item = mvnResult()$univariate_normality$Variable,
                          Statistic = mvnResult()$univariate_normality$Statistic,
                          p = suppressWarnings(as.numeric(mvnResult()$univariate_normality$p.value)),
                          stringsAsFactors = FALSE)

      mvnUV$p[is.na(mvnUV$p)] <- 0
      mvnUV$Signif. <- ifelse(mvnUV$p < input$mvnSL, "*", "")
      mvnUV$p <- ifelse(mvnUV$p < 0.001, "< 0.001", sprintf("%.3f", round(mvnUV$p, 3)))
      mvnUV
    })

    ## the multivariate test, in words and as a table ----
    # Three outputs where the box shows one thing: renderReactable can only give back a
    # table, so the sentence above the table and the sentence below it are their own
    # outputs. Each is guarded, so a failed test leaves only the red message.
    output$comment <- renderUI({
      req(data(), input$mvnSL)

      ### if the test did not run ----
      if (is.null(multivariateTable())) {
        paste(tr("There was an ERROR/WARNING:"), mvnResult()$message) %>%
          HTML() %>%
          div(style = "color:red")

      ### if it did ----
      } else if ("*" %in% multivariateTable()$Signif.) {
        sprintf(tr("At least one of the hypotheses that Mardia's Skewness statistic or Mardias' Kurtosis statistic matches one of a normal distribution has to be discarded on a significance level of %s. Test result:"), input$mvnSL)

      } else {
        sprintf(tr("The hypotheses that Mardia's Skewness statistic and Mardias' Kurtosis statistic match those of a normal distribution can be maintained on a significance level of %s. Test result:"), input$mvnSL)
      }
    })

    output$mvTable <- reactable::renderReactable({
      req(multivariateTable())

      reactable::reactable(
        multivariateTable(),
        # These two boxes are a third of the page wide, so the columns are given the room
        # their own contents need rather than reactable's even 100px each: "Mardia
        # Skewness" wrapped onto two lines while "Signif." was cut short. Measured in a
        # browser against the widest text in each column, plus the theme's cell padding.
        columns = list(
          Test = reactable::colDef(name = tr("Test"), minWidth = 127),
          Statistic = reactable::colDef(name = tr("Statistic"), minWidth = 67,
                                        format = reactable::colFormat(digits = 3, locales = "en-US")),
          p = reactable::colDef(name = tr("p"), minWidth = 64),
          `Signif.` = reactable::colDef(name = tr("Signif."), minWidth = 55)),
        sortable = FALSE,
        pagination = FALSE,
        compact = TRUE)
    })

    output$recommendation <- renderUI({
      req(multivariateTable())

      if ("*" %in% multivariateTable()$Signif.) {
        HTML(tr("It is thus recommended to continue with the <b>Robust Maximum Likelihood (MLR)</b> estimator."))
      } else {
        HTML(tr("It is thus recommended to continue with the <b>Maximum Likelihood (ML)</b> estimator."))
      }
    })

    ## the item-by-item tests ----
    # Same split again: the table when the test ran, the red message when it did not.
    output$table <- reactable::renderReactable({
      req(data(), univariateTable())

      reactable::reactable(
        univariateTable(),
        # Same measurement as the box above: "Anderson-Darling" needs the room, the
        # single-character p and the star in Signif. do not.
        columns = list(
          Test = reactable::colDef(name = tr("Test"), minWidth = 125),
          Item = reactable::colDef(name = tr("Item"), minWidth = 59),
          Statistic = reactable::colDef(name = tr("Statistic"), minWidth = 76,
                                        format = reactable::colFormat(digits = 3, locales = "en-US")),
          p = reactable::colDef(name = tr("p"), minWidth = 52),
          `Signif.` = reactable::colDef(name = tr("Signif."), minWidth = 55)),
        sortable = FALSE,
        pagination = FALSE,
        compact = TRUE)
    })

    output$tableNote <- renderUI({
      req(data(), input$mvnSL)

      if (is.null(univariateTable())) {
        paste(tr("There was an ERROR/WARNING:"), mvnResult()$message) %>%
          HTML() %>%
          div(style = "color:red")
      }
    })

    ## the plot box ----
    output$plotBox <- renderUI({

      shinydashboard::box(
        width = 12,
        title = tr("Multivariate plot:"),

        fluidRow(

          column(
            width = 4,
            # The three plot type labels are a selectInput()'s named choices, and
            # input$mvnPlotType is compared against their values below, so they are left
            # untranslated - see the translation report.
            selectInput(
              ns("mvnPlotType"),
              tr("Choose the type of Plot:"),
              choices = c(
                "Q-Q Plot (all items)" = "qq",
                "Perspective Plot" = "persp",
                "Contour Plot" = "contour"))),
          column(
            width = 4,
            conditionalPanel(
              "input.mvnPlotType != 'qq'",
              selectInput(
                ns("mvnItemX"),
                tr("Select item on the abscissa:"),
                itemCols()),
              ns = ns)),
          column(
            width = 4,
            conditionalPanel(
              "input.mvnPlotType != 'qq'",
              selectInput(
                ns("mvnItemY"),
                tr("Select item on the ordinate:"),
                itemCols(),
                selected = itemCols()[2]),
              ns = ns))

        ), # fluidRow

        plotOutput(ns("plot"))
      ) # box
    })

    ## the plot ----
    output$plot <- renderPlot({
      req(data(), input$mvnPlotType)
      if (input$mvnPlotType != "qq") req(input$mvnItemX, input$mvnItemY)

      userDataNAOmit <- stats::na.omit(data())

      if (input$mvnPlotType == "qq") {
        MVN::multivariate_diagnostic_plot(
          stats::na.omit(userDataNAOmit[, itemCols()]),
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

    recommendedEstimator
  })
}
