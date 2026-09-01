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
        title = "Normality tests:",
        numericInput(
          ns("mvnSL"),
          "Enter the significance level for the tests:",
          value = 0.05,
          min = 0.001,
          max = 1,
          step = 0.001)),

      shinydashboard::box(
        width = NULL,
        title = "Test on multivariate normality:",
        htmlOutput(ns("comment"))),

      shinydashboard::box(
        width = NULL,
        title = "Tests on univariate normality:",
        htmlOutput(ns("table")))
    ), # column

    column(
      width = 8,
      fluidRow(htmlOutput(ns("plotBox"))),
      fluidRow(shinydashboard::infoBox(
        title = "Hint:",
        subtitle = "For more extensive analyses on multivariate normality, load() the MVN package and open its shiny app via run_mvn_app()!",
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

    ## the multivariate test, in words and as a table ----
    output$comment <- renderUI({

      req(data())

      ### if the test ran ----
      if (is.data.frame(mvnResult()$multivariate_normality)) {

        mvnMV <- data.frame(Test = mvnResult()$multivariate_normality$Test,
                            Statistic = mvnResult()$multivariate_normality$Statistic,
                            p = suppressWarnings(as.numeric(mvnResult()$multivariate_normality$p.value)),
                            stringsAsFactors = F)

        mvnMV$p[is.na(mvnMV$p)] <- 0
        mvnMV$Signif. <- ifelse(mvnMV$p < input$mvnSL, "*", "")
        mvnMV$p <- ifelse(mvnMV$p < 0.001, "< 0.001", sprintf("%.3f", round(mvnMV$p, 3)))

        if ("*" %in% mvnMV$Signif.) {

          tagList(
            sprintf("At least one of the hypotheses that Mardia's Skewness statistic
                      or Mardias' Kurtosis statistic matches one of a
                      normal distribution has to be discarded on a significance
                      level of %s. Test result:", input$mvnSL),
            HTML(makeKable(mvnMV, bootstrap_options = "basic")),
            HTML("It is thus recommended to continue with the <b>Robust Maximum Likelihood (MLR)</b> estimator."))

        } else {

          tagList(
            sprintf("The hypotheses that Mardia's Skewness statistic
                      and Mardias' Kurtosis statistic match those of a
                      normal distribution can be maintained on a significance
                      level of %s. Test result:", input$mvnSL),
            HTML(makeKable(mvnMV, bootstrap_options = "basic")),
            HTML("It is thus recommended to continue with the <b>Maximum Likelihood (ML)</b> estimator."))
        }
      } ### if it did not ----
      else {
        paste("There was an ERROR/WARNING:", mvnResult()$message) %>%
          HTML() %>%
          div(style = "color:red")
      }
    })

    ## the item-by-item tests ----
    output$table <- renderUI({

      req(data())

      if (class(mvnResult())[1] == "mvn") {

        mvnUV <- data.frame(Test = mvnResult()$univariate_normality$Test,
                            Item = mvnResult()$univariate_normality$Variable,
                            Statistic = mvnResult()$univariate_normality$Statistic,
                            p = suppressWarnings(as.numeric(mvnResult()$univariate_normality$p.value)),
                            stringsAsFactors = F)

        mvnUV$p[is.na(mvnUV$p)] <- 0
        mvnUV$Signif. <- ifelse(mvnUV$p < input$mvnSL, "*", "")
        mvnUV$p <- ifelse(mvnUV$p < 0.001, "< 0.001", sprintf("%.3f", round(mvnUV$p, 3)))

        HTML(makeKable(mvnUV, bootstrap_options = "basic"))

      } else {
        paste("There was an ERROR/WARNING:", mvnResult()$message) %>%
          HTML() %>%
          div(style = "color:red")
      }
    })

    ## the plot box ----
    output$plotBox <- renderUI({

      shinydashboard::box(
        width = 12,
        title = "Multivariate plot:",

        fluidRow(

          column(
            width = 4,
            selectInput(
              ns("mvnPlotType"),
              "Choose the type of Plot:",
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
                "Select item on the abscissa:",
                itemCols()),
              ns = ns)),
          column(
            width = 4,
            conditionalPanel(
              "input.mvnPlotType != 'qq'",
              selectInput(
                ns("mvnItemY"),
                "Select item on the ordinate:",
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
