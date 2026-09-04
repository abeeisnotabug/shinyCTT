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
        title = tr("stats.mvn.normtests.title"),
        numericInput(
          ns("mvnSL"),
          tr("stats.mvn.siglvl.label"),
          value = 0.05,
          min = 0.001,
          max = 1,
          step = 0.001)),

      shinydashboard::box(
        width = NULL,
        title = tr("stats.mvn.title"),
        uiOutput(ns("comment")),
        reactable::reactableOutput(ns("mvTable")),
        uiOutput(ns("recommendation"))),

      shinydashboard::box(
        width = NULL,
        title = tr("stats.mvn.univ.title"),
        reactable::reactableOutput(ns("table")),
        uiOutput(ns("tableNote")))
    ), # column

    column(
      width = 8,
      fluidRow(htmlOutput(ns("plotBox"))),
      fluidRow(shinydashboard::infoBox(
        title = tr("stats.mvn.hint.label"),
        subtitle = tr("stats.mvn.app.hint"),
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
    # read the recommendation below without the tab ever having been opened. Both tests run
    # together, so one failure leaves one condition object for all three outputs to report.
    mvnResult <- reactive({
      req(data(), itemCols())

      items <- stats::na.omit(data()[, itemCols()])

      tryCatch(
        list(multivariate = mardiaTests(items),
             univariate = andersonDarlingTests(items)),
        warning = function(w) w,
        error = function(e) e)
    })

    ## what the test points to ----
    # Either Mardia statistic coming out significant -> the robust estimator.
    recommendedEstimator <- reactive({
      req(input$mvnSL)

      if (!is.data.frame(mvnResult()$multivariate)) return(NULL)

      if (any(mvnResult()$multivariate$p < input$mvnSL)) "MLR" else "ML"
    })

    ## the two result tables ----
    # The numbers the two boxes show, built once each so the words around them and the
    # table itself can be separate outputs. Both give back NULL when the test did not run.
    #
    # The column names stay English here and are translated in the colDef()s below, so
    # nothing downstream has to look a column up by its translated name.
    multivariateTable <- reactive({
      req(input$mvnSL)

      if (!is.data.frame(mvnResult()$multivariate)) return(NULL)

      mvnMV <- mvnResult()$multivariate
      mvnMV$Signif. <- ifelse(mvnMV$p < input$mvnSL, "*", "")
      mvnMV$p <- ifelse(mvnMV$p < 0.001, "< 0.001", sprintf("%.3f", round(mvnMV$p, 3)))
      mvnMV
    })

    univariateTable <- reactive({
      req(input$mvnSL)

      if (!is.data.frame(mvnResult()$univariate)) return(NULL)

      mvnUV <- mvnResult()$univariate
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
        paste(tr("stats.error.prefix"), mvnResult()$message) |>
          HTML() |>
          div(style = "color:red")

      ### if it did ----
      } else if ("*" %in% multivariateTable()$Signif.) {
        sprintf(tr("stats.mvn.result.nonnormal"), input$mvnSL)

      } else {
        sprintf(tr("stats.mvn.result.normal"), input$mvnSL)
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
          Test = reactable::colDef(name = tr("stats.mvn.col.test"), minWidth = 127),
          Statistic = reactable::colDef(name = tr("stats.mvn.col.statistic"), minWidth = 67,
                                        format = reactable::colFormat(digits = 3, locales = "en-US")),
          p = reactable::colDef(name = tr("common.col.p"), minWidth = 64),
          `Signif.` = reactable::colDef(name = tr("stats.mvn.col.signif"), minWidth = 55)),
        resizable = getOption("shinyCTT.resizable"),
        sortable = FALSE,
        pagination = FALSE,
        compact = TRUE)
    })

    output$recommendation <- renderUI({
      req(multivariateTable())

      estimator <- if ("*" %in% multivariateTable()$Signif.) "MLR" else "ML"

      longName <- c(ML = tr("common.estimator.ml"),
                    MLR = tr("common.estimator.mlr"))[estimator]

      HTML(sprintf(tr("stats.mvn.recommend"),
                   paste0("<b>", longName, " (", estimator, ")</b>")))
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
          Test = reactable::colDef(name = tr("stats.mvn.col.test"), minWidth = 125),
          Item = reactable::colDef(name = tr("common.col.item"), minWidth = 59),
          Statistic = reactable::colDef(name = tr("stats.mvn.col.statistic"), minWidth = 76,
                                        format = reactable::colFormat(digits = 3, locales = "en-US")),
          p = reactable::colDef(name = tr("common.col.p"), minWidth = 52),
          `Signif.` = reactable::colDef(name = tr("stats.mvn.col.signif"), minWidth = 55)),
        resizable = getOption("shinyCTT.resizable"),
        sortable = FALSE,
        pagination = FALSE,
        compact = TRUE)
    })

    output$tableNote <- renderUI({
      req(data(), input$mvnSL)

      if (is.null(univariateTable())) {
        paste(tr("stats.error.prefix"), mvnResult()$message) |>
          HTML() |>
          div(style = "color:red")
      }
    })

    ## the plot box ----
    output$plotBox <- renderUI({

      shinydashboard::box(
        width = 12,
        title = tr("stats.mvn.plot.title"),

        fluidRow(

          column(
            width = 4,
            selectInput(
              ns("mvnPlotType"),
              tr("stats.mvn.plottype.label"),
              choices = stats::setNames(
                c("qq", "persp", "contour"),
                c(tr("stats.mvn.plottype.qq"),
                  tr("stats.mvn.plottype.persp"),
                  tr("stats.mvn.plottype.contour"))))),
          column(
            width = 4,
            conditionalPanel(
              "input.mvnPlotType != 'qq'",
              selectInput(
                ns("mvnItemX"),
                tr("stats.select.abscissa"),
                itemCols()),
              ns = ns)),
          column(
            width = 4,
            conditionalPanel(
              "input.mvnPlotType != 'qq'",
              selectInput(
                ns("mvnItemY"),
                tr("stats.select.ordinate"),
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
        items <- stats::na.omit(userDataNAOmit[, itemCols()])
        # each case's distance from the mean, against where that distance would fall if the
        # items really were multivariate normal
        distances <- sort(stats::mahalanobis(items, colMeans(items), stats::cov(items)))
        expected <- stats::qchisq(stats::ppoints(nrow(items)), df = ncol(items))

        graphics::plot(expected, distances,
                       xlab = tr("stats.mvn.qq.abscissa"),
                       ylab = tr("stats.mvn.qq.ordinate"),
                       pch = 16, col = fuColors()$mark)
        graphics::abline(a = 0, b = 1)

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
