## The "Correlation table with confidence intervals:" box on the Correlations tab.
##
## Two UI entry points sharing one module id: corrTableControlsUI() places the two
## controls in one column, corrTableUI() places the table itself underneath.

corrTableControlsUI <- function(id) {
  ns <- NS(id)

  shinydashboard::box(
    width = NULL,
    title = tr("Correlation table with confidence intervals:"),
    shinyjs::hidden(
      radioButtons(
        ns("corrTabNA"),
        tr("Choose how to handle missing values:"),
        choiceNames = list(
          tr("Use pairwise complete observations"),
          tr("Use only complete observations")),
        choiceValues = c("pairwise.complete.obs", "complete.obs"),
        selected = "pairwise.complete.obs")),
    numericInput(
      ns("corrTabSL"),
      tr("Enter the significance level for the correlation tests:"),
      value = 0.05,
      min = 0.001,
      max = 1,
      step = 0.001))
}

corrTableUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("box"))
}

## Arguments, all reactives:
##   data          : the chosen items, and the group column if there is one
##   itemCols      : the names of the item columns
##   groupCol      : the name of the group column
##   hasGroups     : TRUE when the group column is usable
##   estimatorName : the estimator label shown in the legend, e.g. "ML" or "FIML"
##   sigLvl        : the significance level from the Statistics tab, used only in the legend
##   useFIML       : TRUE when the fit should use full information maximum likelihood
##
## The four colours are the app's, passed in the same way every make*Table() call takes
## them, so they are written down in one place only.
corrTableServer <- function(id, data, itemCols, groupCol, hasGroups, estimatorName, sigLvl,
                            useFIML, goodColor, badColor, neutrColor, textColor) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## the missing-value choice only matters when there are missing values ----
    # shinyjs puts the module's name in front of the id itself (see GOTCHAS.md), so this
    # is the plain id, not ns("corrTabNA").
    observeEvent(useFIML(), {
      if (useFIML()) shinyjs::show(id = "corrTabNA")
    })

    ## the box ----
    output$box <- renderUI({
      req(data())

      ## box create raw cor table and test for errors ----
      corrTableWithCIsRaw <- list(
        cor = tryCatch(
          stats::cor(data()[, itemCols()],
              use = input$corrTabNA),
          warning = function(w) NULL,
          error = function(e) NULL),
        test = tryCatch(
          corrplot::cor.mtest(data()[, itemCols()],
                              conf.level = (1 - input$corrTabSL)),
          warning = function(w) w,
          error = function(e) e))

      corrTableLegend <- tagList(

        cbind(
          kableExtra::cell_spec(
            tr("Legend:")),
          kableExtra::cell_spec(
            tr("Sig. pos."),
            color = textColor,
            background = goodColor),
          kableExtra::cell_spec(
            tr("Sig. neg."),
            color = textColor,
            background = badColor),
          kableExtra::cell_spec(
            tr("Not sig."),
            color = textColor,
            background = neutrColor)) %>%

          makeKable(
            position = "left") %>%
          HTML()

      ) # tagList

      ## box singleCorrTable if no errors: ----
      if (class(corrTableWithCIsRaw$test)[1] == "list") {

        singleCorrTable <- makeCorrTableWithCIs(
          rawTable = corrTableWithCIsRaw,
          goodColor,
          badColor,
          neutrColor,
          textColor,
          sigLvl = input$corrTabSL,
          itemCols = itemCols()) %>%

          makeKable(
            bootstrap_options = c("condensed", "striped"),
            bold_cols = 1) %>%
          HTML()

      } ## box singleCorrTable if errors: ----
      else {
        singleCorrTable <-
          paste(tr("There was an ERROR/WARNING:"), corrTableWithCIsRaw$test) %>%
          HTML() %>%
          div(style = "color:red")
      }

      ## box if groups ----
      if (hasGroups()) {

        groups <- unique(data()[, groupCol()])

        mgCorrTableList <- lapply(
          groups,
          function(group) {

            makeCorrTableWithCIs(

              rawTable = list(
                cor = suppressWarnings(stats::cor(
                  subset(
                    data()[, itemCols()],
                    data()[, groupCol()] == group),
                  use = input$corrTabNA)),
                test = corrplot::cor.mtest(
                  subset(
                    data()[, itemCols()],
                    data()[, groupCol()] == group),
                  conf.level = (1 - input$corrTabSL))),

              goodColor,
              badColor,
              neutrColor,
              textColor,
              sigLvl = input$corrTabSL,
              itemCols = itemCols())
        })

        # join each group corrTable
        mgCorrTable <- makeKable(
          do.call(rbind, mgCorrTableList),
          bootstrap_options = c("condensed", "striped"),
          bold_cols = 1)

        # add group headers
        groupRowHeaders <- sprintf(tr("Group: %s"), groups)

        for (i in 1:length(groups))
          mgCorrTable <- mgCorrTable %>%
            kableExtra::group_rows(
              group_label = groupRowHeaders[i],
              start_row = (i - 1) * length(itemCols()) * 2 + 1,
              end_row = i * length(itemCols()) * 2,
              label_row_css = "background-color: #666; color: #fff;")

        # assemble in tabBox
        shinydashboard::tabBox(
          width = 12,
          title = tr("Correlation table with confidence intervals:"),
          side = "right",

          tabPanel(
              tr("Overall"),
              singleCorrTable,
              br(),
              HTML(makeLegend("corrTable", estimatorName(), sigLvl(),
                              goodColor, badColor, neutrColor, textColor))),
          tabPanel(
              tr("Group-wise"),
              HTML(mgCorrTable),
              br(),
              HTML(makeLegend("corrTable", estimatorName(), sigLvl(),
                              goodColor, badColor, neutrColor, textColor)))

        ) # tabBox

      } ## box if no groups ----
      else {

        shinydashboard::box(
            width = 12,
            title = tr("Correlation table with confidence intervals:"),

            singleCorrTable,
            br(),
            corrTableLegend)
      }
    })
  })
}
