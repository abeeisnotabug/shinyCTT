## The covariance matrix box on the Statistics tab.
##
## With a group column it is a tabBox with an "Overall" and a "Group-wise" tab; without one
## it is a plain box holding just the overall table.

covMatrixUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("box"))
}

## Arguments, all reactives:
##   data      : the chosen items, and the group column if there is one
##   itemCols  : the names of the item columns
##   groupCol  : the name of the group column
##   hasGroups : TRUE when the group column is usable
covMatrixServer <- function(id, data, itemCols, groupCol, hasGroups) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## the box ----
    output$box <- renderUI({
      req(data())

      table <- stats::cov(data()[, itemCols()], use = "pairwise.complete.obs")
      table[upper.tri(table)] <- NA

      ## box if (hasGroups()) ----
      if (hasGroups()) {
        groups <- unique(data()[, groupCol()])

        mgCovMatList <- lapply(
          groups,
          function(group) {
            stats::cov(
              subset(
                data()[, itemCols()],
                data()[, groupCol()] == group),
              use = "pairwise.complete.obs")
          })

        for (i in 1:length(mgCovMatList))
          mgCovMatList[[i]][upper.tri(mgCovMatList[[i]])] <- NA

        mgCovMatTable <- makeKable(do.call(rbind, mgCovMatList),
                                              bold_cols = 1)

        groupRowHeaders <- sprintf(
          tr("Group: %s (n = %i)"),
          groups,
          c(table(data()[, groupCol()]))[as.character(groups)])

        for (i in 1:length(groups))
          mgCovMatTable <- mgCovMatTable %>%
            kableExtra::group_rows(
              group_label = groupRowHeaders[i],
              start_row = (i - 1) * length(itemCols()) + 1,
              end_row = i * length(itemCols()),
              label_row_css = "background-color: #666; color: #fff;")

        # output if groups
        shinydashboard::tabBox(
          width = 12,
          title = tr("Covariance matrix:"),
          side = "right",

          tabPanel(
            title = tr("Overall"),
            makeKable(table, bold_cols = 1) %>%
              HTML()),

          tabPanel(
            tr("Group-wise"),
            HTML(mgCovMatTable))

        ) # tabBox

      } ## box if (!hasGroups()) ----
      else {

        # output if NO groups
        shinydashboard::box(
          width = 12,
          title = tr("Covariance matrix:"),

          makeKable(table, bold_cols = 1) %>%
            HTML()

        ) # box
      }
    })
  })
}
