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

      ## box if (hasGroups()) ----
      if (hasGroups()) {
        groups <- unique(data()[, groupCol()])
        groupSizes <- c(table(data()[, groupCol()]))[as.character(groups)]

        # One table per group, each under its own heading. kableExtra put that heading in a
        # dark band row inside a single tall table; reactable cannot draw a row that is not
        # in the data, so the heading sits above its table instead.
        groupTables <- lapply(seq_along(groups), function(position) {
          tagList(
            groupHeading(sprintf(tr("common.group.label"),
                                 groups[position], groupSizes[position])),
            covarianceTable(
              subset(data()[, itemCols()], data()[, groupCol()] == groups[position])))
        })

        # output if groups
        shinydashboard::tabBox(
          width = 12,
          title = tr("stats.covmatrix.title"),
          side = "right",

          tabPanel(
            title = tr("common.overall"),
            groupHeading(sprintf(tr("common.overall.n"), nrow(data()))),
            covarianceTable(data()[, itemCols()])),

          tabPanel(
            tr("common.groupwise"),
            unname(groupTables))

        ) # tabBox

      } ## box if (!hasGroups()) ----
      else {

        # output if NO groups
        shinydashboard::box(
          width = 12,
          title = tr("stats.covmatrix.title"),

          groupHeading(sprintf(tr("common.overall.n"), nrow(data()))),
          covarianceTable(data()[, itemCols()])

        ) # box
      }
    })
  })
}
