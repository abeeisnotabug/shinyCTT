## The descriptive statistics box on the Statistics tab.
##
## With a group column it is a tabBox with an "Overall" and a "Group-wise" tab; without one
## it is a plain box holding the "Overall" table on its own.

## Skewness and kurtosis of one item, as the moments package computed them: the third and
## fourth central moments divided by the second, with no small-sample correction. Missing
## values are dropped first.
##
## The excess kurtosis of a normal distribution is 0, so the table subtracts 3 from
## itemKurtosis() rather than these functions doing it.
itemSkewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)

  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3 / 2)
}

itemKurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)

  n * sum((x - mean(x))^4) / sum((x - mean(x))^2)^2
}

descriptivesUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("box"))
}

## Arguments, all reactives:
##   data      : the chosen items, and the group column if there is one
##   itemCols  : the names of the item columns
##   groupCol  : the name of the group column
##   hasGroups : TRUE when the group column is usable
descriptivesServer <- function(id, data, itemCols, groupCol, hasGroups) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## the four numbers per item ----
    # Mean, standard deviation, skewness and excess kurtosis of every chosen item, for
    # whichever rows are handed in.
    itemMoments <- function(rows) {
      t(apply(
        rows,
        MARGIN = 2,
        FUN = function(col) {
          c(Mean = mean(col, na.rm = TRUE),
            SD = stats::sd(col, na.rm = TRUE),
            Skew = itemSkewness(col),
            Excess = itemKurtosis(col) - 3)
        }))
    }

    ## one descriptives table ----
    # How many rows went into it is said in the heading above it, the same way every other
    # table on this page says it.
    #
    # locales = "en-US" pins the decimal point - without it reactable rounds in the
    # reader's own language and a German browser prints 1,504 (see GOTCHAS.md).
    momentsTable <- function(moments) {
      reactable::reactable(
        as.data.frame(moments),
        rownames = TRUE,
        defaultColDef = reactable::colDef(
          # Wide enough for the longest of the four headers in any language, "Mittelwert".
          minWidth = 84,
          format = reactable::colFormat(digits = 3, locales = "en-US")),
        columns = list(
          .rownames = reactable::colDef(name = "", style = list(fontWeight = "bold")),
          Mean = reactable::colDef(name = tr("stats.desc.mean")),
          SD = reactable::colDef(name = tr("stats.desc.sd")),
          Skew = reactable::colDef(name = tr("stats.desc.skew")),
          Excess = reactable::colDef(name = tr("stats.desc.excess"))),
        sortable = FALSE,
        pagination = FALSE,
        compact = TRUE)
    }

    ## the box ----
    output$box <- renderUI({
      req(data())

      overallTable <- tagList(
        groupHeading(sprintf(tr("common.overall.n"), nrow(data()))),
        momentsTable(itemMoments(data()[, itemCols()])))

      ### the box with a group column ----
      if (hasGroups()) {
        groups <- unique(data()[, groupCol()])
        groupSizes <- c(table(data()[, groupCol()]))[as.character(groups)]

        # One table per group, each under its own heading - the same shape as the
        # covariance matrix box next to it.
        groupTables <- lapply(seq_along(groups), function(position) {
          tagList(
            groupHeading(sprintf(tr("common.group.label"),
                                 groups[position], groupSizes[position])),
            momentsTable(itemMoments(
              subset(data()[, itemCols()], data()[, groupCol()] == groups[position]))))
        })

        # output if groups
        shinydashboard::tabBox(
          width = 6,
          title = tr("stats.desc.title"),
          side = "right",

          tabPanel(
            tr("common.overall"),
            overallTable),

          tabPanel(
            tr("common.groupwise"),
            unname(groupTables))

        ) # tabBox

      } ### the box without a group column ----
      else {

        shinydashboard::box(
          width = 6,
          title = tr("stats.desc.title"),
          overallTable)
      }
    })
  })
}
