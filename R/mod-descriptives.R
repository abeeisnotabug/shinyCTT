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

    ## the box ----
    output$box <- renderUI({
      req(data())

      table <- t(apply(
        data()[, itemCols()],
        MARGIN = 2,
        FUN = function(col) {
          c(Mean = mean(col, na.rm = TRUE),
            Sd = stats::sd(col, na.rm = TRUE),
            Skew = itemSkewness(col),
            Excess = itemKurtosis(col) - 3)
        }
      )) # t(apply(

      # These column names become the table's header row, so they go through tr() here
      # rather than inside the c() above - a function call cannot stand on the left of "="
      # inside c().
      colnames(table) <- c(tr("Mean"), tr("Sd"), tr("Skew"), tr("Excess"))

      nHeader <- c(1, 4)
      names(nHeader) <- c(" ", sprintf(tr("n<sub>all</sub> = %i"), nrow(data())))

      overallDescrTable <- makeKable(table, bold_cols = 1) %>%
        kableExtra::add_header_above(header = nHeader, escape = FALSE) %>%
        HTML()

      ### the box with a group column ----
      if (hasGroups()) {
        groups <- unique(data()[, groupCol()])

        mgDescrTableList <- lapply(
          groups,
          function(group) {
            groupTable <- t(apply(
                subset(
                  data()[, itemCols()],
                  data()[, groupCol()] == group),
                MARGIN = 2,
                FUN = function(col) {
                  c(Mean = mean(col, na.rm = TRUE), SD = stats::sd(col, na.rm = TRUE),
                    Skew = itemSkewness(col),
                    Excess = itemKurtosis(col) - 3)
                }
            )) # t(apply(

            # Same reasoning as the overall table above: the column names become the
            # header row, so they are translated after the fact.
            colnames(groupTable) <- c(tr("Mean"), tr("SD"), tr("Skew"), tr("Excess"))
            groupTable
          }
        ) # lapply

        descrGroupHeader <- c(1, rep(4, length(groups)))
        names(descrGroupHeader) <- c(
          " ",
          sprintf(
            tr("Group: %s (n<sub>%s</sub> = %i)"),
            groups,
            groups,
            c(table(data()[, groupCol()]))[as.character(groups)]))

        mgDescrTableListTagged <- list()

        for (i in 1:((length(groups) + 1) %/% 2)) {
          mgDescrTableListTagged[i] <-
            makeKable(
                do.call(cbind,
                        mgDescrTableList[(2 * i - 1):min(2 * i, length(groups))]),
                bold_cols = 1) %>%

              kableExtra::add_header_above(
                header = descrGroupHeader[c(1, (2 * i):min(2 * i + 1, length(groups) + 1))],
                escape = FALSE) %>%

              kableExtra::column_spec(
                column = 5,
                border_right = "1px solid lightgrey")
        }

        # output if groups
        shinydashboard::tabBox(
          width = 6,
          title = tr("Descriptive statistics:"),
          side = "right",

          tabPanel(
            tr("Overall"),
            overallDescrTable),

          tabPanel(
            tr("Group-wise"),
            tagList(do.call(HTML, mgDescrTableListTagged)))

        ) # tabBox

      } ### the box without a group column ----
      else {

        shinydashboard::box(
          width = 6,
          title = tr("Descriptive statistics:"),
          overallDescrTable)
      }
    })
  })
}
