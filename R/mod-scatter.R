## The scatter plot box on the Correlations tab.
##
## With a group column it is a cttTabCard() with an "Overall" and a "Group-wise" tab;
## without one it is a plain cttCard() holding the "Overall" half on its own. The controls
## for the whole sample are written once and used in both places.

scatterUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("box"))
}

## Arguments, all reactives:
##   data        : the chosen items, and the group column if there is one
##   itemCols    : the names of the item columns
##   groupCol    : the name of the group column
##   hasGroups   : TRUE when the group column is usable
##   groupColors : $solid for the points, named by group
scatterServer <- function(id, data, itemCols, groupCol, hasGroups, groupColors) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## the box ----
    output$box <- renderUI({

      req(data())

      # Used twice: on its own without a group column, and as the "Overall" tab with one.
      overallContents <- fluidRow(

        column(
          width = 4,
          selectInput(
            ns("scatterItemX"),
            tr("stats.select.abscissa"),
            choices = itemCols())),
        column(
          width = 4,
          selectInput(
            ns("scatterItemY"),
            tr("stats.select.ordinate"),
            choices = itemCols(),
            selected = itemCols()[2])))

      ### the box without a group column ----
      if (!isTRUE(hasGroups()))
        return(
          cttCard(
            title = tr("stats.scatter.title"),

            overallContents,

            plotOutput(ns("singleScatter"))
          )) # box

      ### the tab card with one ----
      cttTabCard(
        title = tr("stats.scatter.title"),

        #### box tab card overall panel ----
        tabPanel(
          title = tr("common.overall"),

          overallContents,

          plotOutput(ns("singleScatter"))),

        #### box tab card group-wise panel ----
        tabPanel(
          title = tr("common.groupwise"),

          fluidRow(

            column(
              width = 4,
              selectInput(
                ns("scatterItemXGroup"),
                tr("stats.select.abscissa"),
                choices = itemCols())),
            column(
              width = 4,
              selectInput(
                ns("scatterItemYGroup"),
                tr("stats.select.ordinate"),
                choices = itemCols(),
                selected = itemCols()[2])),
            column(
              width = 4,
              checkboxGroupInput(
                ns("scatterGroupGroups"),
                tr("stats.select.groups"),
                choices = unique(data()[, groupCol()]),
                selected = unique(data()[, groupCol()]),
                inline = TRUE))),

          plotOutput(ns("groupScatter")))
      ) # tabBox
    })

    ## the whole-sample scatter plot ----
    output$singleScatter <- renderPlot({
      req(data(), input$scatterItemX, input$scatterItemY)

      ggplot2::ggplot(
          data.frame(
              itemX = data()[, input$scatterItemX],
              itemY = data()[, input$scatterItemY]) |>
            stats::na.omit(),
          ggplot2::aes(x = .data$itemX, y = .data$itemY)) +

          ggplot2::geom_point(color = fuColors()$mark) +
          ggplot2::xlab(input$scatterItemX) +
          ggplot2::ylab(input$scatterItemY) +
          ggplot2::theme_classic()
    })

    ## the group-wise scatter plot ----
    output$groupScatter <- renderPlot({
      req(data(), input$scatterItemXGroup, input$scatterItemYGroup,
          input$scatterGroupGroups)

      ggplot2::ggplot(
        subset(
          data(),
          subset = data()[, groupCol()] %in% input$scatterGroupGroups,
          select = c(groupCol(), input$scatterItemXGroup, input$scatterItemYGroup)) |>
          stats::na.omit() |>
          stats::setNames(nm = c("group", "itemX", "itemY")),
        ggplot2::aes(x = .data$itemX, y = .data$itemY, color = .data$group)) +

        ggplot2::geom_point() +
        ggplot2::xlab(input$scatterItemXGroup) +
        ggplot2::ylab(input$scatterItemYGroup) +
        ggplot2::scale_color_manual(values = groupColors()$solid, name = groupCol()) +
        ggplot2::theme_classic()
    })
  })
}
