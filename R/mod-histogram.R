## The histogram box on the Statistics tab.
##
## With a group column it is a tabBox with an "Overall" and a "Group-wise" tab; without one
## it is a plain box holding the "Overall" half on its own. The controls for the whole
## sample are written once and used in both places.
##
## Nothing in this file knows the five CTT models exist.

histogramUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("box"))
}

## Arguments, all reactives:
##   data        : the chosen items, and the group column if there is one
##   itemCols    : the names of the item columns
##   groupCol    : the name of the group column
##   hasGroups   : TRUE when the group column is usable
##   groupColors : $solid for the bars, $light for the density curves, named by group
histogramServer <- function(id, data, itemCols, groupCol, hasGroups, groupColors) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## the box ----
    output$box <- renderUI({

      req(data(), itemCols())

      # Used twice: on its own without a group column, and as the "Overall" tab with one.
      overallContents <- tagList(

        fluidRow(
          column(
            width = 6,
            selectInput(ns("histItem"), tr("stats.hist.item.label"), choices = itemCols()))),

        plotOutput(ns("singleHist")),

        fluidRow(
          column(
            width = 6,
            sliderInput(
              ns("singleNoBins"),
              tr("stats.hist.bins.label"),
              min = 1, max = 100, value = 30, step = 1)),
          column(
            width = 6,
            checkboxInput(
              ns("singleDens"),
              tr("stats.hist.density.checkbox"),
              value = FALSE))))

      ### the box without a group column ----
      if (!isTRUE(hasGroups()))
        return(shinydashboard::box(title = tr("stats.hist.title"), overallContents))

      ### the tabBox with one ----
      shinydashboard::tabBox(
        title = tr("stats.hist.title"),
        side = "right",

        tabPanel(
          title = tr("common.overall"),
          overallContents),

        tabPanel(
          title = tr("common.groupwise"),

          fluidRow(
            column(
              width = 6,
              selectInput(
                ns("histItemGroup"),
                tr("stats.hist.item.label"),
                choices = itemCols())),
            column(
              width = 6,
              checkboxGroupInput(
                ns("histGroupGroups"),
                tr("stats.select.groups"),
                choices = unique(data()[, groupCol()]),
                selected = unique(data()[, groupCol()]),
                inline = TRUE))),

          plotOutput(ns("groupHist")),

          fluidRow(
            column(
              width = 6,
              sliderInput(
                ns("groupNoBins"),
                tr("stats.hist.bins.label"),
                min = 1, max = 100, value = 30, step = 1)),
            column(
              width = 6,
              checkboxInput(
                ns("groupDens"),
                tr("stats.hist.density.checkbox"),
                value = FALSE)))
        ) # tabPanel
      ) # tabBox
    })

    ## the whole-sample histogram ----
    output$singleHist <- renderPlot({

      # The box above builds the dropdown and the slider, so they do not exist until it
      # has run once.
      req(data(), input$histItem, input$singleNoBins)

      ggplot2::ggplot(
        data.frame(item = stats::na.omit(data()[, input$histItem])),
        ggplot2::aes(x = .data$item)) +

        ggplot2::geom_histogram(
          if (input$singleDens) ggplot2::aes(y = ggplot2::after_stat(.data$density)),
          color = "white",
          fill = fuColors()$fill,
          bins = input$singleNoBins) +

        ggplot2::xlab(input$histItem) +
        ggplot2::theme_classic() +

        if (input$singleDens)
          # the group's own colour mixed 40% toward white, so the curve reads on the bars
          ggplot2::geom_density(color = fuColors()$mark, linewidth = 1)
    })

    ## the group-wise histogram ----
    output$groupHist <- renderPlot({

      req(data(), input$histItemGroup, input$groupNoBins, input$histGroupGroups)

      ggplot2::ggplot(
        subset(
          data(),
          subset = data()[, groupCol()] %in% input$histGroupGroups,
          select = c(groupCol(), input$histItemGroup)) %>%
          stats::na.omit() %>%
          stats::setNames(nm = c("group", "item")),
        ggplot2::aes(x = .data$item, fill = .data$group)) +

        ggplot2::geom_histogram(
          if (input$groupDens) ggplot2::aes(y = ggplot2::after_stat(.data$density)),
          color = "white",
          bins = input$groupNoBins,
          position = "dodge") +

        ggplot2::xlab(input$histItemGroup) +
        ggplot2::scale_fill_manual(values = groupColors()$solid, name = groupCol()) +
        ggplot2::theme_classic() +

        if (input$groupDens)
          list(
            ggplot2::geom_density(
              ggplot2::aes(color = .data$group),
              fill = NA,
              linewidth = 1),
            ggplot2::scale_color_manual(values = groupColors()$light, name = groupCol()))
    })
  })
}
