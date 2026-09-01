## Step 1: where the data comes from.
##
## The user picks a source, the data is loaded and checked, and the Select button stays
## switched off until it passes. Pressing Select hands the data on; everything after this
## step works from that copy.

dataSourceUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 3,

      shinydashboard::box(
        width = NULL,
        selectInput(ns("source"), "1a. Choose source of data",
                    choices = c("Workspace", "CSV", "SPSS"))),

      shinydashboard::box(
        width = NULL,
        conditionalPanel(
          condition = "input.source == 'Workspace'",
          uiOutput(ns("objectsInWorkspace")),
          ns = ns),
        conditionalPanel(
          condition = "input.source == 'CSV'",
          fileInput(ns("CSVFile"), "1b. Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          checkboxInput(ns("header"), "Header", TRUE),
          radioButtons(ns("sep"), "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = ","),
          radioButtons(ns("quote"), "Quote",
                       choices = c(None = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'"),
                       selected = '"'),
          ns = ns),
        conditionalPanel(
          condition = "input.source == 'SPSS'",
          fileInput(ns("SPSSFile"), "1b. Choose SPSS File",
                    multiple = FALSE,
                    accept = c(".sav", ".zsav", ".por")),
          ns = ns)),

      shinydashboard::box(
        width = NULL,
        actionButton(ns("dataSelectButton"), "Select", width = "100%"))

    ), # column
    column(
      width = 9,
      shinydashboard::box(
        width = NULL,
        title = "Raw data:",
        DT::dataTableOutput(ns("dataOverview"))))
  ) # fluidRow
}

## Arguments:
##   notifications : the app's notification list, the one behind the bell in the header
##   frozen        : a reactive, TRUE once the user has moved past this step
##
## Returns a list of reactives:
##   raw        : the data as loaded, before Select was pressed
##   chosen     : the copy taken when Select was pressed. Nothing else changes it, so an
##                observer on it is the app's signal that this step is done.
##   descriptor : where the data came from, in the shape makeRCode() wants
##   name       : the name of the data set, for the factor score filenames
dataSourceServer <- function(id, notifications, frozen) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    raw <- reactiveVal()
    chosen <- reactiveVal()

    ## the data objects that are lying around in R ----
    output$objectsInWorkspace <- renderUI({
      selectInput(
        ns("objectFromWorkspace"),
        "1b. Choose data object from Workspace",
        Filter(
          function(object) !is.null(dim(get(object))) && typeof(get(object)) != "character",
          ls(envir = globalenv())))
    })

    ## load the data and check it ----
    observeEvent(
      list(input$source,
           input$objectFromWorkspace,
           input$CSVFile,
           input$SPSSFile,
           input$header,
           input$sep,
           input$quote), {
      req(!frozen())

      raw(NULL)

      shinyjs::disable("dataSelectButton")

      notifications$notList$noData <- shinydashboard::notificationItem(
        text = "No data selected",
        icon = icon("times"),
        status = "danger")

      ### choose data source ----
      if (input$source == "CSV") {
        req(input$CSVFile)

        userDataTmp <- utils::read.csv(
          file = input$CSVFile$datapath,
          header = input$header,
          sep = input$sep,
          quote = input$quote,
          stringsAsFactors = FALSE)
      } else if (input$source == "SPSS") {
        req(input$SPSSFile)

        userDataTmp <- haven::read_spss(file = input$SPSSFile$datapath)
      } else if (input$source == "Workspace") {
        req(input$objectFromWorkspace)

        userDataTmp <- get(input$objectFromWorkspace)
      }

      if (any(sapply(userDataTmp, is.factor))) {
        userDataTmp[sapply(userDataTmp, is.factor)] <- lapply(
          userDataTmp[sapply(userDataTmp, is.factor)],
          as.character)
      }

      raw(data.frame(userDataTmp, stringsAsFactors = FALSE))

      notifications$notList$noData <- NULL

      ### Test the data for problems ----
      if (!any(sapply(raw(), is.numeric))) {
        notifications$notList$noNumeric <- shinydashboard::notificationItem(
          text = "No numeric columns found",
          icon = icon("times"),
          status = "danger")
        showNotification(
          "No numeric columns found",
          duration = 5,
          id = "noNumericNot",
          type = "error")

      } else {
        notifications$notList$noNumeric <- NULL
        removeNotification("noNumericNot")
      }

      if (length(raw()) <= 1) {
        notifications$notList$oneCol <- shinydashboard::notificationItem(
          text = "Only one column found",
          icon = icon("times"),
          status = "danger")
        showNotification(
          "Only one column found",
          duration = 5,
          id = "oneColNot",
          type = "error")

      } else {
        notifications$notList$oneCol <- NULL
        removeNotification("oneColNot")
      }

      #### If all is good, enable the select button ----
      if (all(
        is.null(notifications$notList$noNumeric),
        is.null(notifications$notList$oneCol))) {

        shinyjs::enable("dataSelectButton")
      }
    })

    ## the raw data, as a table ----
    observeEvent(raw(), {
      output$dataOverview <- raw() %>%
        DT::datatable() %>%
        DT::formatRound(
          columns = seq_along(raw())[sapply(raw(), is.numeric)],
          digits = 3) %>%
        DT::renderDataTable()
    })

    ## Select ----
    observeEvent(input$dataSelectButton, chosen(isolate(raw())))

    ## freeze this step's controls once the user has moved past it ----
    observeEvent(frozen(), {
      req(frozen())

      # shinyjs adds this box's name to the id itself (see GOTCHAS.md), so these are plain.
      for (controlId in c("source", "CSVFile", "header", "sep", "quote",
                          "objectFromWorkspace", "dataSelectButton"))
        shinyjs::disable(controlId)
    })

    ## what the rest of the app gets back ----
    list(
      raw = raw,

      chosen = chosen,

      # Where the data came from, so makeRCode() can write the matching
      # read.csv() / read_spss() / workspace line into the exported script.
      descriptor = reactive(switch(
        input$source,
        "Workspace" = list(type = "Workspace", object = input$objectFromWorkspace),
        "CSV" = list(type = "CSV",
                     name = input$CSVFile$name,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote),
        "SPSS" = list(type = "SPSS", name = input$SPSSFile$name))),

      name = reactive(switch(
        input$source,
        "Workspace" = input$objectFromWorkspace,
        "CSV" = gsub("\\.csv", "", input$CSVFile$name),
        "SPSS" = gsub("\\.sav|\\.zsav|\\.por", "", input$SPSSFile$name))))
  })
}
