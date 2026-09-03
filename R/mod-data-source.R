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
        # The three source names are also the values input$source is compared against
        # elsewhere, so they are left untranslated - see the translation report.
        selectInput(ns("source"), tr("1a. Choose source of data"),
                    choices = c("Workspace", "CSV", "SPSS"))),

      shinydashboard::box(
        width = NULL,
        conditionalPanel(
          condition = "input.source == 'Workspace'",
          uiOutput(ns("objectsInWorkspace")),
          ns = ns),
        conditionalPanel(
          condition = "input.source == 'CSV'",
          fileInput(ns("CSVFile"), tr("1b. Choose CSV File"),
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          checkboxInput(ns("header"), tr("Header"), TRUE),
          radioButtons(ns("sep"), tr("Separator"),
                       choiceNames = list(tr("Comma"), tr("Semicolon"), tr("Tab")),
                       choiceValues = c(",", ";", "\t"),
                       selected = ","),
          radioButtons(ns("quote"), tr("Quote"),
                       choiceNames = list(tr("None"), tr("Double Quote"), tr("Single Quote")),
                       choiceValues = c("", '"', "'"),
                       selected = '"'),
          ns = ns),
        conditionalPanel(
          condition = "input.source == 'SPSS'",
          fileInput(ns("SPSSFile"), tr("1b. Choose SPSS File"),
                    multiple = FALSE,
                    accept = c(".sav", ".zsav", ".por")),
          ns = ns)),

      shinydashboard::box(
        width = NULL,
        actionButton(ns("dataSelectButton"), tr("Select"), width = "100%"))

    ), # column
    column(
      width = 9,
      shinydashboard::box(
        width = NULL,
        title = tr("Raw data:"),
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
        tr("1b. Choose data object from Workspace"),
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
        text = tr("No data selected"),
        icon = icon("times"),
        status = "danger")

      ### choose data source ----
      # Nothing to read until the control that goes with the chosen source has something in
      # it. This stays outside the tryCatch below, because req() stops the observer by
      # raising an error of its own and would otherwise be reported as a failed read.
      req(switch(
        input$source,
        "CSV" = input$CSVFile,
        "SPSS" = input$SPSSFile,
        "Workspace" = input$objectFromWorkspace))

      # Reading is caught, because it fails on plenty of things a user can point at: a
      # malformed CSV, a corrupt SPSS file, or a workspace object that gets past the
      # chooser's filter without being something data.frame() can make a table of - a sparse
      # Matrix, for one. An error let out of an observer ends the session on the spot, so
      # the user would lose the app rather than be told the data set is unusable.
      loadedData <- tryCatch({

        # The same three names the req() above tests, so input$source is always one of
        # them by the time this runs.
        userDataTmp <- switch(
          input$source,
          "CSV" = utils::read.csv(
            file = input$CSVFile$datapath,
            header = input$header,
            sep = input$sep,
            quote = input$quote,
            stringsAsFactors = FALSE),
          "SPSS" = haven::read_spss(file = input$SPSSFile$datapath),
          "Workspace" = get(input$objectFromWorkspace))

        # TRUE for every column that arrived as a factor -> those become plain text.
        factorColumns <- vapply(userDataTmp, is.factor, logical(1))

        if (any(factorColumns)) {
          userDataTmp[factorColumns] <- lapply(userDataTmp[factorColumns], as.character)
        }

        data.frame(userDataTmp, stringsAsFactors = FALSE)

      }, error = function(e) e)

      ### the data could not be read ----
      # raw() and the Select button were already emptied and switched off at the top of this
      # observer, so saying so and stopping here leaves step 1 where it was.
      if (inherits(loadedData, "error")) {
        notifications$notList$unreadable <- shinydashboard::notificationItem(
          text = tr("This data set could not be read."),
          icon = icon("times"),
          status = "danger")
        showNotification(
          paste(tr("This data set could not be read."), conditionMessage(loadedData)),
          duration = 10,
          id = "unreadableNot",
          type = "error")

        return()
      }

      notifications$notList$unreadable <- NULL
      removeNotification("unreadableNot")

      raw(loadedData)

      notifications$notList$noData <- NULL

      ### Test the data for problems ----
      if (!any(vapply(raw(), is.numeric, logical(1)))) {
        notifications$notList$noNumeric <- shinydashboard::notificationItem(
          text = tr("No numeric columns found"),
          icon = icon("times"),
          status = "danger")
        showNotification(
          tr("No numeric columns found"),
          duration = 5,
          id = "noNumericNot",
          type = "error")

      } else {
        notifications$notList$noNumeric <- NULL
        removeNotification("noNumericNot")
      }

      if (length(raw()) <= 1) {
        notifications$notList$oneCol <- shinydashboard::notificationItem(
          text = tr("Only one column found"),
          icon = icon("times"),
          status = "danger")
        showNotification(
          tr("Only one column found"),
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
    # An output of its own rather than one written from inside an observer: picking a new
    # source empties raw(), and the table has to empty with it instead of going on showing
    # the last data set that could be read.
    output$dataOverview <- DT::renderDataTable({

      # No data -> one empty column, which draws as an empty table. Neither NULL nor a
      # data.frame() with no columns at all will do: both reach the browser with nothing
      # for DT to draw, and the error DT throws there stops shiny applying the rest of
      # that batch of outputs - the workspace chooser among them (see GOTCHAS.md).
      # dom = "t" leaves out the search box, the length menu and the row count, so an
      # empty preview is a blank panel rather than the furniture of a table with nothing
      # in it.
      if (is.null(raw()))
        return(DT::datatable(
          stats::setNames(data.frame(character(0)), " "),
          rownames = FALSE,
          options = list(dom = "t")))

      raw() %>%
        DT::datatable() %>%
        DT::formatRound(
          columns = seq_along(raw())[vapply(raw(), is.numeric, logical(1))],
          digits = 3)
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
