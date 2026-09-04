## Step 1: where the data comes from.
##
## The user picks a source, the data is loaded and checked, and the Select button stays
## switched off until it passes. Pressing Select hands the data on; everything after this
## step works from that copy.

dataSourceUI <- function(id) {
  ns <- NS(id)

  # The names are what the user reads, the values what input$source is compared
  # against - so only the names are translated.
  sources <- stats::setNames(
    c("Workspace", "CSV", "SPSS", "RData"),
    c(tr("data.source.type.workspace"),
      tr("data.source.type.csv"),
      tr("data.source.type.spss"),
      tr("data.source.type.rdata")))

  # globalenv() is somebody's own workspace only when a person is sitting at the console
  # the app was started from. Hosted, it holds whatever was left there by whoever put the
  # app up -> drop the entry, and the R data file upload is the way in.
  # shinyCTTApp(workspace = ) decides, and defaults to interactive().
  if (!isTRUE(getOption("shinyCTT.workspace")))
    sources <- sources[sources != "Workspace"]

  fluidRow(
    column(
      width = 3,

      shinydashboard::box(
        width = NULL,
        selectInput(ns("source"), tr("data.source.label"), choices = sources)),

      shinydashboard::box(
        width = NULL,
        conditionalPanel(
          condition = "input.source == 'CSV'",
          fileInput(ns("CSVFile"), tr("data.source.csv"),
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          checkboxInput(ns("header"), tr("common.header"), TRUE),
          radioButtons(ns("sep"), tr("common.separator"),
                       choiceNames = list(tr("common.comma"), tr("common.semicolon"), tr("common.tab")),
                       choiceValues = c(",", ";", "\t"),
                       selected = ","),
          radioButtons(ns("quote"), tr("data.csv.quote.label"),
                       choiceNames = list(tr("common.none"), tr("data.csv.quote.double"), tr("data.csv.quote.single")),
                       choiceValues = c("", '"', "'"),
                       selected = '"'),
          ns = ns),
        conditionalPanel(
          condition = "input.source == 'SPSS'",
          fileInput(ns("SPSSFile"), tr("data.source.spss"),
                    multiple = FALSE,
                    accept = c(".sav", ".zsav", ".por")),
          ns = ns),
        conditionalPanel(
          condition = "input.source == 'RData'",
          fileInput(ns("RDataFile"), tr("data.source.rdata"),
                    multiple = FALSE,
                    accept = c(".RData", ".rda", ".rds")),
          ns = ns),

        # Two of the sources hold several objects to pick between - the workspace and an
        # .RData - so the chooser sits outside the panels above and draws nothing for the
        # rest.
        uiOutput(ns("objectChooser"))),

      shinydashboard::box(
        width = NULL,
        actionButton(ns("dataSelectButton"), tr("common.select"), width = "100%"))

    ), # column
    column(
      width = 9,
      shinydashboard::box(
        width = NULL,
        title = tr("data.preview.title"),
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

    # Whether the workspace is on offer at all - the same setting the source list is built
    # from. It cannot change while the app runs, so it is read once.
    workspaceOffered <- isTRUE(getOption("shinyCTT.workspace"))

    ## which kind of R data file was uploaded ----
    # An .rds holds one object and no name for it; an .RData (or .rda) holds any number of
    # them and gives their names.
    isRds <- reactive({
      req(input$RDataFile)

      grepl("\\.rds$", input$RDataFile$name, ignore.case = TRUE)
    })

    ## what an uploaded .RData holds ----
    # A reactive of its own, so the file is read once per upload rather than once per pick:
    # read inside the observer below, it would be read again every time the user chose
    # another object out of it, and the rebuilt chooser would snap back to the first name.
    #
    # The objects go into an environment of their own, never into the app's own workspace,
    # which one R process shares between every visitor. Nothing points at that environment
    # once the visit ends, so R frees it.
    uploadedObjects <- reactive({
      req(input$RDataFile, !isRds())

      objects <- new.env()
      load(input$RDataFile$datapath, envir = objects)

      objects
    })

    ## the data objects there are to pick from ----
    # Three answers, and the difference between the last two is what the observer below
    # reports on: NULL when this source has no objects to pick between at all - a CSV, an
    # SPSS file, an .rds - an empty vector when it has objects but none of them could be a
    # table, and the names otherwise.
    pickableObjects <- reactive({

      # The console's workspace, or what an uploaded .RData holds.
      objects <- if (identical(input$source, "Workspace") && workspaceOffered) {
        globalenv()

      } else if (identical(input$source, "RData") && !isRds()) {
        # A file that cannot be read is reported by the observer below; here it only means
        # there is nothing to list.
        tryCatch(uploadedObjects(), error = function(e) NULL)

      } else {
        NULL
      }

      if (is.null(objects)) return(NULL)

      Filter(
        function(objectName) {
          object <- get(objectName, envir = objects)

          !is.null(dim(object)) && typeof(object) != "character"
        },
        ls(envir = objects))
    })

    output$objectChooser <- renderUI({

      # NULL and not req(): a stopped render leaves what it drew last time on the screen,
      # so switching from the workspace to a CSV would keep the workspace's chooser.
      if (length(pickableObjects()) == 0) return(NULL)

      # The workspace is picked from straight away, an uploaded .RData after the file, so
      # the two labels number themselves 1b. and 1c.
      label <- if (identical(input$source, "Workspace")) {
        tr("data.source.workspace")
      } else {
        tr("data.source.rdata.object")
      }

      selectInput(ns("chosenObject"), label, pickableObjects())
    })

    ## load the data and check it ----
    observeEvent(
      list(input$source,
           input$chosenObject,
           input$CSVFile,
           input$SPSSFile,
           input$RDataFile,
           input$header,
           input$sep,
           input$quote), {
      req(!frozen())

      raw(NULL)

      shinyjs::disable("dataSelectButton")

      notifications$notList$noData <- shinydashboard::notificationItem(
        text = tr("data.preview.empty"),
        icon = icon("times"),
        status = "danger")

      ### an R data file holding no data set ----
      # The chooser above draws nothing when a file holds no table, so without this the
      # user would be left with a file they had just chosen and no sign of what was wrong
      # with it. Only for an uploaded file: a workspace with nothing in it is not something
      # the user has just done, and this runs at startup.
      if (identical(input$source, "RData") &&
          identical(pickableObjects(), character(0))) {

        notifications$notList$noDataset <- shinydashboard::notificationItem(
          text = tr("data.error.no.dataset"),
          icon = icon("times"),
          status = "danger")
        showNotification(
          tr("data.error.no.dataset"),
          duration = 10,
          id = "noDatasetNot",
          type = "error")

        return()
      }

      notifications$notList$noDataset <- NULL
      removeNotification("noDatasetNot")

      ### choose data source ----
      # Nothing to read until the control that goes with the chosen source has something in
      # it. This stays outside the tryCatch below, because req() stops the observer by
      # raising an error of its own and would otherwise be reported as a failed read.
      #
      # An .rds is the one object it holds, so the file is enough; an .RData needs a pick
      # as well. So does the workspace, which is not readable at all when the app is not
      # offering it.
      req(switch(
        input$source,
        "CSV" = input$CSVFile,
        "SPSS" = input$SPSSFile,
        "RData" = if (isRds()) input$RDataFile else input$chosenObject,
        "Workspace" = if (workspaceOffered) input$chosenObject))

      # Reading is caught, because it fails on plenty of things a user can point at: a
      # malformed CSV, a corrupt SPSS file, an R data file that is neither, or a workspace
      # object that gets past the chooser's filter without being something data.frame() can
      # make a table of - a sparse Matrix, for one. An error let out of an observer ends
      # the session on the spot, so the user would lose the app rather than be told the
      # data set is unusable.
      loadedData <- tryCatch({

        # The same four names the req() above tests, so input$source is always one of
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
          "RData" = if (isRds()) readRDS(input$RDataFile$datapath)
                    else get(input$chosenObject, envir = uploadedObjects()),
          "Workspace" = get(input$chosenObject, envir = globalenv()))

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
          text = tr("data.error.unreadable"),
          icon = icon("times"),
          status = "danger")
        showNotification(
          paste(tr("data.error.unreadable"), conditionMessage(loadedData)),
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
          text = tr("data.error.no.numeric"),
          icon = icon("times"),
          status = "danger")
        showNotification(
          tr("data.error.no.numeric"),
          duration = 5,
          id = "noNumericNot",
          type = "error")

      } else {
        notifications$notList$noNumeric <- NULL
        removeNotification("noNumericNot")
      }

      if (length(raw()) <= 1) {
        notifications$notList$oneCol <- shinydashboard::notificationItem(
          text = tr("data.error.one.column"),
          icon = icon("times"),
          status = "danger")
        showNotification(
          tr("data.error.one.column"),
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

      raw() |>
        DT::datatable(options = list(language = dtLanguage())) |>
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
      for (controlId in c("source", "CSVFile", "header", "sep", "quote", "SPSSFile",
                          "RDataFile", "chosenObject", "dataSelectButton"))
        shinyjs::disable(controlId)
    })

    ## what the rest of the app gets back ----
    list(
      raw = raw,

      chosen = chosen,

      # Where the data came from, so makeRCode() can write the matching
      # read.csv() / read_spss() / readRDS() / load() line into the exported script. An
      # .rds is read straight into one object and an .RData is loaded and one of its
      # objects taken, so the two are told apart here rather than in makeRCode().
      descriptor = reactive(switch(
        input$source,
        "Workspace" = list(type = "Workspace", object = input$chosenObject),
        "CSV" = list(type = "CSV",
                     name = input$CSVFile$name,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote),
        "SPSS" = list(type = "SPSS", name = input$SPSSFile$name),
        "RData" = if (isRds()) {
          list(type = "RDS", name = input$RDataFile$name)
        } else {
          list(type = "RData", name = input$RDataFile$name, object = input$chosenObject)
        })),

      # An .rds has no name for what it holds, so the file's own name stands in.
      name = reactive(switch(
        input$source,
        "Workspace" = input$chosenObject,
        "CSV" = gsub("\\.csv", "", input$CSVFile$name),
        "SPSS" = gsub("\\.sav|\\.zsav|\\.por", "", input$SPSSFile$name),
        "RData" = if (isRds()) {
          sub("\\.rds$", "", input$RDataFile$name, ignore.case = TRUE)
        } else {
          input$chosenObject
        })))
  })
}
