## Step 2: which items, which groups, and what to do about missing values.
##
## Everything after this step works from the cut-down data this one produces, so it hands
## back six answers: the data itself, which columns are the items, which column is the
## group column, whether that group column can be used, whether FIML is on, and which rows
## have a missing value.

dataSubsetUI <- function(id) {
  ns <- NS(id)

  tagList(
    #### first row info boxes ----
    fluidRow(
      shinydashboard::valueBoxOutput(ns("itemInfoBox")),
      shinydashboard::valueBoxOutput(ns("groupInfoBox")),
      shinydashboard::valueBoxOutput(ns("naInfoBox"))),

    #### second row choosers ----
    fluidRow(

      column(
        width = 4,

        shinydashboard::box(
          width = NULL,
          uiOutput(ns("itemColsChooser"))),

        shinydashboard::box(
          width = NULL,
          uiOutput(ns("groupColChooser")),
          conditionalPanel(
            condition = "input.groupCol != 'noGroupSelected'",
            uiOutput(ns("groupChooser")),
            ns = ns)),

        conditionalPanel(
          "output.incompleteCasesBoolRV",

          shinydashboard::box(
            width = NULL,
            tagList(
              strong(tr("subset.missing.label")),
              checkboxInput(
                ns("useFIML"),
                tr("subset.fiml.checkbox"),
                value = TRUE),

              conditionalPanel(
                "!input.useFIML",
                div(
                  style = paste0("color:red"),
                  HTML(tr("subset.fiml.warning"))),
                ns = ns))),
          ns = ns
        ), # conditionalPanel

        shinydashboard::box(
          width = NULL,
          # subset of items
          shinyjs::disabled(
            actionButton(ns("subsetSelectButton"), tr("common.select"), width = "100%")))
      ), # column

      column(
        width = 4,

        shinydashboard::box(
          width = NULL,
          title = tr("subset.obs.title"),
          reactable::reactableOutput(ns("obsTable"))),

        # The group counts are a table when there is a group column and a note when there
        # is not. renderReactable can only give back a table, so they are two outputs with
        # a guard on each - only one of them ever fills.
        shinydashboard::box(
          width = NULL,
          title = tr("subset.obs.pergroup.title"),
          reactable::reactableOutput(ns("obsPerGroupTable")),
          uiOutput(ns("obsPerGroupNote")))),

      column(
        width = 4,
        shinydashboard::box(
          width = NULL,
          title = tr("subset.na.percol.title"),
          reactable::reactableOutput(ns("naTable"))))
    ) # fluidRow
  ) # tagList
}

## Arguments:
##   chosenData    : a reactive holding the data step 1 handed on
##   notifications : the app's notification list, the one behind the bell in the header
##   frozen        : a reactive, TRUE once the user has moved past this step
##
## Returns a list of reactives:
##   data            : the data cut down to the chosen items and groups. Nothing but the
##                     Select button fills it, so an observer on it is the app's signal
##                     that this step is done.
##   itemCols        : the names of the item columns
##   groupCol        : the name of the group column, or "noGroupSelected"
##   groups          : which groups were ticked
##   hasGroups       : TRUE when the group column can actually be used
##   useFIML         : TRUE when there are missing values and FIML was left ticked
##   incompleteCases : TRUE for every row of the subset with a missing value somewhere
dataSubsetServer <- function(id, chosenData, notifications, frozen) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # The subset before the Select button is pressed, used for the counts on this page.
    dataWithNAs <- reactiveVal()

    # Not called "data": R CMD check reads data(x) as a call to utils::data().
    chosenSubset <- reactiveVal()
    hasGroups <- reactiveVal()
    useFIML <- reactiveVal(FALSE)

    # How many columns each chooser had to offer, for the two blue boxes at the top.
    itemColsRV <- reactiveVal()
    groupColRV <- reactiveVal()

    # The columns the item tick boxes are built from: every numeric column of the chosen
    # data. Read in three places - the chooser below and the two Select all / Unselect all
    # observers - which all have to offer the same list.
    possibleItemColumns <- reactive(
      colnames(chosenData())[vapply(chosenData(), is.numeric, logical(1))])

    ## itemColsChooser ----
    output$itemColsChooser <- renderUI({
      itemColsRV(length(possibleItemColumns()))

      tagList(
        checkboxGroupInput(
          ns("itemCols"),
          tr("subset.items.label"),
          choices = possibleItemColumns(),
          selected = possibleItemColumns(),
          inline = TRUE),
        fluidRow(actionLink(ns("selectall"), tr("common.select.all"), style = "margin-left: 15px"),
                 actionLink(ns("deselectall"), tr("common.unselect.all"), style = "margin-left: 15px")))
    })

    ## groupColChooser ----
    output$groupColChooser <- renderUI({
      possibleGroupCols <- colnames(chosenData())[!(colnames(chosenData()) %in% input$itemCols)]
      groupColRV(length(possibleGroupCols))

      # The "No group column selected" label sits inside a selectInput()'s named choices
      # vector, so it cannot be run through tr() without breaking the value it is paired
      # with - see the translation report.
      selectInput(
          ns("groupCol"),
          tr("subset.groupcol.label"),
          choices = c(
            "No group column selected" = "noGroupSelected",
            possibleGroupCols))
    })

    ## groupChooser ----
    output$groupChooser <- renderUI({
      req(input$groupCol)

      if (input$groupCol != "noGroupSelected" && input$groupCol %in% colnames(chosenData())) {
        possibleGroups <- unique(stats::na.omit(chosenData()[, input$groupCol]))

        if (any(c(table(chosenData()[, input$groupCol])) == 1)) {
          groupWarning <- tr("subset.groups.one.obs")
          possibleGroups <- NULL

          notifications$notList$invalGroups <- shinydashboard::notificationItem(
            text = tr("subset.groups.invalid"),
            icon = icon("times"),
            status = "danger")
          showNotification(
            tr("subset.groups.invalid"),
            duration = 5,
            id = "invalGroups",
            type = "error")

        } else {
          groupWarning <- ""

          notifications$notList$invalGroups <- NULL
          removeNotification("invalGroups")
        }

        tagList(
          checkboxGroupInput(
            ns("groups"),
            tr("subset.groups.label"),
            choices = possibleGroups,
            selected = possibleGroups,
            inline = TRUE),
          helpText(groupWarning))
      }
    })

    ## the subset as it stands, missing values and all ----
    observeEvent(
      list(input$groupCol,
           input$groups,
           input$itemCols), {

      # Nothing to build until step 1 has handed over a data set and the two choosers below
      # have reported what they are set to. input$groupCol is the one to wait for: its list
      # always starts with the hard-coded "No group column selected" = "noGroupSelected",
      # so it holds a value even when the data offers no group column at all. An empty
      # input$itemCols next to it therefore means the user unticked every item, not that the
      # tick boxes have yet to be drawn.
      req(chosenData(), input$groupCol)

      if (input$groupCol != "noGroupSelected") {

        keepRows <- chosenData()[, input$groupCol] %in% input$groups
        keepCols <- c(input$groupCol, input$itemCols)
      } else {

        keepRows <- rep(TRUE, nrow(chosenData()))
        keepCols <- input$itemCols
      }

      dataWithNAs(
        subset(
          chosenData(),
          subset = keepRows,
          select = keepCols))
    })

    ## Select all / Unselect all ----
    observeEvent(input$selectall, {
      # Only act while the user is still choosing items (see GOTCHAS.md).
      if (input$selectall != 0 && !frozen()) {
        itemColsRV(length(possibleItemColumns()))

        updateCheckboxGroupInput(
          session,
          "itemCols",
          inline = TRUE,
          choices = possibleItemColumns(),
          selected = possibleItemColumns())
      }
    })

    observeEvent(input$deselectall, {
      if (input$deselectall != 0 && !frozen()) {
        itemColsRV(length(possibleItemColumns()))

        updateCheckboxGroupInput(
          session,
          "itemCols",
          inline = TRUE,
          choices = possibleItemColumns())
      }
    })

    ## is the selection usable, and does it need a warning? ----
    observeEvent(
      list(input$groupCol,
           input$groups,
           input$itemCols), {

      # Same guard as the observer above: no data chosen yet -> no item count to report on.
      req(chosenData(), input$groupCol)

      # Only while this step is current: once the app has moved on, the item and group
      # selections are frozen and this must not hand the button back.
      req(!frozen())

      if (length(input$itemCols) <= 1 ||
          (input$groupCol != "noGroupSelected" && length(input$groups) == 0)) {

        shinyjs::disable("subsetSelectButton") # subset of items
      } else {
        shinyjs::enable("subsetSelectButton")
      }

      notifications$notList$numItems <- switch(
        as.character(length(input$itemCols)),
        "0" = shinydashboard::notificationItem(
          text = tr("subset.items.none"),
          icon = icon("times"),
          status = "danger"),
        "1" = shinydashboard::notificationItem(
          text = tr("subset.items.only.one"),
          icon = icon("times"),
          status = "danger"),
        "2" = shinydashboard::notificationItem(
          text = HTML(tr("subset.items.only.two")),
          icon = icon("exclamation-triangle"),
          status = "warning"),
        "3" = shinydashboard::notificationItem(
          text = HTML(tr("subset.items.only.three")),
          icon = icon("exclamation-triangle"),
          status = "warning"),
        NULL)

      if (!is.null(notifications$notList$numItems)) {
        showNotification(
          ui = notifications$notList$numItems$children[[1]]$children[[2]],
          duration = 5,
          id = "numItemsNot",
          type = ifelse(notifications$notList$numItems$children[[1]]$children[[1]]$attribs[[4]] == "text-danger",
                        yes = "error",
                        no = "warning"))
      } else {
        removeNotification("numItemsNot")
      }
    })

    ## itemInfoBox ----
    output$itemInfoBox <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = itemColsRV(),
        color = switch(
            as.character(itemColsRV()),
            "0" = "red",
            "1" = "red",
            "2" = "orange",
            "3" = "orange",
            "green"),
        subtitle = tr("subset.items.count"),
        icon = icon("list"))
    })

    ## groupInfoBox ----
    output$groupInfoBox <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = groupColRV(),
        color = "blue",
        subtitle = tr("subset.groupcol.count"),
        icon = icon("users"))
    })

    ## incomplete cases ----
    # TRUE for every row of the chosen subset that has a missing value somewhere.
    # Used by: the yellow NA box, the observations table, and the FIML checkbox.
    incompleteCases <- reactive({
      req(dataWithNAs())
      !stats::complete.cases(dataWithNAs())
    })

    output$incompleteCasesBoolRV <- reactive(any(incompleteCases()))
    outputOptions(output, "incompleteCasesBoolRV", suspendWhenHidden = FALSE)

    ## naInfoBox ----
    output$naInfoBox <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = sum(incompleteCases()),
        color = if (any(incompleteCases())) "yellow" else "green",
        subtitle = tr("subset.na.rows.note"),
        icon = icon("exclamation-triangle"))
    })

    ## naTable ----
    # One column of missing-value counts. The column names of the chosen data sit in the
    # row names, so rownames = TRUE and the .rownames column gets an empty header.
    output$naTable <- reactable::renderReactable({
      reactable::reactable(
        data.frame(NAs = colSums(is.na(chosenData()))),
        rownames = TRUE,
        columns = list(
          .rownames = reactable::colDef(name = ""),
          NAs = reactable::colDef(name = tr("subset.na.col.header"))),
        sortable = FALSE,
        pagination = FALSE,
        compact = TRUE)
    })

    ## obsTable ----
    # How many rows the subset has, and how many of them are complete.
    output$obsTable <- reactable::renderReactable({
      reactable::reactable(
        data.frame(Total = nrow(dataWithNAs()), Complete = sum(!incompleteCases())),
        columns = list(
          Total = reactable::colDef(name = tr("common.total")),
          Complete = reactable::colDef(name = tr("subset.obs.complete"))),
        sortable = FALSE,
        pagination = FALSE,
        compact = TRUE)
    })

    ## obsPerGroupTable ----
    # How many rows each group value has. Its counterpart is obsPerGroupNote below: this
    # one fills only when there is a group column, that one only when there is not.
    output$obsPerGroupTable <- reactable::renderReactable({
      req(input$groupCol, input$groupCol != "noGroupSelected")

      # as.data.frame.matrix takes the "table" class off, so the counts stay on one row
      # instead of melting into three long columns. useNA = "ifany" gives its column the
      # name NA, which needs a printable header.
      groupSizes <- as.data.frame.matrix(
        t(table(chosenData()[, input$groupCol], useNA = "ifany")))
      names(groupSizes)[is.na(names(groupSizes))] <- "NA"

      reactable::reactable(
        groupSizes,
        sortable = FALSE,
        pagination = FALSE,
        compact = TRUE)
    })

    ## obsPerGroupNote ----
    output$obsPerGroupNote <- renderUI({
      req(input$groupCol)

      if (input$groupCol == "noGroupSelected") helpText(tr("subset.groupcol.none"))
    })

    ## Select ----
    observeEvent(input$subsetSelectButton, {

      useFIML(any(incompleteCases()) && isTRUE(input$useFIML))

      if (input$groupCol != "noGroupSelected") {

        keepRows <- chosenData()[, input$groupCol] %in% input$groups
        keepCols <- c(input$groupCol, input$itemCols)
      } else {

        keepRows <- rep(TRUE, nrow(chosenData()))
        keepCols <- input$itemCols
      }

      chosenSubset(
        subset(
          chosenData(),
          subset = keepRows,
          select = keepCols))

      hasGroups(
        input$groupCol != "noGroupSelected" &&
          !any(c(table(chosenSubset()[, input$groupCol])) == 1) &&
          length(input$groups) > 1)

      if (any(incompleteCases())) {
        notifications$notList$NAhand <- shinydashboard::notificationItem(
          text = HTML(tr("subset.na.removed.note")),
          icon = icon("exclamation-triangle"),
          status = "warning")

        showNotification(
          ui = tr("subset.na.removed.note"),
          duration = 5,
          id = "NAremovedNot",
          type = "warning")
      }
    })

    ## freeze this step's controls once the user has moved past it ----
    observeEvent(frozen(), {
      req(frozen())

      # shinyjs adds this box's name to the id itself (see GOTCHAS.md), so these are plain.
      for (controlId in c("itemCols", "selectall", "deselectall", "groupCol", "groups",
                          "subsetSelectButton", "useFIML"))
        shinyjs::disable(controlId)
    })

    ## what the rest of the app gets back ----
    list(
      data = chosenSubset,
      itemCols = reactive(input$itemCols),
      groupCol = reactive(input$groupCol),
      groups = reactive(input$groups),
      hasGroups = hasGroups,
      useFIML = useFIML,
      incompleteCases = incompleteCases)
  })
}
