server <- function(input, output, session) {
  # Preparation ----
  ## Names and colors ----
  if (TRUE) {
    goodColor <- "darkgreen"
    badColor <- "darkred"
    textColor <- "white"
    neutrColor <- "grey"
  } else {
    goodColor <- "white"
    badColor <- "white"
    textColor <- "black"
    neutrColor <- "white"
  }

  # Everything about the five models comes from one place: cttModelFamily(), defined in
  # R/modelFamily.R. Read that file to see what each of these contains.
  family <- cttModelFamily()

  models       <- family$names     # c(tko = "tko", ete = "ete", ...)
  modelsLong   <- family$long      # "&tau;-kongeneric", ... - for tab titles
  modelsAbbrev <- family$abbrev    # "&#964;-kong.", ...     - for table headers
  minItems     <- family$minItems  # c(tko = 4, ete = 3, ...) - fewest testable items
  possComps    <- family$comparable # "etetko", "teqtko", ... - the 9 valid comparisons

  # Model labels plus the coordinates the hierarchical plot draws them at.
  modelTestDF <- family$plot

  # The results half of the app is built twice from the same code: once for the whole
  # sample and once for the group-wise fit. The second pass puts "Mg" on the end of every
  # output id, so "modelTests" and "modelTestsMg" are the same page from different fits.
  passSuffixes <- c(single = "", multigroup = "Mg")

  ## Reactive values ----
  notifications <- reactiveValues(notList = list())

  userDataRaw <- reactiveVal()
  userDataChosen <- reactiveVal()
  userDataNA <- reactiveVal()
  userDataGroup <- reactiveVal()

  itemColsRV <- reactiveVal()
  groupColRV <- reactiveVal()
  validGroupsRV <- reactiveVal()

  fimlRV <- reactiveVal(FALSE)

  # "ML" or "MLR", with "FI" in front when the fits use full information maximum
  # likelihood. Shown in table headers and legends.
  estimatorName <- reactive(paste0(if (fimlRV()) "FI", input$estimator))

  # The fitted models, written by "Test the models" and read by everything on the results
  # tabs. NULL until the button has been pressed.
  modelFitsRV <- reactiveVal(NULL)

  # TRUE once the estimator has been changed after a run -> the results on screen came
  # from the other estimator, so the user is told and the button is marked.
  refitPendingRV <- reactiveVal(FALSE)

  mvnTestResult <- reactiveValues(
    raw = NULL,
    estimator = "ML")

  ## Workflow stage ----

  appStage <- reactiveVal("data")

  # Frozen at the transition to "results" so the sidebar re-renders only on stage changes.
  doMgRV <- reactiveVal(FALSE)


  # Which controls belong to which stage: live while that stage is current, frozen once
  # the user moves past it.
  stageControls <- list(
    data       = c("source", "CSVFile", "header", "sep", "quote", "objectFromWorkspace",
                   "dataSelectButton"),
    subset     = c("itemCols", "selectall", "deselectall", "groupCol", "groups",
                   "subsetSelectButton", "useFIML"),
    statistics = c("doMg", "etaIntFree"))

  observeEvent(appStage(), {

    # Every stage before the current one. seq_len(... - 1) stops one short, so the stage the
    # user is actually on keeps its own controls live.
    stagesAlreadyPassed <- stages[seq_len(match(appStage(), stages) - 1)]

    # stageControls[stagesAlreadyPassed] is a list of character vectors, one per stage;
    # unlist() runs them together into a single vector of input ids to freeze.
    controlsToFreeze <- unlist(stageControls[stagesAlreadyPassed], use.names = FALSE)

    for (controlId in controlsToFreeze) shinyjs::disable(controlId)
  })

  ## Group colours ----
  # ggplot2's default discrete palette, but pinned to the group *by name*: a discrete
  # scale hands out its palette to whichever levels are still in the data, so
  # de-selecting a group in a plot tab used to recolour the ones that remain. The light
  # variants - the same colours mixed 40% toward white - are for the density curves,
  # which are drawn on top of bars in the solid colour.
  groupColors <- reactive({
    groupLevels <- sort(unique(userDataGroup()[, input$groupCol]))
    solid <- grDevices::hcl(
      h = seq(15, 375, length.out = length(groupLevels) + 1)[seq_along(groupLevels)],
      c = 100,
      l = 65)

    list(
      solid = stats::setNames(solid, groupLevels),
      light = stats::setNames(
        grDevices::rgb(t(0.6 * grDevices::col2rgb(solid) + 0.4 * 255), maxColorValue = 255),
        groupLevels))
  })

  ## Notifications ----
  output$infoMenu <- shinydashboard::renderMenu({
    if (any(sapply(notifications$notList, grepl, pattern = "danger"))) {
      status <- "danger"
    } else {
      status <- "primary"
    }

    shinydashboard::dropdownMenu(
      type = "notifications",
      .list = notifications$notList,
      badgeStatus = status)
  })

  output$dataMenuOut <- shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(
      id = "dataMenu",
      .list = sidebarGroups(appStage(), doMgRV()))})

  ## observeEvent reload button ----
  observeEvent(input$dataMenu, {
    if (input$dataMenu == "reloadTab")
      shinyjs::runjs("location.reload()")
  })

  ## Display NAs correctly in datatable ----
  options(htmlwidgets.TOJSON_ARGS = list(na = "string"))

  observeEvent(userDataRaw(), {
    output$dataOverview <- userDataRaw() %>%
      DT::datatable() %>%
      DT::formatRound(
        columns = seq_along(userDataRaw())[sapply(userDataRaw(), is.numeric)],
        digits = 3) %>%
      DT::renderDataTable()
  })

  # dataSelectionTab ----
  ## dataSelectionTab objectsInWorkspace ----
  output$objectsInWorkspace <- renderUI({
    selectInput(
      "objectFromWorkspace",
      "1b. Choose data object from Workspace",
      Filter(
        function(object) !is.null(dim(get(object))) && typeof(get(object)) != "character",
        ls(envir = globalenv())))
  })

  ## dataSelectionTab observeEvent data properties ----
  observeEvent(
    list(input$source,
         input$objectFromWorkspace,
         input$CSVFile,
         input$SPSSFile,
         input$header,
         input$sep,
         input$quote), {
    req(identical(appStage(), "data"))

    userDataRaw(NULL)

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

    userDataRaw(data.frame(userDataTmp, stringsAsFactors = FALSE))

    notifications$notList$noData <- NULL

    ### Test the data for problems ----
    if (!any(sapply(userDataRaw(), is.numeric))) {
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

    if (length(userDataRaw()) <= 1) {
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

  ## dataSelectionTab observeEvent dataSelectButton ----
  observeEvent(input$dataSelectButton, {
    appStage("subset")

    userDataChosen(isolate(userDataRaw()))
    userDataNA(isolate(userDataRaw()))
  })

  # subsetSelectionTab ----
  ## subsetSelectionTab itemColsChooser ----
  output$itemColsChooser <- renderUI({
    possibleItemColumns <- colnames(userDataChosen())[sapply(userDataChosen(), is.numeric)]
    itemColsRV(length(possibleItemColumns))

    tagList(
      checkboxGroupInput(
        "itemCols",
        "2a. Select the item columns:",
        choices = possibleItemColumns,
        selected = possibleItemColumns,
        inline = TRUE),
      fluidRow(actionLink("selectall", "Select all", style = "margin-left: 15px"),
               actionLink("deselectall", "Unselect all", style = "margin-left: 15px")))
  })

  ## subsetSelectionTab groupColChooser ----
  output$groupColChooser <- renderUI({
    possibleGroupCols <- colnames(userDataChosen())[!(colnames(userDataChosen()) %in% input$itemCols)]
    groupColRV(length(possibleGroupCols))

    selectInput(
        "groupCol",
        "2b. Select the group column:",
        choices = c(
          "No group column selected" = "noGroupSelected",
          possibleGroupCols))
  })

  ## subsetSelectionTab groupChooser ----
  output$groupChooser <- renderUI({
    req(input$groupCol)

    if (input$groupCol != "noGroupSelected" && input$groupCol %in% colnames(userDataChosen())) {
      possibleGroups <- unique(stats::na.omit(userDataChosen()[, input$groupCol]))

      if (any(c(table(userDataChosen()[, input$groupCol])) == 1)) {
        groupWarning <- "There are groups with only one observation,
                         you might have selected an item as group column."
        possibleGroups <- NULL

        notifications$notList$invalGroups <- shinydashboard::notificationItem(
          text = "Invalid groups found.",
          icon = icon("times"),
          status = "danger")
        showNotification(
          "Invalid groups found.",
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
          "groups",
          "2c. Select which groups to include",
          choices = possibleGroups,
          selected = possibleGroups,
          inline = TRUE),
        helpText(groupWarning))
    }
  })

  ## subsetSelectionTab observeEvent for userDataNA ----
  observeEvent(
    list(input$groupCol,
         input$groups,
         input$itemCols), {

    if (atLeastStage(appStage(), "subset")) {

      if (input$groupCol != "noGroupSelected") {

        subset <- userDataChosen()[, input$groupCol] %in% input$groups
        select <- c(input$groupCol, input$itemCols)
      } else {

        subset <- rep(TRUE, nrow(userDataChosen()))
        select <- input$itemCols
      }

      userDataNA(
        subset(
          userDataChosen(),
          subset = subset,
          select = select))
    }
  })

  ## subsetSelectionTab observeEvent selectall ----
  observeEvent(input$selectall, {
    # Only act while the user is still choosing items (see GOTCHAS.md).
    if (input$selectall != 0 && identical(appStage(), "subset")) {
      possibleItemColumns <- colnames(userDataChosen())[sapply(userDataChosen(), is.numeric)]
      itemColsRV(length(possibleItemColumns))

      updateCheckboxGroupInput(
        session,
        "itemCols",
        inline = TRUE,
        choices = possibleItemColumns,
        selected = possibleItemColumns)
    }
  })

  ## subsetSelectionTab observeEvent deselectall ----
  observeEvent(input$deselectall, {
    if (input$deselectall != 0 && identical(appStage(), "subset")) {
      possibleItemColumns <- colnames(userDataChosen())[sapply(userDataChosen(), is.numeric)]
      itemColsRV(length(possibleItemColumns))

      updateCheckboxGroupInput(
        session,
        "itemCols",
        inline = TRUE,
        choices = possibleItemColumns)
    }
  })

  ## subsetSelectionTab observeEvent valid subset and notifications ----
  observeEvent(
    list(input$groupCol,
         input$groups,
         input$itemCols), {
    #req(input$itemCols)

    # Only while the subset stage is current: once the app has moved on, the item and
    # group selections are frozen and this must not hand the button back.
    if (identical(appStage(), "subset")) {

      if (length(input$itemCols) <= 1 ||
          (input$groupCol != "noGroupSelected" && length(input$groups) == 0)) {

        shinyjs::disable("subsetSelectButton") # subset of items
      } else {
        shinyjs::enable("subsetSelectButton")
      }

      ### keep the model selection in step with the item count ----
      # TRUE for each model the current item count is enough to test.
      enoughItems <- minItems <= length(input$itemCols)

      for (thisModel in models) {
        updateCheckboxInput(session, thisModel, value = unname(enoughItems[thisModel]))
      }

      for (thisComp in possComps) {
        updateCheckboxInput(
          session,
          thisComp,
          value = enoughItems[substr(thisComp, 1, 3)] && enoughItems[substr(thisComp, 4, 6)])
      }

      notifications$notList$numItems <- switch(
        as.character(length(input$itemCols)),
        "0" = shinydashboard::notificationItem(
          text = "No item selected. No analysis possible.",
          icon = icon("times"),
          status = "danger"),
        "1" = shinydashboard::notificationItem(
          text = "Only one item selected. No analysis possible.",
          icon = icon("times"),
          status = "danger"),
        "2" = shinydashboard::notificationItem(
          text = HTML("Only two items selected. Unable to test the &tau;-kongeneric and
                      the ess. &tau;-equivalent model."),
          icon = icon("exclamation-triangle"),
          status = "warning"),
        "3" = shinydashboard::notificationItem(
          text = HTML("Only three items selected. Unable to test the &tau;-kongeneric model."),
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
    }
  })

  ## subsetSelectionTab itemInfoBox ----
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
      subtitle = "possible item column(s) found",
      icon = icon("list"))
  })

  ## subsetSelectionTab groupInfoBox ----
  output$groupInfoBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = groupColRV(),
      color = "blue",
      subtitle = "possible group column(s) found",
      icon = icon("users"))
  })

  ## subsetSelectionTab incomplete cases ----
  # TRUE for every row of the chosen subset that has a missing value somewhere.
  # Used by: the yellow NA box, the observations table, and the FIML checkbox.
  incompleteCases <- reactive({
    req(userDataNA())
    !stats::complete.cases(userDataNA())
  })

  output$incompleteCasesBoolRV <- reactive(any(incompleteCases()))
  outputOptions(output, "incompleteCasesBoolRV", suspendWhenHidden = FALSE)

  ## subsetSelectionTab naInfoBox ----
  output$naInfoBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = sum(incompleteCases()),
      color = if (any(incompleteCases())) "yellow" else "green",
      subtitle = "rows with missing values in this subset",
      icon = icon("exclamation-triangle"))
  })

  ## subsetSelectionTab naTable ----
  output$naTable <- renderUI({
    HTML(makeKable(data.frame(NAs = colSums(is.na(userDataChosen())))))
  })

  ## subsetSelectionTab obsTable ----
  output$obsTable <- renderUI({
    nTotal <- nrow(userDataNA())
    nComplete <- sum(!incompleteCases())

    HTML(makeKable(data.frame(Total = nTotal, Complete = nComplete)))
  })

  ## subsetSelectionTab obsPerGroupTable ----
  output$obsPerGroupTable <- renderUI({
      req(input$groupCol)

      if (input$groupCol != "noGroupSelected") {
        HTML(makeKable(t(table(userDataChosen()[, input$groupCol], useNA = "ifany"))))
      } else {
        helpText("No group column selected.")
      }
  })

  ## subsetSelectionTab observeEvent subsetSelectButton ----
  observeEvent(input$subsetSelectButton, {
    appStage("statistics")

    fimlRV(any(incompleteCases()) && isTRUE(input$useFIML))

    if (input$groupCol != "noGroupSelected") {

      subset <- userDataChosen()[, input$groupCol] %in% input$groups
      select <- c(input$groupCol, input$itemCols)
    } else {

      subset <- rep(TRUE, nrow(userDataChosen()))
      select <- input$itemCols
    }

    userDataGroup(
      subset(
        userDataChosen(),
        subset = subset,
        select = select))

    validGroupsRV(
      input$groupCol != "noGroupSelected" &&
        !any(c(table(userDataGroup()[, input$groupCol])) == 1) &&
        length(input$groups) > 1)

    if (validGroupsRV()) {
      shinyjs::enable("doMg")

      updateCheckboxInput(
        session,
        "doMg",
        value = TRUE)
    }

    if (fimlRV()) {
      updateRadioButtons(
        inputId = "corrIndEst",
        choices = c("(Full Information) Maximum Likelihood" = "ML",
                    "Robust (Full Information) Maximum Likelihood" = "MLR"))
      shinyjs::show(id = "corrTabNA")

      updateRadioButtons(
        inputId = "estimator",
        choices = c("(Full Information) Maximum Likelihood" = "ML",
                    "Robust (Full Information) Maximum Likelihood" = "MLR"))
    }

    if (any(incompleteCases())) {
      notifications$notList$NAhand <- shinydashboard::notificationItem(
        text = HTML("For all plots and the multivariate normality analyses<br/>
                      rows with missing values have been removed."),
        icon = icon("exclamation-triangle"),
        status = "warning")

      showNotification(
        ui = "For all plots and the multivariate normality analyses
                rows with missing values have been removed.",
        duration = 5,
        id = "NAremovedNot",
        type = "warning")
    }
  })

  # statisticsTab ----
  ## statisticsTab descrBox ----
  output$descrBox <- renderUI({
    req(userDataGroup())

    table <- t(apply(
      userDataGroup()[, input$itemCols],
      MARGIN = 2,
      FUN = function(col) {
        c(Mean = mean(col, na.rm = TRUE),
          Sd = stats::sd(col, na.rm = TRUE),
          Skew = moments::skewness(col, na.rm = TRUE),
          Excess = moments::kurtosis(col, na.rm = TRUE) - 3)
      }
    )) # t(apply(

    nHeader <- c(1, 4)
    names(nHeader) <- c(" ", sprintf("n<sub>all</sub> = %i", nrow(userDataGroup())))

    overallDescrTable <- makeKable(table, bold_cols = 1) %>%
      kableExtra::add_header_above(header = nHeader, escape = FALSE) %>%
      HTML()

    ## descrBox if (validGroupsRV()) { ----
    if (validGroupsRV()) {
      groups <- unique(userDataGroup()[, input$groupCol])

      mgDescrTableList <- lapply(
        groups,
        function(group) {
          t(apply(
              subset(
                userDataGroup()[, input$itemCols],
                userDataGroup()[, input$groupCol] == group),
              MARGIN = 2,
              FUN = function(col) {
                c(Mean = mean(col, na.rm = TRUE), SD = stats::sd(col, na.rm = TRUE),
                  Skew = moments::skewness(col, na.rm = TRUE),
                  Excess = moments::kurtosis(col, na.rm = TRUE) - 3)
              }
          )) # t(apply(
        }
      ) # lapply

      descrGroupHeader <- c(1, rep(4, length(groups)))
      names(descrGroupHeader) <- c(
        " ",
        sprintf(
          "Group: %s (n<sub>%s</sub> = %i)",
          groups,
          groups,
          c(table(userDataGroup()[, input$groupCol]))[as.character(groups)]))

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
        title = "Descriptive statistics:",
        side = "right",

        tabPanel(
          "Overall",
          overallDescrTable),

        tabPanel(
          "Group-wise",
          tagList(do.call(HTML, mgDescrTableListTagged)))

      ) # tabBox

    } ## descrBox if (!validGroupsRV()) ----
    else {

      shinydashboard::box(
        width = 6,
        title = "Descriptive statistics:",
        overallDescrTable)
    }
  })

  ## statisticsTab histogram plots ----
  # output$histBox further down draws the box and leaves empty plot slots -> these fill them.
  output$singleHist <- renderPlot({
    # histBox builds the dropdown and the slider, so they do not exist on the first run.
    req(userDataGroup(), input$histItem, input$singleNoBins)

    ggplot2::ggplot(
      data.frame(item = stats::na.omit(userDataGroup()[, input$histItem])),
      ggplot2::aes(x = .data$item)) +

      ggplot2::geom_histogram(
        if (input$singleDens) ggplot2::aes(y = ggplot2::after_stat(.data$density)),
        color = "white",
        fill = "#438BCA",
        bins = input$singleNoBins) +

      ggplot2::xlab(input$histItem) +
      ggplot2::theme_classic() +

      if (input$singleDens)
        # the bars' "#438BCA" mixed 40% toward white
        ggplot2::geom_density(color = "#8EB9DF", linewidth = 1)
  })

  output$groupHist <- renderPlot({
    req(userDataGroup(), input$histItemGroup, input$groupNoBins, input$histGroupGroups)

    ggplot2::ggplot(
      subset(
        userDataGroup(),
        subset = userDataGroup()[, input$groupCol] %in% input$histGroupGroups,
        select = c(input$groupCol, input$histItemGroup)) %>%
        stats::na.omit() %>%
        stats::setNames(nm = c("group", "item")),
      ggplot2::aes(x = .data$item, fill = .data$group)) +

      ggplot2::geom_histogram(
        if (input$groupDens) ggplot2::aes(y = ggplot2::after_stat(.data$density)),
        color = "white",
        bins = input$groupNoBins,
        position = "dodge") +

      ggplot2::xlab(input$histItemGroup) +
      ggplot2::scale_fill_manual(values = groupColors()$solid, name = input$groupCol) +
      ggplot2::theme_classic() +

      if (input$groupDens)
        list(
          ggplot2::geom_density(
            ggplot2::aes(color = .data$group),
            fill = NA,
            linewidth = 1),
          ggplot2::scale_color_manual(values = groupColors()$light, name = input$groupCol))
  })

  ## statisticsTab histBox ----
  output$histBox <- renderUI({

    ## histBox if (validGroupsRV()) ----
    if (validGroupsRV()) {

      ### histBox tabBox ----
      shinydashboard::tabBox(
        title = "Histogram:",
        side = "right",

        #### histBox tabBox overall panel ----
        tabPanel(
          title = "Overall",

          fluidRow(
            column(
              width = 6,
              selectInput("histItem", "Select the item:", input$itemCols))),

          plotOutput("singleHist"),

          fluidRow(
            column(
              width = 6,
              sliderInput(
                "singleNoBins",
                "Choose the number of bins:",
                min = 1, max = 100, value = 30, step = 1)),
            column(
              width = 6,
              checkboxInput(
                "singleDens",
                "Overlay a density curve",
                value = FALSE)))),

        #### histBox tabBox group-wise panel ----
        tabPanel(
          title = "Group-wise",

          fluidRow(
            column(
              width = 6,
              selectInput(
                "histItemGroup",
                "Select the item:",
                choices = input$itemCols)),
            column(
              width = 6,
              checkboxGroupInput(
                "histGroupGroups",
                "Select the groups to include:",
                choices = unique(userDataGroup()[, input$groupCol]),
                selected = unique(userDataGroup()[, input$groupCol]),
                inline = TRUE))),

          plotOutput("groupHist"),

          fluidRow(
            column(
              width = 6,
              sliderInput(
                "groupNoBins",
                "Choose the number of bins:",
                min = 1, max = 100, value = 30, step = 1)),
            column(
              width = 6,
              checkboxInput(
                "groupDens",
                "Overlay a density curve",
                value = FALSE)))
        ) # tabPanel
      ) # tabBox

    } ## histBox if (!validGroupsRV()) ----
    else {

      shinydashboard::box(
        title = "Histogram:",

        fluidRow(
          column(
            width = 6,
            selectInput(
              "histItem",
              "Select the item:",
              choices = input$itemCols))),

        plotOutput("singleHist"),

        fluidRow(
          column(
            width = 6,
            sliderInput(
              "singleNoBins",
              "Choose the number of bins:",
              min = 1, max = 100, value = 30, step = 1)),
          column(
            width = 6,
            checkboxInput(
              "singleDens",
              "Overlay a density curve",
              value = FALSE)))

      ) # box
    }
  })

  ## statisticsTab covMatBox ----
  output$covMatBox <- renderUI({
    req(userDataGroup())

    table <- stats::cov(userDataGroup()[, input$itemCols], use = "pairwise.complete.obs")
    table[upper.tri(table)] <- NA

    ## covMatBox if (validGroupsRV()) ----
    if (validGroupsRV()) {
      groups <- unique(userDataGroup()[, input$groupCol])

      mgCovMatList <- lapply(
        groups,
        function(group) {
          stats::cov(
            subset(
              userDataGroup()[, input$itemCols],
              userDataGroup()[, input$groupCol] == group),
            use = "pairwise.complete.obs")
        })

      for (i in 1:length(mgCovMatList))
        mgCovMatList[[i]][upper.tri(mgCovMatList[[i]])] <- NA

      mgCovMatTable <- makeKable(do.call(rbind, mgCovMatList),
                                            bold_cols = 1)

      groupRowHeaders <- sprintf(
        "Group: %s (n = %i)",
        groups,
        c(table(userDataGroup()[, input$groupCol]))[as.character(groups)])

      for (i in 1:length(groups))
        mgCovMatTable <- mgCovMatTable %>%
          kableExtra::group_rows(
            group_label = groupRowHeaders[i],
            start_row = (i - 1) * length(input$itemCols) + 1,
            end_row = i * length(input$itemCols),
            label_row_css = "background-color: #666; color: #fff;")

      # output if groups
      shinydashboard::tabBox(
        width = 12,
        title = "Covariance matrix:",
        side = "right",

        tabPanel(
          title = "Overall",
          makeKable(table, bold_cols = 1) %>%
            HTML()),

        tabPanel(
          "Group-wise",
          HTML(mgCovMatTable))

      ) # tabBox

    } ## covMatBox if (!validGroupsRV()) ----
    else {

      # output if NO groups
      shinydashboard::box(
        width = 12,
        title = "Covariance matrix:",

        makeKable(table, bold_cols = 1) %>%
          HTML()

      ) # box
    }
  })

  # corrTab ----
  ## corrTab corrInd ----
  output$corrInd <- renderUI({

    req(userDataGroup())
    req(input$corrIndEst)

    dummyModel <- paste(
      sprintf("%s ~ 1", colnames(userDataGroup()[, input$itemCols])),
      collapse = "\n")

    corrIndRaw <- tryCatch(
      lavaan::cfa(
        model = dummyModel,
        data = userDataGroup(),
        estimator = input$corrIndEst,
        missing = ifelse(fimlRV(), "fiml", "listwise")),
      warning = function(w) w,
      error = function(e) e)

    ## corrInd if (class(corrIndRaw)[1] == "lavaan") ---
    if (class(corrIndRaw)[1] == "lavaan") {

      corrInd <- unlist(extractFitIndices(corrIndRaw)[, c(2, 1, 3)])

      if (!is.na(input$corrIndSL) && input$corrIndSL < 1 && input$corrIndSL > 0) {

        tagList(
          strong("Test result:"),

          sprintf(
            ifelse(
              corrInd[3] < input$corrIndSL,
              yes = "The hypothesis that all correlations are equal to
                      zero has to be discarded on a significance level of
                      %s (%s-&chi;&sup2; = %.3f, df = %i, p %s).",
              no = "The hypothesis that all correlations are equal to
                    zero can be maintained on a significance level of
                    %s (%s-&chi;&sup2; = %.3f, df = %i, p %s)."),
            input$corrIndSL, # %s
            paste0(if (fimlRV()) "FI", input$corrIndEst), # %s
            corrInd[1], # %.3f
            corrInd[2], # %i
            ifelse(corrInd[3] < 0.001, "< 0.001", sprintf("= %.3f", corrInd[3]))) %>%

            HTML() %>%
            p()

        ) # tagList

      } else {
        HTML("Please enter a valid significance level") %>%
          div(style = "color:red")
      }

    } ## corrInd if (class(corrIndRaw)[1] != "lavaan") ----
    else {
      tagList(
        strong("Test result:"),
        paste("There was an ERROR/WARNING:", corrIndRaw$message) %>%
          HTML() %>%
          div(style = "color:red"))
    }
  })

  ## corrTab scatter plots ----
  output$singleScatter <- renderPlot({
    req(userDataGroup(), input$scatterItemX, input$scatterItemY)

    ggplot2::ggplot(
        data.frame(
            itemX = userDataGroup()[, input$scatterItemX],
            itemY = userDataGroup()[, input$scatterItemY]) %>%
          stats::na.omit(),
        ggplot2::aes(x = .data$itemX, y = .data$itemY)) +

        ggplot2::geom_point(color = "#438BCA") +
        ggplot2::xlab(input$scatterItemX) +
        ggplot2::ylab(input$scatterItemY) +
        ggplot2::theme_classic()
  })

  output$groupScatter <- renderPlot({
    req(userDataGroup(), input$scatterItemXGroup, input$scatterItemYGroup,
        input$scatterGroupGroups)

    ggplot2::ggplot(
      subset(
        userDataGroup(),
        subset = userDataGroup()[, input$groupCol] %in% input$scatterGroupGroups,
        select = c(input$groupCol, input$scatterItemXGroup, input$scatterItemYGroup)) %>%
        stats::na.omit() %>%
        stats::setNames(nm = c("group", "itemX", "itemY")),
      ggplot2::aes(x = .data$itemX, y = .data$itemY, color = .data$group)) +

      ggplot2::geom_point() +
      ggplot2::xlab(input$scatterItemXGroup) +
      ggplot2::ylab(input$scatterItemYGroup) +
      ggplot2::scale_color_manual(values = groupColors()$solid, name = input$groupCol) +
      ggplot2::theme_classic()
  })

  ## mvnTab multivariate plot ----
  output$mvnPlot <- renderPlot({
    req(userDataGroup(), input$mvnPlotType)
    if (input$mvnPlotType != "qq") req(input$mvnItemX, input$mvnItemY)

    userDataNAOmit <- stats::na.omit(userDataGroup())

    if (input$mvnPlotType == "qq") {
      MVN::multivariate_diagnostic_plot(
        stats::na.omit(userDataNAOmit[, input$itemCols]),
        type = "qq")

    } else if (input$mvnPlotType == "persp") {
      graphics::persp(x = MASS::kde2d(userDataNAOmit[, input$mvnItemX],
                            userDataNAOmit[, input$mvnItemY],
                            n = 100),
            theta = 1, phi = 30, border = NA, shade = 0.5, box = T,
            xlab = input$mvnItemX,
            ylab = input$mvnItemY,
            zlab = "Density")

    } else if (input$mvnPlotType == "contour") {
      graphics::contour(x = MASS::kde2d(userDataNAOmit[, input$mvnItemX],
                              userDataNAOmit[, input$mvnItemY],
                              n = 100),
              nlevels = 20,
              xlab = input$mvnItemX,
              ylab = input$mvnItemY)
    }
  })

  ## corrTab scatterBox ----
  output$scatterBox <- renderUI({

    req(userDataGroup())

    ## scatterBox if (validGroupsRV()) ----
    if (validGroupsRV()) {

      ### scatterBox tabBox ----
      shinydashboard::tabBox(
        title = "Scatter plot:",
        width = NULL,
        side = "right",

        #### scatterBox tabBox overall panel ----
        tabPanel(
          title = "Overall",

          fluidRow(

            column(
              width = 4,
              selectInput(
                "scatterItemX",
                "Select item on the abscissa:",
                choices = input$itemCols)),
            column(
              width = 4,
              selectInput(
                "scatterItemY",
                "Select item on the ordinate:",
                choices = input$itemCols,
                selected = input$itemCols[2]))),

          plotOutput("singleScatter")),

        #### scatterBox tabBox group-wise panel ----
        tabPanel(
          title = "Group-wise",

          fluidRow(

            column(
              width = 4,
              selectInput(
                "scatterItemXGroup",
                "Select item on the abscissa:",
                choices = input$itemCols)),
            column(
              width = 4,
              selectInput(
                "scatterItemYGroup",
                "Select item on the ordinate:",
                choices = input$itemCols,
                selected = input$itemCols[2])),
            column(
              width = 4,
              checkboxGroupInput(
                "scatterGroupGroups",
                "Select the groups to include:",
                choices = unique(userDataGroup()[, input$groupCol]),
                selected = unique(userDataGroup()[, input$groupCol]),
                inline = TRUE))),

          plotOutput("groupScatter"))
      ) # tabBox

    } ## scatterBox if (!validGroupsRV()) ----
    else {

      shinydashboard::box(
        title = "Scatter plot:",
        width = NULL,

        fluidRow(

          column(
            width = 4,
            selectInput(
              "scatterItemX",
              "Select item on the abscissa:",
              choices = input$itemCols)),
          column(
              width = 4,
              selectInput(
                "scatterItemY",
                "Select item on the ordinate:",
                choices = input$itemCols,
                selected = input$itemCols[2]))),

        plotOutput("singleScatter")
      ) # box
    }
  })

  ## corrTab corrTableBox ----
  output$corrTableBox <- renderUI({
    req(userDataGroup())

    ## corrTableBox create raw cor table and test for errors ----
    corrTableWithCIsRaw <- list(
      cor = tryCatch(
        stats::cor(userDataGroup()[, input$itemCols],
            use = input$corrTabNA),
        warning = function(w) NULL,
        error = function(e) NULL),
      test = tryCatch(
        corrplot::cor.mtest(userDataGroup()[, input$itemCols],
                            conf.level = (1 - input$corrTabSL)),
        warning = function(w) w,
        error = function(e) e))

    corrTableLegend <- tagList(
      # h5("Legend:"),

      cbind(
        kableExtra::cell_spec(
          "Legend:"),
        kableExtra::cell_spec(
          "Sig. pos.",
          color = textColor,
          background = goodColor),
        kableExtra::cell_spec(
          "Sig. neg.",
          color = textColor,
          background = badColor),
        kableExtra::cell_spec(
          "Not sig.",
          color = textColor,
          background = neutrColor)) %>%

        makeKable(
          # bootstrap_options = "bordered",
          position = "left") %>%
        HTML()

    ) # tagList

    ## corrTableBox singleCorrTable if no errors: ----
    if (class(corrTableWithCIsRaw$test)[1] == "list") {

      singleCorrTable <- makeCorrTableWithCIs(
        rawTable = corrTableWithCIsRaw,
        goodColor,
        badColor,
        neutrColor,
        textColor,
        sigLvl = input$corrTabSL,
        itemCols = input$itemCols) %>%

        makeKable(
          bootstrap_options = c("condensed", "striped"),
          bold_cols = 1) %>%
        HTML()

    } ## corrTableBox singleCorrTable if errors: ----
    else {
      singleCorrTable <-
        paste("There was an ERROR/WARNING:", corrTableWithCIsRaw$test) %>%
        HTML() %>%
        div(style = "color:red")
    }

    ## corrTableBox if groups ----
    if (validGroupsRV()) {
      mgCorrTableList <- lapply(
        unique(userDataGroup()[, input$groupCol]),
        function(group) {

          makeCorrTableWithCIs(

            rawTable = list(
              cor = suppressWarnings(stats::cor(
                subset(
                  userDataGroup()[, input$itemCols],
                  userDataGroup()[, input$groupCol] == group),
                use = input$corrTabNA)),
              test = corrplot::cor.mtest(
                subset(
                  userDataGroup()[, input$itemCols],
                  userDataGroup()[, input$groupCol] == group),
                conf.level = (1 - input$corrTabSL))),

            goodColor,
            badColor,
            neutrColor,
            textColor,
            sigLvl = input$corrTabSL,
            itemCols = input$itemCols)
      })

      # join each group corrTable
      mgCorrTable <- makeKable(
        do.call(rbind, mgCorrTableList),
        bootstrap_options = c("condensed", "striped"),
        bold_cols = 1)

      # add group headers
      groupRowHeaders <- sprintf("Group: %s", unique(userDataGroup()[, input$groupCol]))

      for (i in 1:length(unique(userDataGroup()[, input$groupCol])))
        mgCorrTable <- mgCorrTable %>%
          kableExtra::group_rows(
            group_label = groupRowHeaders[i],
            start_row = (i - 1) * length(input$itemCols) * 2 + 1,
            end_row = i * length(input$itemCols) * 2,
            label_row_css = "background-color: #666; color: #fff;")

      # assemble in tabBox
      shinydashboard::tabBox(
        width = 12,
        title = "Correlation table with confidence intervals:",
        side = "right",

        tabPanel(
            "Overall",
            singleCorrTable,
            br(),
            HTML(makeLegend("corrTable", estimatorName(), input$sigLvl,
                            goodColor, badColor, neutrColor, textColor))),
        tabPanel(
            "Group-wise",
            HTML(mgCorrTable),
            br(),
            HTML(makeLegend("corrTable", estimatorName(), input$sigLvl,
                            goodColor, badColor, neutrColor, textColor)))

      ) # tabBox

    } ## corrTableBox if no groups ----
    else {

      shinydashboard::box(
          width = 12,
          title = "Correlation table with confidence intervals:",

          singleCorrTable,
          br(),
          corrTableLegend)
    }
  })

  # observeEvent input$estimator ----
  observeEvent(input$estimator, {
    mvnTestResult$estimator <- input$estimator

    # The models on screen were fitted with whichever estimator was chosen at the time, so
    # a change after a run means they no longer match the setting -> a refit is pending.
    if (!is.null(modelFitsRV())) refitPendingRV(TRUE)
  })

  # The pending refit: a note and a mark on the button ----
  observeEvent(refitPendingRV(), {
    updateActionButton(
      session,
      "goModels",
      label = paste0("Test the models", if (refitPendingRV()) " *"))
  })

  output$refitPendingNote <- renderUI({
    if (!refitPendingRV()) return(NULL)

    tagList(
      br(),
      HTML("The estimator has changed. The results still come from the estimator that
            was chosen when the models were last fitted. Press <b>Test the models *</b>
            to fit them again.") %>%
        div(style = "color:orange"))
  })

  # mvnTab ----
  ## mvnTab output mvnTable ----
  output$mvnTable <- renderUI({

    notifications$notList$mvnApp <- shinydashboard::notificationItem(
      text = HTML("For more extensive analyses on multivariate normality,<br/>
                    load() the MVN package and open its shiny app via run_mvn_app()!"),
      icon = icon("lightbulb"),
      status = "success")

    req(userDataGroup())

    mvnTestResult$raw <- tryCatch(
      MVN::mvn(stats::na.omit(userDataGroup()[, input$itemCols]),
               mvn_test = "mardia"),
      warning = function(w) w,
      error = function(e) e)

    # req(mvnTestResult$raw)

    ## mvnTable if result of MVN test is data.frame ----
    if (is.data.frame(mvnTestResult$raw$multivariate_normality)) {

      mvnTestResult$estimator <- ifelse(
        test = is.numeric(mvnTestResult$raw$multivariate_normality[, "p.value"]),

        yes = ifelse(
          test = any(mvnTestResult$raw$multivariate_normality[, "p.value"] < input$mvnSL),
          yes = "MLR",
          no = "ML"),

        no = ifelse(
          test = any(mvnTestResult$raw$multivariate_normality[, "p.value"] == "<0.001"),
          yes = "MLR",
          no = "ML"))

      notifications$notList$estUpdate <- shinydashboard::notificationItem(
        text = "Updated estimator based on MVN test result.",
        icon = icon("exclamation-triangle"),
        status = "warning")

      showNotification(
        ui = "Updated estimator based on MVN test result.",
        duration = 5,
        id = "estUpdateNot",
        type = "warning")

      updateRadioButtons(
        session,
        "estimator",
        selected = mvnTestResult$estimator)
    }

    ## mvnTable if no error ----
    if (class(mvnTestResult$raw)[1] == "mvn") {

      mvnUV <- data.frame(Test = mvnTestResult$raw$univariate_normality$Test,
                          Item = mvnTestResult$raw$univariate_normality$Variable,
                          Statistic = mvnTestResult$raw$univariate_normality$Statistic,
                          p = suppressWarnings(as.numeric(mvnTestResult$raw$univariate_normality$p.value)),
                          stringsAsFactors = F)

      mvnUV$p[is.na(mvnUV$p)] <- 0
      mvnUV$Signif. <- ifelse(mvnUV$p < input$mvnSL, "*", "")
      mvnUV$p <- ifelse(mvnUV$p < 0.001, "< 0.001", sprintf("%.3f", round(mvnUV$p, 3)))

      HTML(makeKable(mvnUV, bootstrap_options = "basic"))

    } ## mvnTable if error ----
    else {
      paste("There was an ERROR/WARNING:", mvnTestResult$raw$message) %>%
        HTML() %>%
        div(style = "color:red")
    }
  })

  ## mvnTab output mvnComment ----
  output$mvnComment <- renderUI({

    req(userDataGroup())

    ## mvnComment if result of MVN test is data.frame ----
    if (is.data.frame(mvnTestResult$raw$multivariate_normality)) {

      mvnMV <- data.frame(Test = mvnTestResult$raw$multivariate_normality$Test,
                          Statistic = mvnTestResult$raw$multivariate_normality$Statistic,
                          p = suppressWarnings(as.numeric(mvnTestResult$raw$multivariate_normality$p.value)),
                          stringsAsFactors = F)

      mvnMV$p[is.na(mvnMV$p)] <- 0
      mvnMV$Signif. <- ifelse(mvnMV$p < input$mvnSL, "*", "")
      mvnMV$p <- ifelse(mvnMV$p < 0.001, "< 0.001", sprintf("%.3f", round(mvnMV$p, 3)))

      if ("*" %in% mvnMV$Signif.) {

        tagList(
          sprintf("At least one of the hypotheses that Mardia's Skewness statistic
                    or Mardias' Kurtosis statistic matches one of a
                    normal distribution has to be discarded on a significance
                    level of %s. Test result:", input$mvnSL),
          HTML(makeKable(mvnMV, bootstrap_options = "basic")),
          HTML("It is thus recommended to continue with the <b>Robust Maximum Likelihood (MLR)</b> estimator."))

      } else {

        tagList(
          sprintf("The hypotheses that Mardia's Skewness statistic
                    and Mardias' Kurtosis statistic match those of a
                    normal distribution can be maintained on a significance
                    level of %s. Test result:", input$mvnSL),
          HTML(makeKable(mvnMV, bootstrap_options = "basic")),
          HTML("It is thus recommended to continue with the <b>Maximum Likelihood (ML)</b> estimator."))
      }
    } ## mvnComment if error ----
    else {
      paste("There was an ERROR/WARNING:", mvnTestResult$raw$message) %>%
        HTML() %>%
        div(style = "color:red")
    }
  })

  ## mvnTab output mvnPlotBox ----
  output$mvnPlotBox <- renderUI({

    shinydashboard::box(
      width = 12,
      title = "Multivariate plot:",

      fluidRow(

        column(
          width = 4,
          selectInput(
            "mvnPlotType",
            "Choose the type of Plot:",
            choices = c(
              "Q-Q Plot (all items)" = "qq",
              "Perspective Plot" = "persp",
              "Contour Plot" = "contour"))),
        column(
          width = 4,
          conditionalPanel(
            "input.mvnPlotType != 'qq'",
            selectInput(
              "mvnItemX",
              "Select item on the abscissa:",
              input$itemCols))),
        column(
          width = 4,
          conditionalPanel(
            "input.mvnPlotType != 'qq'",
            selectInput(
              "mvnItemY",
              "Select item on the ordinate:",
              input$itemCols,
              selected = input$itemCols[2])))

      ), # fluidRow

      plotOutput("mvnPlot")
    ) # box
  })

  # observeEvent input$sigLvl ----
  observeEvent(input$sigLvl, {
    if ((input$sigLvl < 0.001 | input$sigLvl > 1) && !is.na(input$sigLvl))
      updateNumericInput(session, "sigLvl", value = 0.05)
  })

  # observeEvent input$rmseaCiLvl ----
  observeEvent(input$rmseaCiLvl, {
    if ((input$rmseaCiLvl < 0.5 | input$rmseaCiLvl > 0.999) && !is.na(input$rmseaCiLvl))
      updateNumericInput(session, "rmseaCiLvl", value = 0.9)
  })

  # The two display settings ----
  # Both are read all over the results tabs, and both can be changed after a run without
  # refitting anything. Emptying either box sends NA, which would turn every table into an
  # error message -> hold the tables back until there is a number again.
  sigLvl <- reactive({
    req(!is.na(input$sigLvl))
    input$sigLvl
  })

  rmseaCiLvl <- reactive({
    req(!is.na(input$rmseaCiLvl))
    input$rmseaCiLvl
  })

  # observeEvent input$goModels ----
  # Pressing "Test the models" fits the models and stores them, and does nothing else.
  # Everything drawn from them is built under "Results" below and redraws on its own.
  observeEvent(input$goModels, tryCatch({
    output$goModelsError <- renderUI(NULL)

    # Whatever is on screen was fitted with the settings as they were before this press.
    modelFitsRV(NULL)

    # Freeze the multigroup choice, then reveal the results entries in the sidebar.
    doMgRV(isTRUE(input$doMg))
    appStage("results")

    modelsToTest <- models[sapply(models, function(thisModel) input[[thisModel]])]
    comps <- possComps[sapply(possComps, function(thisComp) input[[thisComp]])]

    # Where the data came from, so makeRCode() can write the matching
    # read.csv() / read_spss() / workspace line into the exported script.
    dataSource <- switch(
      input$source,
      "Workspace" = list(type = "Workspace", object = input$objectFromWorkspace),
      "CSV" = list(type = "CSV",
                   name = input$CSVFile$name,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote),
      "SPSS" = list(type = "SPSS", name = input$SPSSFile$name))

    # The name of the data set, used as the first part of the factor score filename.
    dataName <- switch(
      input$source,
      "Workspace" = input$objectFromWorkspace,
      "CSV" = gsub("\\.csv", "", input$CSVFile$name),
      "SPSS" = gsub("\\.sav|\\.zsav|\\.por", "", input$SPSSFile$name))

    # TRUE when the user left some of the groups out, so the exported script has to
    # subset the data before fitting.
    isSubset <- (
      validGroupsRV() &&
        (length(unique(userDataGroup()[, input$groupCol])) <
           length(unique(userDataRaw()[, input$groupCol]))))

    # Which group each row belongs to, for the predicted factor scores. FALSE when the
    # user chose no group column, because then the data has no such column to read.
    groupValues <- if (input$groupCol == "noGroupSelected") {
      FALSE
    } else {
      userDataGroup()[, input$groupCol]
    }

    ## test the models! ----
    # One pass over the whole sample, plus one fitting the groups separately if the user
    # asked for that.
    passes <- list(single = FALSE)
    if (isTRUE(input$doMg)) passes$multigroup <- input$groupCol

    modelFitsRV(lapply(passes, function(groupName) {

      ### try fitting and capture warning and error messages ----
      modelCodes <- makeModelCodes(inputData = userDataGroup(),
                                              itemCols = input$itemCols,
                                              group = groupName,
                                              etaIntFree = as.logical(input$etaIntFree))

      #### fit each model once, keeping both a warning and the completed fit ----
      # Fit each chosen model:
      #   warning -> note it, carry on, keep the fit
      #   error   -> keep the error message instead of a fit
      # group and group.equal are NULL without a grouping column, so one call does both.
      fittedModelsWarns <- lapply(
        modelCodes[modelsToTest],
        FUN = function(model) {
          warnCond <- NULL
          fit <- withCallingHandlers(
            tryCatch(lavaan::lavaan(model = model,
                                    data = userDataGroup(),
                                    meanstructure = TRUE,
                                    group = if (isFALSE(groupName)) NULL else groupName,
                                    group.equal = if (isFALSE(groupName)) NULL else c("loadings", "intercepts"),
                                    estimator = mvnTestResult$estimator,
                                    missing = ifelse(fimlRV(), "fiml", "listwise"),
                                    int.ov.free = TRUE,
                                    int.lv.free = as.logical(input$etaIntFree),
                                    auto.fix.first = TRUE,
                                    auto.fix.single = TRUE,
                                    auto.var = TRUE,
                                    auto.cov.lv.x = TRUE,
                                    auto.efa = TRUE,
                                    auto.th = TRUE,
                                    auto.delta = TRUE,
                                    auto.cov.y = TRUE),
                     error = function(e) e),
            warning = function(w) {
              warnCond <<- w
              invokeRestart("muffleWarning")
            })
          attr(fit, "shinyCTTwarning") <- warnCond
          fit
        })

      #### warning and error counting and capturing ----
      errs <- sapply(fittedModelsWarns, inherits, what = "error")
      warns <- sapply(fittedModelsWarns, function(f) !is.null(attr(f, "shinyCTTwarning")))

      # A model that only warns is still usable - its fit was kept above - so only a
      # genuine error excludes it from goodModels. warnModels is purely informational now.
      goodModels <- modelsToTest[!errs]
      errModels <- modelsToTest[errs]
      warnModels <- modelsToTest[warns]

      #### the successive comparisons along the hierarchy ----
      succTable <- list()

      if (length(goodModels) > 1 && !identical(goodModels, c(teq = "teq", etp = "etp"))) {

        if ("teq" %in% goodModels) {

          succTable$teq <- do.call(
            lavaan::lavTestLRT,
            args = c(object = fittedModelsWarns[[goodModels[1]]],
                     ... = fittedModelsWarns[goodModels[-c(1, which(goodModels == "etp"))]]))

          rownames(succTable$teq) <- goodModels[which(goodModels != "etp")]
        }

        if ("etp" %in% goodModels) {
          succTable$etp <- do.call(
            lavaan::lavTestLRT,
            args = c(object = fittedModelsWarns[[goodModels[1 + (goodModels[1] == "teq")]]],
                     ... = fittedModelsWarns[goodModels[-c(1 + (goodModels[1] == "teq"),
                                                           which(goodModels == "teq"))]]))

          rownames(succTable$etp) <- goodModels[which(goodModels != "teq")]
        }

        if (!any(c("teq", "etp") %in% goodModels)) {
          succTable$teq <- do.call(
            lavaan::lavTestLRT,
            args = c(object = fittedModelsWarns[[goodModels[1]]],
                     ... = fittedModelsWarns[goodModels[-1]]))

          rownames(succTable$teq) <- goodModels
        }
      }

      #### everything the results tabs need from this pass ----
      # The settings are kept alongside the fits. The estimator can be changed after a
      # run, and the tables must go on reporting the one that actually produced them.
      list(
        groupName     = groupName,
        modelCodes    = modelCodes,
        fittedModels  = fittedModelsWarns,
        errs          = errs,
        warns         = warns,
        goodModels    = goodModels,
        errModels     = errModels,
        warnModels    = warnModels,
        comps         = comps,
        succTable     = succTable,
        estimator     = mvnTestResult$estimator,
        estimatorName = estimatorName(),
        missingMethod = ifelse(fimlRV(), "fiml", "listwise"),
        itemCols      = input$itemCols,
        groupCol      = input$groupCol,
        groups        = input$groups,
        groupValues   = groupValues,
        dataSource    = dataSource,
        dataName      = dataName,
        isSubset      = isSubset)
    }))

    # The fits now match the estimator that is selected.
    refitPendingRV(FALSE)
  },

  ## observeEvent input$goModels error handler ----
  # Anything goes wrong above -> show the message under the button, go back one stage.
  error = function(e) {
    # Back to "statistics", so the results entries disappear from the sidebar again.
    appStage("statistics")
    doMgRV(FALSE)

    # The lockout only ever disables, so switch these back on by hand. The multigroup box
    # only if there is a usable group column.
    shinyjs::enable("etaIntFree")
    if (isTRUE(validGroupsRV())) shinyjs::enable("doMg")

    output$goModelsError <- renderUI(
      tagList(
        br(),
        strong("The model tests failed:"),
        paste("There was an ERROR:", conditionMessage(e)) %>%
          HTML() %>%
          div(style = "color:red")))
  })) # observeEvent(input$goModels, {

  # Results ----
  # Everything below is built once, when the app starts, and draws itself from whatever
  # modelFitsRV() holds. Nothing here fits anything, so changing the significance level or
  # the confidence level of the RMSEA interval only redraws the tables.
  #
  # The same code runs twice, once per pass: the whole sample writes to "modelTests",
  # "parTables" and so on, the group-wise fit to the same ids with "Mg" on the end.
  lapply(names(passSuffixes), function(thisPass) {

    groupAppend <- passSuffixes[[thisPass]]

    modelTestsContStr <- paste0("modelTestsCont", groupAppend)
    hierPlotStr <- paste0("hierPlot", groupAppend)
    hierTableStr <- paste0("hierTable", groupAppend)
    fitsTableStr <- paste0("fitsTable", groupAppend)
    combCompTableStr <- paste0("combCompTable", groupAppend)
    infCompTableStr <- paste0("infCompTable", groupAppend)

    hierTableLegendStr <- paste0("hierTableLegend", groupAppend)
    fitsTableLegendStr <- paste0("fitsTableLegend", groupAppend)
    combCompTableLegendStr <- paste0("combCompTableLegend", groupAppend)
    infCompTableLegendStr <- paste0("infCompTableLegend", groupAppend)

    parTabsetStr <- paste0("parTabset", groupAppend)
    fsTabsetStr <- paste0("fsTabset", groupAppend)
    mcTabsetStr <- paste0("mcTabset", groupAppend)

    ## This pass's fits ----
    # req() holds every output below back until the button has been pressed - and, for the
    # group-wise pass, until the user has asked for one.
    passFit <- reactive(req(modelFitsRV()[[thisPass]]))

    ## The fit indices of every model that fitted ----
    # The confidence level of the RMSEA interval is a display choice, so these are worked
    # out again whenever the user changes it. No model is refitted.
    passFits <- reactive(
      do.call(rbind, lapply(passFit()$fittedModels[passFit()$goodModels],
                            extractFitIndices,
                            rmseaCiLevel = rmseaCiLvl())))

    ## The three comparison matrices ----
    # One cell per pair of models: the model's own chi-square on the diagonal, the
    # difference against an earlier model to the left of it, and the same layout again for
    # AIC and BIC. The chi-square cells are coloured by significance, so all of this is
    # redrawn when the significance level changes.
    compMatrices <- reactive({
      fits <- passFits()
      fittedModels <- passFit()$fittedModels
      goodModels <- passFit()$goodModels
      comps <- passFit()$comps

      # Cells are addressed by pair, "etetko" being the ess. tau-equivalent model against
      # the tau-congeneric one. Anything still empty at the end prints as a blank cell.
      cellNames <- outer(models, models, paste0)

      chisqCells <- dfCells <- aicCells <- bicCells <-
        stats::setNames(rep(NA_character_, 25), cellNames)

      # Comparing A with B is the same test as comparing B with A, so only the diagonal
      # and the cells left of it are used. Those start as a grey X and are overwritten
      # below wherever there is something to write.
      chisqCells[lower.tri(diag(5), diag = TRUE)] <-
        aicCells[lower.tri(diag(5), diag = TRUE)] <-
        bicCells[lower.tri(diag(5), diag = TRUE)] <- "<span style=\"color: lightgrey;\" >X</span>"

      for (thisModel in goodModels) {

        whichModel <- which(goodModels == thisModel)
        thisModelStr <- paste0(thisModel, thisModel)

        ### write to diag(chisq comp table) ----
        if (fits[thisModel, "pvalue"] < sigLvl()) {
          sigAddon <- "*"
          sigColor <- badColor

          if (fits[thisModel, "pvalue"] < 0.01)
            sigAddon <- paste0(sigAddon, "*")

          if (fits[thisModel, "pvalue"] < 0.001)
            sigAddon <- paste0(sigAddon, "*")

        } else {

          sigAddon <- ""
          sigColor <- goodColor
        }

        chisqCells[thisModelStr] <-
          kableExtra::cell_spec(
            sprintf(paste0("%.2f", sigAddon), fits[thisModel, "chisq"]),
            background = sigColor,
            color = textColor,
            italic = TRUE)

        dfCells[thisModelStr] <-
          kableExtra::cell_spec(
            sprintf("%i", fits[thisModel, "df"]),
            background = sigColor,
            color = textColor,
            italic = TRUE)

        ### write to AIC/BIC comp table ----
        aicCells[thisModelStr] <-
          kableExtra::cell_spec(
            sprintf("%.1f", fits[thisModel, "aic"]),
            color = textColor,
            background = neutrColor)

        bicCells[thisModelStr] <-
          kableExtra::cell_spec(
            sprintf("%.1f", fits[thisModel, "bic"]),
            color = textColor,
            background = neutrColor)

        #### if there is more than one good model ----
        if (whichModel > 1) {

          aicDiffs <- fits[thisModel, "aic"] - fits[1:(whichModel - 1), "aic"]
          bicDiffs <- fits[thisModel, "bic"] - fits[1:(whichModel - 1), "bic"]

          aicCells[paste0(thisModel, rownames(fits)[1:(whichModel - 1)])] <-
            kableExtra::cell_spec(
              sprintf(ifelse(aicDiffs < 0, "%.1f", "+%.1f"), aicDiffs),
              color = textColor,
              background = ifelse(aicDiffs < 0, goodColor, badColor))

          bicCells[paste0(thisModel, rownames(fits)[1:(whichModel - 1)])] <-
            kableExtra::cell_spec(
              sprintf(ifelse(bicDiffs < 0, "%.1f", "+%.1f"), bicDiffs),
              color = textColor,
              background = ifelse(bicDiffs < 0, goodColor, badColor))
        }

        ### write to lower.tri(chisq comp table) ----
        compsWithThisModel <- substring(
          text = comps[grep(thisModel, substr(comps, 1, 3))],
          first = 4,
          last = 6)

        compsWithThisModel <- compsWithThisModel[compsWithThisModel %in% goodModels]
        names(compsWithThisModel) <- compsWithThisModel

        fitCompsWithThisModel <- sapply(
          compsWithThisModel,
          function(thisComp) {
            tmpTbl <- lavaan::lavTestLRT(fittedModels[[thisModel]], fittedModels[[thisComp]])
            unlist(tmpTbl[2, c("Chisq diff", "Df diff", "Pr(>Chisq)")])
        })

        for (thisComp in compsWithThisModel) {

          if (fitCompsWithThisModel["Pr(>Chisq)", thisComp] < sigLvl()) {
            sigAddon <- "*"
            sigColor <- badColor

            if (fitCompsWithThisModel["Pr(>Chisq)", thisComp] < 0.01)
              sigAddon <- paste0(sigAddon, "*")

            if (fitCompsWithThisModel["Pr(>Chisq)", thisComp] < 0.001)
              sigAddon <- paste0(sigAddon, "*")

          } else {

            sigAddon <- ""
            sigColor <- goodColor
          }

          thisModelCompStr <- paste0(thisModel, thisComp)

          chisqCells[thisModelCompStr] <- kableExtra::cell_spec(
            sprintf(paste0("+%.2f", sigAddon), fitCompsWithThisModel["Chisq diff", thisComp]),
            background = sigColor,
            color = textColor)

          dfCells[thisModelCompStr] <- kableExtra::cell_spec(
            sprintf("+%i", fitCompsWithThisModel["Df diff", thisComp]),
            background = sigColor,
            color = textColor)
        }
      }

      ### the df and the chi-square of one pair go in two columns side by side ----
      combCompTable <- matrix(NA, nrow = 5, ncol = 10)
      combCompTable[, seq(1, 10, 2)] <- matrix(dfCells, nrow = 5, ncol = 5)
      combCompTable[, seq(2, 10, 2)] <- matrix(chisqCells, nrow = 5, ncol = 5)

      rownames(combCompTable) <- modelsAbbrev
      colnames(combCompTable) <- rep(
        c("&Delta;df", paste0(passFit()$estimatorName, "-&Delta;&chi;&sup2;")),
        times = 5)

      list(
        chisq = combCompTable,
        aic = matrix(aicCells, nrow = 5, ncol = 5,
                     dimnames = list(modelsAbbrev, modelsAbbrev)),
        bic = matrix(bicCells, nrow = 5, ncol = 5,
                     dimnames = list(modelsAbbrev, modelsAbbrev)))
    })

    ## the page holding the comparison of all models ----
    # Only the boxes and their headings. Each table is an output of its own below, so
    # changing the significance level redraws the table without rebuilding the page - which
    # would close any legend the user has open.
    output[[modelTestsContStr]] <<- renderUI({

      fit <- passFit()

      #### message if warnings ----
      if (sum(fit$warns) > 0) {

        lavWarnsMsg <- tagList(
          h6("The following models produced warnings:"),

          cbind(paste0(modelsLong[fit$warnModels], ":&emsp;"),
                sapply(fit$fittedModels[fit$warnModels],
                       function(model) attr(model, "shinyCTTwarning")$message)) %>%
            kableExtra::kbl(row.names = FALSE, escape = FALSE) %>%
            kableExtra::column_spec(column = 1, bold = TRUE) %>%
            HTML() %>%
            div(style = "color:orange")
        ) # tagList

      } else {
        lavWarnsMsg <- NULL
      }

      #### message if errors ----
      if (sum(fit$errs) > 0) {

        lavErrsMsg <- tagList(
          h6("The following models produced errors:"),

          cbind(paste0(modelsLong[fit$errModels], ":&emsp;"),
                sapply(fit$fittedModels[fit$errModels],
                       function(model) model$message)) %>%
            kableExtra::kbl(row.names = FALSE, escape = FALSE) %>%
            kableExtra::column_spec(column = 1, bold = TRUE) %>%
            HTML() %>%
            div(style = "color:red")
          ) # tagList

      } else {
        lavErrsMsg <- NULL
      }

      lavStatus <- if (sum(fit$warns) > 0 || sum(fit$errs) > 0) {
        wellPanel(
          h5(sprintf("Lavaan status: %i warnings, %i errors.",
                     sum(fit$warns),
                     sum(fit$errs))),
          lavErrsMsg,
          lavWarnsMsg)
      }

      #### if there are no good models, the status is the whole page ----
      if (length(fit$goodModels) == 0) return(tagList(lavStatus))

      #### otherwise, one box per comparison ----
      fluidPage(

        if (!is.null(lavStatus)) fluidRow(lavStatus),

        fluidRow(
          shinydashboard::box(
            title = "Hierarchical model comparison plot:",
            width = 12,
            plotOutput(hierPlotStr))),

        fluidRow(
          shinydashboard::box(
            title = "Hierarchical model comparison table:",
            width = 12,
            htmlOutput(hierTableStr),
            actionLink(paste0("showLegendHierTable", groupAppend), "Show/hide legend"),
            conditionalPanel(paste0("input.showLegendHierTable", groupAppend, " % 2 == 1"),
                             htmlOutput(hierTableLegendStr)))),

        fluidRow(
          shinydashboard::box(
            title = "Fit index table",
            width = 12,
            htmlOutput(fitsTableStr),
            br(),
            actionLink(paste0("showLegendFitIndexTable", groupAppend), "Show/hide legend"),
            conditionalPanel(paste0("input.showLegendFitIndexTable", groupAppend, " % 2 == 1"),
                             htmlOutput(fitsTableLegendStr)))),

        fluidRow(
          shinydashboard::box(
            title = HTML("&chi;&sup2;-comparison table:"),
            width = 12,
            htmlOutput(combCompTableStr),
            br(),
            actionLink(paste0("showLegendCombCompTable", groupAppend), "Show/hide legend"),
            conditionalPanel(paste0("input.showLegendCombCompTable", groupAppend, " % 2 == 1"),
                             htmlOutput(combCompTableLegendStr)))),

        fluidRow(
          shinydashboard::box(
            title = "AIC/BIC-comparison table:",
            width = 12,
            htmlOutput(infCompTableStr),
            actionLink(paste0("showLegendInfCompTable", groupAppend), "Show/hide legend"),
            conditionalPanel(paste0("input.showLegendInfCompTable", groupAppend, " % 2 == 1"),
                             htmlOutput(infCompTableLegendStr))))

      ) # fluidPage
    })

    ## hierarchical model comparison plot ----
    output[[hierPlotStr]] <<- renderPlot({

      succTable <- passFit()$succTable
      goodModels <- passFit()$goodModels

      req(length(goodModels) > 0)

      modelNumbs <- which(models %in% goodModels)

      chisqs <- dfs <- pvalues <- rep(NA, 5)

      names(chisqs) <-
        names(dfs) <-
        names(pvalues) <- c("tkoete", "eteteq", "eteetp", "teqtpa", "etptpa")

      if (!is.null(succTable$teq)) {
        teqNames <- paste0(rownames(succTable$teq)[1:(nrow(succTable$teq) - 1)],
                           rownames(succTable$teq)[2:nrow(succTable$teq)])

        chisqs[teqNames] <- succTable$teq[-1, "Chisq diff"]
        dfs[teqNames] <- succTable$teq[-1, "Df diff"]
        pvalues[teqNames] <- succTable$teq[-1, "Pr(>Chisq)"]
      }

      if (!is.null(succTable$etp)) {
        etpNames <- paste0(rownames(succTable$etp)[1:(nrow(succTable$etp) - 1)],
                           rownames(succTable$etp)[2:nrow(succTable$etp)])

        chisqs[etpNames] <- succTable$etp[-1, "Chisq diff"]
        dfs[etpNames] <- succTable$etp[-1, "Df diff"]
        pvalues[etpNames] <- succTable$etp[-1, "Pr(>Chisq)"]
      }

      modelTestDF$chisq <- chisqs
      modelTestDF$df <- dfs
      modelTestDF$pvalue <- pvalues

      ### ggplot code ----
      ggplot2::ggplot(modelTestDF,
                      ggplot2::aes(x = .data$x, y = .data$y, label = .data$name)) +

        ggplot2::geom_text(parse = TRUE, fontface = "bold", size = 5) +
        ggplot2::geom_segment(
          ggplot2::aes(x = .data$xstarts, y = .data$ystarts,
                       xend = .data$xends, yend = .data$yends),
          linewidth = 0.3) +

        ggplot2::geom_label(
          ggplot2::aes(
            x = .data$labelxs,
            y = .data$labelys,

            label = ifelse(
              is.na(.data$chisq),
              yes = "No~Comparison",
              no = sprintf(
                "'%s-'*Delta*chi^2==%.3f*','~Delta*df==%i*','~p%s",
                passFit()$estimatorName, # %s
                .data$chisq, # %.3f
                .data$df, # %i
                ifelse(.data$pvalue < 0.001, "<0.001", sprintf("==%.3f", .data$pvalue)))),

            fill = c("nsig", "sig")[c(.data$pvalue < sigLvl()) + 1]), # aes

          color = textColor,
          size = 4.5,
          parse = TRUE) + # geom_label

        ggplot2::scale_fill_manual(
          values = c("nsig" = goodColor, "sig" = badColor),
          na.value = neutrColor) +

        ggplot2::guides(fill = "none") +
        ggplot2::xlim(c(-4, 4)) +
        ggplot2::coord_fixed() +
        ggplot2::theme_void()

    }) # renderPlot

    ## hierarchical model comparison table ----
    # Two tables side by side: one down each branch of the hierarchy.
    output[[hierTableStr]] <<- renderUI({

      fit <- passFit()
      req(length(fit$goodModels) > 0)

      hierTables <- lapply(

        c("teq", "etp"),

        function(model) {
          if (!is.null(fit$succTable[[model]])) {

            succTableTmp <- as.data.frame(fit$succTable[[model]])
            makeHierTable(succTableTmp, passFits()[rownames(succTableTmp), "cfi"],
                          fit$estimatorName, sigLvl(),
                          goodColor, badColor, neutrColor, textColor, modelsAbbrev)
          } else {
            NULL
          }
        } # function(model)
      ) # lapply

      paste0(
        "<table align = \"center\", width = \"100%\"><tr><td>",
        hierTables[[1]],
        "</td><td>&nbsp;</td><td>",
        hierTables[[2]],
        "</td></tr></table>") %>%
        HTML()
    })

    ## fit index table ----
    output[[fitsTableStr]] <<- renderUI({
      req(length(passFit()$goodModels) > 0)

      HTML(makeFitsTable(passFits(), passFit()$estimatorName, sigLvl(), rmseaCiLvl(),
                         goodColor, badColor, neutrColor, textColor,
                         modelsAbbrev))
    })

    ## chi-square comparison table ----
    output[[combCompTableStr]] <<- renderUI({
      req(length(passFit()$goodModels) > 0)

      # One header spanning the two columns of each model.
      headerNames <- c(1, rep(2, 5))
      names(headerNames) <- c(" ", modelsAbbrev)

      makeKable(compMatrices()$chisq, bold_cols = 1) %>%
        kableExtra::add_header_above(headerNames, escape = FALSE) %>%
        HTML()
    })

    ## AIC/BIC comparison table ----
    output[[infCompTableStr]] <<- renderUI({
      req(length(passFit()$goodModels) > 0)

      paste0(
        "<table align = \"center\", width = \"100%\"> <tr><td>
          <table align = \"center\"> <tr><td>
            <h5>AIC:</h5>",

        makeKable(compMatrices()$aic, bold_cols = 1),

        "</td></tr></table>
      </td>
      <td>&nbsp;</td>
      <td>
        <table align = \"center\"> <tr><td>
          <h5>BIC:</h5>",

        makeKable(compMatrices()$bic, bold_cols = 1),

      "</td></tr></table>
    </td></tr></table>") %>%
      HTML()
    })

    ## the four legends ----
    # Each names the significance level it is describing, so each follows it.
    output[[hierTableLegendStr]] <<- renderUI(
      makeLegend("hierTables", passFit()$estimatorName, sigLvl(),
                 goodColor, badColor, neutrColor, textColor))

    output[[fitsTableLegendStr]] <<- renderUI(
      makeLegend("fitIndexTable", passFit()$estimatorName, sigLvl(),
                 goodColor, badColor, neutrColor, textColor,
                 rmseaCiLvl = rmseaCiLvl()))

    output[[combCompTableLegendStr]] <<- renderUI(
      makeLegend("combCompTable", passFit()$estimatorName, sigLvl(),
                 goodColor, badColor, neutrColor, textColor))

    output[[infCompTableLegendStr]] <<- renderUI(
      makeLegend("infCompTable", passFit()$estimatorName, sigLvl(),
                 goodColor, badColor, neutrColor, textColor))

    ## the three tab strips ----
    # Built whole from the models that fitted, rather than a tab being added per model:
    # the models can be fitted again, and adding to the strip would give two tabs per
    # model the second time round. tabBox() takes its panels one by one, so do.call()
    # hands it the list.
    output[[parTabsetStr]] <<- renderUI({
      panels <- lapply(
        passFit()$goodModels,
        function(thisModel) tabPanel(
          title = HTML(modelsLong[thisModel]),
          htmlOutput(paste0(thisModel, "ParTable", groupAppend))))

      do.call(
        shinydashboard::tabBox,
        c(list(id = paste0("parTabsetTab", groupAppend),
               title = "Estimated parameters",
               width = 12),
          unname(panels)))
    })

    output[[fsTabsetStr]] <<- renderUI({
      panels <- lapply(
        passFit()$goodModels,
        function(thisModel) tabPanel(
          title = HTML(modelsLong[thisModel]),
          sidebarLayout(

            sidebarPanel(
              h4("Download Predicted Factor Scores as CSV"),

              textInput(
                paste0(thisModel, "Filename", groupAppend),
                "Filename:",
                sprintf("%s_%s_factorscores.csv", passFit()$dataName, thisModel)),

              hr(),

              radioButtons(
                paste0(thisModel, "Sep", groupAppend),
                "Separator",
                choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                selected = ","),

              radioButtons(
                paste0(thisModel, "Dec", groupAppend),
                "Decimal Separator",
                choices = c(Comma = ",", Dot = "."),
                selected = "."),

              hr(),

              downloadButton(
                paste0(thisModel, "ScoresDownload", groupAppend),
                "Download Factor Scores") %>%

                div(align = "center"),

              width = 3
            ), # sidebarPanel

            mainPanel(
              h4("Data Overview"),
              DT::dataTableOutput(paste0(thisModel, "Scores", groupAppend)))

          ) # sidebarLayout
        )) # tabPanel, lapply

      do.call(
        shinydashboard::tabBox,
        c(list(id = paste0("fsTabsetTab", groupAppend),
               title = HTML("Predicted factor scores (&eta;&#x302;)"),
               width = 12),
          unname(panels)))
    })

    output[[mcTabsetStr]] <<- renderUI({
      panels <- lapply(
        passFit()$goodModels,
        function(thisModel) tabPanel(
          title = HTML(modelsLong[thisModel]),
          h5("The following R code can be used to fit this model with lavaan:"),
          verbatimTextOutput(paste0(thisModel, "Code", groupAppend))))

      do.call(
        shinydashboard::tabBox,
        c(list(id = paste0("mcTabsetTab", groupAppend),
               title = "Model code",
               width = 12),
          unname(panels)))
    })

    ## one set of outputs per model ----
    # All five models get theirs, whether or not they were chosen. The req() at the top of
    # each holds back the ones that were not fitted; the tab strips above only show tabs
    # for the ones that were.
    lapply(models, function(thisModel) {

      parTableStr <- paste0(thisModel, "ParTable", groupAppend)
      scoresStr <- paste0(thisModel, "Scores", groupAppend)
      scoresDLStr <- paste0(thisModel, "ScoresDownload", groupAppend)
      scoresDLFileStr <- paste0(thisModel, "Filename", groupAppend)
      sepStr <- paste0(thisModel, "Sep", groupAppend)
      decStr <- paste0(thisModel, "Dec", groupAppend)
      codeStr <- paste0(thisModel, "Code", groupAppend)

      ### parameter table ----
      output[[parTableStr]] <<- renderUI({

        fit <- passFit()
        req(thisModel %in% fit$goodModels)

        fittedModel <- fit$fittedModels[[thisModel]]
        thisModelsNgroups <- fittedModel@Data@ngroups

        parTableWithCIs <- makeParTableWithCIs(fittedModel, fit$estimatorName,
                                               sigLvl(), fit$itemCols,
                                               thisModelsNgroups)

        #### modify parameter tables if there are groups ----
        if (!isFALSE(fit$groupName)) {
          for (i in 1:thisModelsNgroups) {

            groupRowHeaders <- sprintf("Group: %s", fittedModel@Data@group.label)

            parTableWithCIs <- kableExtra::group_rows(
              parTableWithCIs,
              group_label = groupRowHeaders[i],
              start_row = (i - 1) * (length(fit$itemCols) + 1) + 1,
              end_row = i * (length(fit$itemCols) + 1),
              label_row_css = "background-color: #666; color: #fff;")
          }
        }

        HTML(parTableWithCIs)
      })

      ### factor scores ----
      output[[scoresStr]] <<- DT::renderDataTable({

        req(thisModel %in% passFit()$goodModels)

        getPredictedScores(
          passFit()$fittedModels[[thisModel]],
          passFit()$groupValues)

      }, options = list(pageLength = 10))

      output[[scoresDLStr]] <<- downloadHandler(
        filename = function() input[[scoresDLFileStr]],
        content = function(file) {

          utils::write.table(
            getPredictedScores(
              passFit()$fittedModels[[thisModel]],
              passFit()$groupValues),

            file = file,
            sep = input[[sepStr]],
            dec = input[[decStr]],
            row.names = FALSE)
        },
        contentType = "text/csv")

      ### model code ----
      output[[codeStr]] <<- renderPrint({

        fit <- passFit()
        req(thisModel %in% fit$goodModels)

        cat(
          makeRCode(
            dataSource = fit$dataSource,
            groupCol = fit$groupCol,
            groups = fit$groups,
            modelCode = fit$modelCodes[[thisModel]],
            estimator = fit$estimator,
            missingMethod = fit$missingMethod,
            isSubset = fit$isSubset,
            model = thisModel,
            isMg = !isFALSE(fit$groupName)))
      })
    })
  })
}
