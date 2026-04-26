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

  modelsLong <- c("&tau;-kongeneric",
                  "essentially &tau;-equivalent",
                  "&tau;-equivalent",
                  "essentially &tau;-parallel",
                  "&tau;-parallel")
  modelsExpr <- c("bold(\u03C4*'-kongeneric')",
                  "bold(essentially~\u03C4*'-equivalent')",
                  "bold(\u03C4*'-equivalent')",
                  "bold(essentially~\u03C4*'-parallel')",
                  "bold(\u03C4*'-parallel')")
  modelsAbbrev <- c("&#964;-kong.",
                    "ess. &#964;-equiv.",
                    "&#964;-equiv.",
                    "ess. &#964;-paral.",
                    "&#964;-paral.")
  models <- c("tko", "ete", "teq", "etp", "tpa")

  names(models) <-
    names(modelsLong) <-
    names(modelsExpr) <-
    names(modelsAbbrev) <- models

  possComps <- outer(models, models, paste0)[lower.tri(diag(5))][-8]

  modelTestDF <- data.frame(name = modelsExpr,
                            x = c(0, 0, -2, 2, 0),
                            y = c(6, 4, 2, 2, 0),
                            xstarts = c(0, 0, 0, -2, 2),
                            xends = c(0, -2, 2, 0, 0),
                            ystarts = c(5.8, 3.8, 3.8, 1.8, 1.8),
                            yends = c(4.2, 2.2, 2.2, 0.2, 0.2),
                            labelxs = c(0, -2, 2, -2, 2),
                            labelys = c(5, 3, 3, 1, 1))

  ## Reactive values ----
  notifications <- reactiveValues(notList = list())

  dataMenuList <- reactiveValues()

  userDataRaw <- reactiveVal()
  userDataChosen <- reactiveVal()
  userDataNA <- reactiveVal()
  userDataGroup <- reactiveVal()

  itemColsRV <- reactiveVal()
  groupColRV <- reactiveVal()
  validGroupsRV <- reactiveVal()

  incompleteCasesRV <- reactiveVal()
  fimlRV <- reactiveVal(FALSE)
  estimatorNameRV <- reactiveVal()

  mvnTestResult <- reactiveValues(
    raw = NULL,
    estimator = "ML")

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

  ## Sidebar ----
  dataMenuList$menuList <- list(
    shinydashboard::menuItem(
      "1. Data selection",
      tabName = "dataSelectionTab",
      icon = icon("database")),
    hr(),
    shinydashboard::menuItem(
      "Reload",
      tabName = "reloadTab",
      icon = icon("sync"),
      selected = FALSE))

  output$dataMenuOut <- shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(
      id = "dataMenu",
      .list = dataMenuList$menuList)})

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
    userDataRaw(NULL)

    shinyjs::disable("dataSelectButton")

    notifications$notList$noData <- shinydashboard::notificationItem(
      text = "No data selected",
      icon = icon("times"),
      status = "danger")

    ### choose data source ----
    if (input$source == "CSV") {
      req(input$CSVFile)

      userDataTmp <- read.csv(
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
    shinyjs::disable("source")
    shinyjs::disable("CSVFile")
    shinyjs::disable("header")
    shinyjs::disable("sep")
    shinyjs::disable("quote")
    shinyjs::disable("header")
    shinyjs::disable("objectFromWorkspace")
    shinyjs::disable("dataSelectButton")

    dataMenuList$menuList[[2]] <- shinydashboard::menuItem(
      "2. Subset selection",
      tabName = "subsetSelectionTab",
      icon = icon("table"),
      selected = TRUE)

    dataMenuList$menuList[[3]] <- hr()

    dataMenuList$menuList[[4]] <- shinydashboard::menuItem(
      "Reload",
      tabName = "reloadTab",
      icon = icon("sync"),
      selected = FALSE)

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
      possibleGroups <- unique(na.omit(userDataChosen()[, input$groupCol]))

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

    if (input$dataSelectButton > 0) {

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
    if (input$selectall != 0) {
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
    if (input$deselectall != 0) {
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

    if (input$dataSelectButton > 0) {

      if (length(input$itemCols) <= 1 ||
          (input$groupCol != "noGroupSelected" && length(input$groups) == 0)) {

        shinyjs::disable("subsetSelectButton") # subset of items
      } else {
        shinyjs::enable("subsetSelectButton")
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

  ## subsetSelectionTab naInfoBox ----
  output$naInfoBox <- shinydashboard::renderValueBox({
    incompleteCasesRV(!complete.cases(userDataNA()))
    output$incompleteCasesBoolRV <- reactive({any(incompleteCasesRV())})
    outputOptions(output, "incompleteCasesBoolRV", suspendWhenHidden = FALSE)

    shinydashboard::valueBox(
      value = sum(incompleteCasesRV()),
      color = if (any(incompleteCasesRV())) "yellow" else "green",
      subtitle = "rows with missing values in this subset",
      icon = icon("exclamation-triangle"))
  })

  ## subsetSelectionTab naTable ----
  output$naTable <- renderUI({
    HTML(shinyCTT:::makeKable(data.frame(NAs = colSums(is.na(userDataChosen())))))
  })

  ## subsetSelectionTab obsTable ----
  output$obsTable <- renderUI({
    nTotal <- nrow(userDataNA())
    nComplete <- sum(!incompleteCasesRV())

    HTML(shinyCTT:::makeKable(data.frame(Total = nTotal, Complete = nComplete)))
  })

  ## subsetSelectionTab obsPerGroupTable ----
  output$obsPerGroupTable <- renderUI({
      req(input$groupCol)

      if (input$groupCol != "noGroupSelected") {
        HTML(shinyCTT:::makeKable(t(table(userDataChosen()[, input$groupCol], useNA = "ifany"))))
      } else {
        helpText("No group column selected.")
      }
  })

  ## subsetSelectionTab observeEvent subsetSelectButton ----
  observeEvent(input$subsetSelectButton, {
    shinyjs::disable("itemCols")
    shinyjs::disable("groupCol")
    shinyjs::disable("groups")
    shinyjs::disable("subsetSelectButton")
    shinyjs::disable("useFIML")

    fimlRV(any(incompleteCasesRV()) & input$useFIML)

    dataMenuList$menuList[[8]] <- dataMenuList$menuList[[4]]

    dataMenuList$menuList[[4]] <- shinydashboard::menuItem(
      "3. Statistics",
      shinydashboard::menuSubItem(
          "Descriptive Statistics",
          tabName = "statisticsTab",
          selected = TRUE),
      shinydashboard::menuSubItem(
          "Correlational Analysis",
          tabName = "corrTab"),
      shinydashboard::menuSubItem(
          "Test on Multivariate Normality",
          tabName = "mvnTab"),
      icon = icon("chart-bar"),
      startExpanded = TRUE)

    dataMenuList$menuList[[5]] <- hr()

    dataMenuList$menuList[[6]] <- shinydashboard::menuItem(
        "4. Testing Parameters",
        tabName = "testParamTab",
        icon = icon("cog"))

    dataMenuList$menuList[[7]] <- hr()

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

    if (any(incompleteCasesRV())) {
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
          Sd = sd(col, na.rm = TRUE),
          Skew = moments::skewness(col, na.rm = TRUE),
          Excess = moments::kurtosis(col, na.rm = TRUE) - 3)
      }
    )) # t(apply(

    nHeader <- c(1, 4)
    names(nHeader) <- c(" ", sprintf("n<sub>all</sub> = %i", nrow(userDataGroup())))

    overallDescrTable <- shinyCTT:::makeKable(table, bold_cols = 1) %>%
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
                c(Mean = mean(col, na.rm = TRUE), SD = sd(col, na.rm = TRUE),
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
          shinyCTT:::makeKable(
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

  ## statisticsTab histBox ----
  output$histBox <- renderUI({

    ## histBox output singleHist ----
    output$singleHist <- renderPlot({
      ggplot2::ggplot(
        data.frame(item = na.omit(userDataGroup()[, input$histItem])),
        ggplot2::aes(x = item)) +

        ggplot2::geom_histogram(
          if (input$singleDens) ggplot2::aes(y = ggplot2::after_stat(density)),
          color = "white",
          fill = "#438BCA",
          bins = input$singleNoBins) +

        ggplot2::xlab(input$histItem) +
        ggplot2::theme_classic()
    })

    ## histBox if (validGroupsRV()) ----
    if (validGroupsRV()) {

      ### histBox output groupHist ----
      output$groupHist <- renderPlot({
        ggplot2::ggplot(
          subset(
            userDataGroup(),
            subset = userDataGroup()[, input$groupCol] %in% input$histGroupGroups,
            select = c(input$groupCol, input$histItemGroup)) %>%
            na.omit() %>%
            setNames(nm = c("group", "item")),
          ggplot2::aes(x = item, fill = group)) +

          ggplot2::geom_histogram(
            if (input$groupDens) ggplot2::aes(y = ggplot2::after_stat(density)),
            color = "white",
            bins = input$groupNoBins,
            position = "dodge") +

          ggplot2::xlab(input$histItemGroup) +
          ggplot2::scale_fill_discrete(name = input$groupCol) +
          ggplot2::theme_classic()
      })

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
              radioButtons(
                "singleDens",
                "Choose the ordinate scaling:",
                choices = c("Density" = TRUE, "Frequency" = FALSE),
                selected = FALSE)))),

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
              radioButtons(
                "groupDens",
                "Choose the ordinate scaling:",
                choices = c("Density" = TRUE, "Frequency" = FALSE),
                selected = FALSE)))
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
              "Number of bins",
              min = 1, max = 100, value = 30, step = 1)),
          column(
            width = 6,
            radioButtons(
              "singleDens",
              "Choose ordinate units",
              choices = c("Density" = TRUE, "Frequency" = FALSE),
              selected = FALSE)))

      ) # box
    }
  })

  ## statisticsTab covMatBox ----
  output$covMatBox <- renderUI({
    req(userDataGroup())

    table <- cov(userDataGroup()[, input$itemCols], use = "pairwise.complete.obs")
    table[upper.tri(table)] <- NA

    ## covMatBox if (validGroupsRV()) ----
    if (validGroupsRV()) {
      groups <- unique(userDataGroup()[, input$groupCol])

      mgCovMatList <- lapply(
        groups,
        function(group) {
          cov(
            subset(
              userDataGroup()[, input$itemCols],
              userDataGroup()[, input$groupCol] == group),
            use = "pairwise.complete.obs")
        })

      for (i in 1:length(mgCovMatList))
        mgCovMatList[[i]][upper.tri(mgCovMatList[[i]])] <- NA

      mgCovMatTable <- shinyCTT:::makeKable(do.call(rbind, mgCovMatList),
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
        title = "Covariance Matrix:",
        side = "right",

        tabPanel(
          title = "Overall",
          shinyCTT:::makeKable(table, bold_cols = 1) %>%
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
        title = "Covariance Matrix:",

        shinyCTT:::makeKable(table, bold_cols = 1) %>%
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

      corrInd <- unlist(shinyCTT:::extractFitIndices(corrIndRaw)[, c(2, 1, 3)])

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

  ## corrTab scatterBox ----
  output$scatterBox <- renderUI({

    req(userDataGroup())

    ## scatterBox output singleScatter ----
    output$singleScatter <- renderPlot({
      ggplot2::ggplot(
          data.frame(
              itemX = userDataGroup()[, input$scatterItemX],
              itemY = userDataGroup()[, input$scatterItemY]) %>%
            na.omit(),
          ggplot2::aes(x = itemX, y = itemY)) +

          ggplot2::geom_point(color = "#438BCA") +
          ggplot2::xlab(input$scatterItemX) +
          ggplot2::ylab(input$scatterItemY) +
          ggplot2::theme_classic()
    })

    ## scatterBox if (validGroupsRV()) ----
    if (validGroupsRV()) {

      output$groupScatter <- renderPlot({
        ggplot2::ggplot(
          subset(
            userDataGroup(),
            subset = userDataGroup()[, input$groupCol] %in% input$scatterGroupGroups,
            select = c(input$groupCol, input$scatterItemXGroup, input$scatterItemYGroup)) %>%
            na.omit() %>%
            setNames(nm = c("group", "itemX", "itemY")),
          ggplot2::aes(x = itemX, y = itemY, color = group)) +

          ggplot2::geom_point() +
          ggplot2::xlab(input$scatterItemXGroup) +
          ggplot2::ylab(input$scatterItemYGroup) +
          ggplot2::scale_color_discrete(name = input$groupCol) +
          ggplot2::theme_classic()
      })

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
        title = "Scatter Plot:",
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
        cor(userDataGroup()[, input$itemCols],
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

        shinyCTT:::makeKable(
          # bootstrap_options = "bordered",
          position = "left") %>%
        HTML()

    ) # tagList

    ## corrTableBox singleCorrTable if no errors: ----
    if (class(corrTableWithCIsRaw$test)[1] == "list") {

      singleCorrTable <- shinyCTT:::makeCorrTableWithCIs(
        rawTable = corrTableWithCIsRaw,
        goodColor,
        badColor,
        neutrColor,
        textColor,
        sigLvl = input$corrTabSL,
        itemCols = input$itemCols) %>%

        shinyCTT:::makeKable(
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

          shinyCTT:::makeCorrTableWithCIs(

            rawTable = list(
              cor = suppressWarnings(cor(
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
      mgCorrTable <- shinyCTT:::makeKable(
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
        title = "Correlation Table with Confidence Intervals:",
        side = "right",

        tabPanel(
            "Overall",
            singleCorrTable,
            br(),
            HTML(makeLegend("corrTable", estimatorNameRV(), input$sigLvl,
                            goodColor, badColor, neutrColor, textColor))),
        tabPanel(
            "Group-wise",
            HTML(mgCorrTable),
            br(),
            HTML(makeLegend("corrTable", estimatorNameRV(), input$sigLvl,
                            goodColor, badColor, neutrColor, textColor)))

      ) # tabBox

    } ## corrTableBox if no groups ----
    else {

      shinydashboard::box(
          width = 12,
          title = "Correlation Table with Confidence Intervals:",

          singleCorrTable,
          br(),
          corrTableLegend)
    }
  })

  # observeEvent input$estimator ----
  observeEvent(input$estimator, {
    mvnTestResult$estimator <- input$estimator
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
      MVN::mvn(na.omit(userDataGroup()[, input$itemCols]),
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

      HTML(shinyCTT:::makeKable(mvnUV, bootstrap_options = "basic"))

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
          HTML(shinyCTT:::makeKable(mvnMV, bootstrap_options = "basic")),
          HTML("It is thus recommended to continue with the <b>Robust Maximum Likelihood (MLR)</b> estimator."))

      } else {

        tagList(
          sprintf("The hypotheses that Mardia's Skewness statistic
                    and Mardias' Kurtosis statistic match those of a
                    normal distribution can be maintained on a significance
                    level of %s. Test result:", input$mvnSL),
          HTML(shinyCTT:::makeKable(mvnMV, bootstrap_options = "basic")),
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

    output$mvnPlot <- renderPlot({

      userDataNAOmit <- na.omit(userDataGroup())

      if (input$mvnPlotType == "qq") {
        MVN::multivariate_diagnostic_plot(
          na.omit(userDataNAOmit[, input$itemCols]),
          type = "qq")

      } else if (input$mvnPlotType == "persp") {
        persp(x = MASS::kde2d(userDataNAOmit[, input$mvnItemX],
                              userDataNAOmit[, input$mvnItemY],
                              n = 100),
              theta = 1, phi = 30, border = NA, shade = 0.5, box = T,
              xlab = input$mvnItemX,
              ylab = input$mvnItemY,
              zlab = "Density")

      } else if (input$mvnPlotType == "contour") {
        contour(x = MASS::kde2d(userDataNAOmit[, input$mvnItemX],
                                userDataNAOmit[, input$mvnItemY],
                                n = 100),
                nlevels = 20,
                xlab = input$mvnItemX,
                ylab = input$mvnItemY)
      }
    })

    shinydashboard::box(
      width = 12,
      title = "Multivariate Plot:",

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

  # observeEvent input$goModels ----
  observeEvent(input$goModels, {
    shinyjs::disable("goModels")
    shinyjs::disable("doMg")
    shinyjs::disable("etaIntFree")
    shinyjs::disable("estimator")

    estimatorNameRV(paste0(if (fimlRV()) "FI", input$estimator))

    dataMenuList$menuList[[13]] <- dataMenuList$menuList[[8]]

    ## observeEvent input$goModels if doMg true ----
    if (input$doMg) {

      dataMenuList$menuList[[8]] <- shinydashboard::menuItem(
        "5. Model Comparison Tests",

        shinydashboard::menuSubItem("Single Group", tabName = "modelTests", selected = TRUE),
        shinydashboard::menuSubItem("Multigroup", tabName = "modelTestsMg"),
        icon = icon("chart-bar"),
        startExpanded = TRUE)

      dataMenuList$menuList[[9]] <- shinydashboard::menuItem(
        "6. Parameter Tables",

        shinydashboard::menuSubItem("Single Group", tabName = "parTables"),
        shinydashboard::menuSubItem("Multigroup", tabName = "parTablesMg"),
        icon = icon("chart-bar"))

      dataMenuList$menuList[[10]] <- shinydashboard::menuItem(
        "7. Factor Scores",

        shinydashboard::menuSubItem("Single Group", tabName = "facScores"),
        shinydashboard::menuSubItem("Multigroup", tabName = "facScoresMg"),
        icon = icon("chart-bar"))

      dataMenuList$menuList[[11]] <- shinydashboard::menuItem(
        "8. Model Code",

        shinydashboard::menuSubItem("Single Group", tabName = "modelCode"),
        shinydashboard::menuSubItem("Multigroup", tabName = "modelCodeMg"),
        icon = icon("chart-bar"))

    } ## observeEvent input$goModels if doMg false ----
    else {
      dataMenuList$menuList[[8]] <- shinydashboard::menuItem(
        "5. Model Comparison Tests",

        shinydashboard::menuSubItem("Single Group", tabName = "modelTests", selected = TRUE),
        icon = icon("chart-bar"),
        startExpanded = TRUE)

      dataMenuList$menuList[[9]] <- shinydashboard::menuItem(
        "6. Parameter Tables",

        shinydashboard::menuSubItem("Single Group", tabName = "parTables"),
        icon = icon("chart-bar"))

      dataMenuList$menuList[[10]] <- shinydashboard::menuItem(
        "7. Factor Scores",

        shinydashboard::menuSubItem("Single Group", tabName = "facScores"),
        icon = icon("chart-bar"))

      dataMenuList$menuList[[11]] <- shinydashboard::menuItem(
        "8. Model Code",

        shinydashboard::menuSubItem("Single Group", tabName = "modelCode"),
        icon = icon("chart-bar"))
    }

    ## test the models! ----
    dataMenuList$menuList[[12]] <- hr()

    modelsToTest <- models[sapply(models, function(thisModel) input[[thisModel]])]

    lapply(
      append(list(FALSE), if (isTRUE(input$doMg)) input$groupCol),
      function(groupName) {
        groupAppend <- c("Mg")[!isFALSE(groupName)]
        hierPlotStr <- paste0("hierPlot", groupName)
        modelTestsContStr <- paste0("modelTestsCont", groupAppend)

        ### try fitting and capture warning and error messages ----
        modelCodes <- shinyCTT:::makeModelCodes(inputData = userDataGroup(),
                                                itemCols = input$itemCols,
                                                group = groupName,
                                                etaIntFree = as.logical(input$etaIntFree))

        #### tryCatches if there are no groups ----
        if (isFALSE(groupName)) {

          ##### Capture warnings ----
          fittedModelsWarns <- lapply(
            modelCodes[modelsToTest],
            FUN = function(model) {
              tryCatch(lavaan::lavaan(model = model,
                                      data = userDataGroup(),
                                      meanstructure = TRUE,
                                      estimator = mvnTestResult$estimator,
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
                       error = function(e) e,
                       warning = function(w) w)
          })

          ##### Capture errors ----
          fittedModelsErrs <- lapply(
            modelCodes[modelsToTest],
            FUN = function(model) {
              suppressWarnings(
                tryCatch(lavaan::lavaan(model = model,
                                        data = userDataGroup(),
                                        meanstructure = TRUE,
                                        estimator = mvnTestResult$estimator,
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
                         error = function(e) e))
          })

        } #### tryCatches if there are groups ----
        else {

          ##### Capture warnings ----
          fittedModelsWarns <- lapply(
            modelCodes[modelsToTest],
            FUN = function(model) {
              tryCatch(lavaan::lavaan(model = model,
                                      data = userDataGroup(),
                                      meanstructure = TRUE,
                                      group = groupName,
                                      group.equal = c("loadings", "intercepts"),
                                      estimator = mvnTestResult$estimator,
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
                       error = function(e) e,
                       warning = function(w) w)
          })

          ##### Capture errors ----
          fittedModelsErrs <- lapply(
            modelCodes[modelsToTest],
            FUN = function(model) {
              suppressWarnings(
                tryCatch(lavaan::lavaan(model = model,
                                        data = userDataGroup(),
                                        meanstructure = TRUE,
                                        group = groupName,
                                        group.equal = c("loadings", "intercepts"),
                                        estimator = mvnTestResult$estimator,
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
                         error = function(e) e))
          })
        }

        #### warning and error counting and capturing ----
        warns <- sapply(
          lapply(fittedModelsWarns, class),
          function(code) code[1] == "simpleWarning")

        errs <- sapply(
          lapply(fittedModelsErrs, class),
          function(code) code[1] == "simpleError")

        goodModels <- modelsToTest[!warns & !errs]
        errModels <- modelsToTest[errs]
        warnModels <- modelsToTest[warns]

        #### message if warnings ----
        if (sum(warns) > 0) {

          lavWarnsMsg <- tagList(
            h6("The following models produced warnings:"),

            cbind(paste0(modelsLong[warnModels], ":&emsp;"),
                  sapply(fittedModelsWarns[warnModels],
                         function(model) model$message)) %>%
              kableExtra::kbl(row.names = FALSE, escape = FALSE) %>%
              kableExtra::column_spec(column = 1, bold = TRUE) %>%
              HTML() %>%
              div(style = "color:orange")
          ) # tagList

        } else {
          lavWarnsMsg <- NULL
        }

        #### message if errors ----
        if (sum(errs) > 0) {

          lavErrsMsg <- tagList(
            h6("The following models produced errors:"),

            cbind(paste0(modelsLong[errModels], ":&emsp;"),
                  sapply(fittedModelsErrs[errModels],
                         function(model) model$message)) %>%
              kableExtra::kbl(row.names = FALSE, escape = FALSE) %>%
              kableExtra::column_spec(column = 1, bold = TRUE) %>%
              HTML() %>%
              div(style = "color:red")
            ) # tagList

        } else {
          lavErrsMsg <- NULL
        }

        ### generate comparative fit table and tab ----
        fits <- do.call(rbind, lapply(fittedModelsWarns[goodModels], shinyCTT:::extractFitIndices))
        comps <- possComps[sapply(possComps, function(thisComp) input[[thisComp]])]

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

        compTable <- reactiveValues(
          df = matrix(ncol = 5, nrow = 5),
          chisq = matrix(ncol = 5, nrow = 5))

        infCompTable <- reactiveValues(
          aic = matrix(ncol = 5, nrow = 5),
          bic = matrix(ncol = 5, nrow = 5))

        names(compTable$df) <-
          names(compTable$chisq) <-
          names(infCompTable$aic) <-
          names(infCompTable$bic) <- outer(models, models, paste0)

        compTable$chisq[lower.tri(diag(5), diag = TRUE)] <-
          infCompTable$aic[lower.tri(diag(5), diag = TRUE)] <-
          infCompTable$bic[lower.tri(diag(5), diag = TRUE)] <- "<span style=\"color: lightgrey;\" >X</span>"

        ### generate parameter tables, fits and fit tables ----
        for (model in goodModels) {
          local({
            thisModel <- model

            whichModel <- which(goodModels == thisModel)

            thisModelStr <- paste0(thisModel, thisModel)
            thisModelsNgroups <- fittedModelsWarns[[thisModel]]@Data@ngroups

            thisModelScoresStr <- paste0(thisModel, "Scores", groupAppend)
            thisModelScoresDLStr <- paste0(thisModel, "ScoresDownload", groupAppend)
            thisModelScoresDLFileStr <- paste0(thisModel, "Filename", groupAppend)

            thisModelCodeStr <- paste0(thisModel, "Code", groupAppend)

            #### write to diag(chisq comp table) ----
            if (fits[thisModel, "pvalue"] < input$sigLvl) {
              sigAddon <- "*"
              sigColor <- badColor
              sigTxtColor <- textColor

              if (fits[thisModel, "pvalue"] < 0.01)
                sigAddon <- paste0(sigAddon, "*")

              if (fits[thisModel, "pvalue"] < 0.001)
                sigAddon <- paste0(sigAddon, "*")

            } else {

              sigAddon <- ""
              sigColor <- goodColor
              sigTxtColor <- textColor
            }

            compTable$chisq[thisModelStr] <-
              kableExtra::cell_spec(
                sprintf(paste0("%.3f", sigAddon), fits[thisModel, "chisq"]),
                background = sigColor,
                color = sigTxtColor,
                italic = TRUE)

            compTable$df[thisModelStr] <-
              kableExtra::cell_spec(
                sprintf("%i", fits[thisModel, "df"]),
                background = sigColor,
                color = sigTxtColor,
                italic = TRUE)

            ##### write to AIC/BIC comp table ----
            infCompTable$aic[thisModelStr] <-
              kableExtra::cell_spec(
                sprintf("%.3f", fits[thisModel, "aic"]),
                color = textColor,
                background = neutrColor)

            infCompTable$bic[thisModelStr] <-
              kableExtra::cell_spec(
                sprintf("%.3f", fits[thisModel, "bic"]),
                color = textColor,
                background = neutrColor)

            ###### if there is more than one good model ----
            if (whichModel > 1) {

              aicDiffs <- fits[thisModel, "aic"] - fits[1:(whichModel - 1), "aic"]
              bicDiffs <- fits[thisModel, "bic"] - fits[1:(whichModel - 1), "bic"]

              infCompTable$aic[paste0(thisModel, rownames(fits)[1:(whichModel - 1)])] <-
                kableExtra::cell_spec(
                  sprintf(ifelse(aicDiffs < 0, "%.3f", "+%.3f"), aicDiffs),
                  color = textColor,
                  background = ifelse(aicDiffs < 0, goodColor, badColor))

              infCompTable$bic[paste0(thisModel, rownames(fits)[1:(whichModel - 1)])] <-
                kableExtra::cell_spec(
                  sprintf(ifelse(bicDiffs < 0, "%.3f", "+%.3f"), bicDiffs),
                  color = textColor,
                  background = ifelse(bicDiffs < 0, goodColor, badColor))
            }

            #### write to lower.tri(chisq comp table) ----
            compsWithThisModel <- substring(
              text = comps[grep(thisModel, substr(comps, 1, 3))],
              first = 4,
              last = 6)

            compsWithThisModel <- compsWithThisModel[compsWithThisModel %in% goodModels]
            names(compsWithThisModel) <- compsWithThisModel

            fitCompsWithThisModel <- sapply(
              compsWithThisModel,
              function(thisComp) {
                tmpTbl <- lavaan::lavTestLRT(fittedModelsWarns[[thisModel]], fittedModelsWarns[[thisComp]])
                unlist(tmpTbl[2, c("Chisq diff", "Df diff", "Pr(>Chisq)")])
            })

            for (thisComp in compsWithThisModel) {

              if (fitCompsWithThisModel["Pr(>Chisq)", thisComp] < input$sigLvl) {
                sigAddon <- "*"
                sigColor <- badColor
                sigTxtColor <- textColor

                if (fitCompsWithThisModel["Pr(>Chisq)", thisComp] < 0.01)
                  sigAddon <- paste0(sigAddon, "*")

                if (fitCompsWithThisModel["Pr(>Chisq)", thisComp] < 0.001)
                  sigAddon <- paste0(sigAddon, "*")

              } else {

                sigAddon <- ""
                sigColor <- goodColor
                sigTxtColor <- textColor
              }

              thisModelCompStr <- paste0(thisModel, thisComp)

              compTable$chisq[thisModelCompStr] <- kableExtra::cell_spec(
                sprintf(paste0("+%.3f", sigAddon), fitCompsWithThisModel["Chisq diff", thisComp]),
                background = sigColor,
                color = sigTxtColor)

              compTable$df[thisModelCompStr] <- kableExtra::cell_spec(
                sprintf("+%i", fitCompsWithThisModel["Df diff", thisComp]),
                background = sigColor,
                color = sigTxtColor)
            }

            #### parameter tables ----
            parTableWithCIs <- makeParTableWithCIs(fittedModelsWarns[[thisModel]], estimatorNameRV(),
                                                   input$sigLvl, input$itemCols,
                                                   thisModelsNgroups)

            ##### modify parameter tables if there are groups ----
            if (!isFALSE(groupName)) {
              for (i in 1:thisModelsNgroups) {

                groupRowHeaders <- sprintf(
                  "Group: %s",
                  fittedModelsWarns[[thisModel]]@Data@group.label)

                parTableWithCIs <- kableExtra::group_rows(
                  parTableWithCIs,
                  group_label = groupRowHeaders[i],
                  start_row = (i - 1) * (length(input$itemCols) + 1) + 1,
                  end_row = i * (length(input$itemCols) + 1),
                  label_row_css = "background-color: #666; color: #fff;")
              }
            }

            #### factor scores ----
            output[[thisModelScoresStr]] <<- DT::renderDataTable(

              shinyCTT:::getPredictedScores(
                fittedModelsWarns[[thisModel]],
                userDataGroup()[, input$groupCol]),

              options = list(pageLength = 10))

            output[[thisModelScoresDLStr]] <<- downloadHandler(
              filename = function() input[[thisModelScoresDLFileStr]],
              content = function(file) {

                write.table(
                  shinyCTT:::getPredictedScores(
                    fittedModelsWarns[[thisModel]],
                    userDataGroup()[, input$groupCol]),

                  file = file,
                  sep = input[[paste0(thisModel, "Sep")]],
                  dec = input[[paste0(thisModel, "Dec")]],
                  row.names = FALSE)
              },
              contentType = "text/csv")

            #### model code ----
            output[[thisModelCodeStr]] <<- renderPrint({

              isSubset <- (
                validGroupsRV() &&
                  (length(unique(userDataGroup()[, input$groupCol])) <
                     length(unique(userDataRaw()[, input$groupCol]))))

              cat(
                shinyCTT:::makeRCode(
                  input = input,
                  modelCode = modelCodes[[thisModel]],
                  estimator = mvnTestResult$estimator,
                  isSubset = isSubset,
                  model = thisModel,
                  isMg = !isFALSE(groupName)))

            })

            #### make tabs for each model ----
            appendTab(
              inputId = paste0("parTabsetTab", groupAppend),

              tabPanel(
                title = HTML(modelsLong[thisModel]),
                HTML(parTableWithCIs)),

              select = as.logical(whichModel == 1))

            appendTab(
              inputId = paste0("mcTabsetTab", groupAppend),

              tabPanel(
                title = HTML(modelsLong[thisModel]),
                h5("The following R code can be used to fit this model with lavaan:"),
                verbatimTextOutput(thisModelCodeStr)),

              select = as.logical(whichModel == 1))

            ##### factor score tab ----
            appendTab(
              inputId = paste0("fsTabsetTab", groupAppend),

              tabPanel(
                title = HTML(modelsLong[thisModel]),
                sidebarLayout(

                  sidebarPanel(
                    h4("Download Predicted Factor Scores as CSV"),

                    textInput(
                      paste0(thisModel, "Filename"),
                      "Filename:",
                      sprintf(
                        "%s_%s_factorscores.csv",
                        switch(input$source,
                               "Workspace" = input$objectFromWorkspace,
                               "CSV" = gsub("\\.csv", "", input$CSVFile$name),
                               "SPSS" = gsub("\\.sav|\\.zsav|\\.por", "", input$SPSSFile$name)),
                        thisModel)),

                    hr(),

                    radioButtons(
                      paste0(thisModel, "Sep"),
                      "Separator",
                      choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                      selected = ","),

                    radioButtons(
                      paste0(thisModel, "Dec"),
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
                    DT::dataTableOutput(
                      paste0(thisModel, "Scores", groupAppend)))

                ) # sidebarLayout
              ), # tabPael

              select = as.logical(whichModel == 1)
            ) # appendTab
          })
        }

        ### if there are good models ----
        if (length(goodModels) > 0) {
          #### hierarchical model comparison plot ----
          output[[hierPlotStr]] <<- renderPlot({

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

            ##### ggplot code ----
            ggplot2::ggplot(modelTestDF,
                            ggplot2::aes(x = x, y = y, label = name)) +

              ggplot2::geom_text(parse = TRUE, fontface = "bold", size = 5) +
              ggplot2::geom_segment(
                ggplot2::aes(x = xstarts, y = ystarts, xend = xends, yend = yends),
                linewidth = 0.3) +

              ggplot2::geom_label(
                ggplot2::aes(
                  x = labelxs,
                  y = labelys,

                  label = ifelse(
                    is.na(chisq),
                    yes = "No~Comparison",
                    no = sprintf(
                      "'%s-'*Delta*chi^2==%.3f*','~Delta*df==%i*','~p%s",
                      estimatorNameRV(), # %s
                      chisq, # %.3f
                      df, # %i
                      ifelse(pvalue < 0.001, "<0.001", sprintf("==%.3f", pvalue)))),

                  fill = c("nsig", "sig")[c(pvalue < input$sigLvl) + 1]), # aes

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

          #### Hierarchical model comparison table ----
          hierTables <- lapply(

            c("teq", "etp"),

            function(model) {
              if (!is.null(succTable[[model]])) {

                succTableTmp <- as.data.frame(succTable[[model]])
                makeHierTable(succTableTmp, fits[rownames(succTableTmp), "cfi"],
                              estimatorNameRV(), input$sigLvl,
                              goodColor, badColor, textColor, modelsAbbrev)
              } else {
                NULL
              }
            } # function(model)
          ) # lapply

          #### Chisq comparison table ----
          combCompTable <- matrix(NA, nrow = 5, ncol = 10)

          combCompTable[, seq(1, 10, 2)] <- matrix(compTable$df, ncol = 5, nrow = 5)
          combCompTable[, seq(2, 10, 2)] <- matrix(compTable$chisq, ncol = 5, nrow = 5)

          colnames(combCompTable) <- rep(
            c("&Delta;df", paste0(estimatorNameRV(), "-&Delta;&chi;&sup2;")),
            times = 5)

          headerNames <- c(1, rep(2, 5))
          names(headerNames) <- c(" ", modelsAbbrev)

          #### AIC comparison table ----
          dim(infCompTable$aic) <- dim(infCompTable$bic) <- c(5, 5)

          rownames(combCompTable) <-
            rownames(infCompTable$aic) <-
            rownames(infCompTable$bic) <-
            colnames(infCompTable$aic) <-
            colnames(infCompTable$bic) <-
              modelsAbbrev

          #### put them in a tab ----
          output[[modelTestsContStr]] <<- renderUI({

            fluidPage(

              fluidRow(
                shinydashboard::box(
                  title = "Hierarchical model comparison plot:",
                  width = 12,
                  plotOutput(paste0("hierPlot", groupName)))),

              fluidRow(
                shinydashboard::box(
                  title = "Hierarchical model comparison table:",
                  width = 12,
                  paste0(
                    "<table align = \"center\", width = \"100%\"><tr><td>",
                    hierTables[[1]],
                    "</td><td>&nbsp;</td><td>",
                    hierTables[[2]],
                    "</td></tr></table>") %>%
                    HTML(),
                  actionLink(paste0("showLegendHierTable", groupAppend), "Show/hide legend"), # , style = "margin-left: 15px"),
                  conditionalPanel(paste0("input.showLegendHierTable", groupAppend, " % 2 == 1"),
                                   makeLegend("hierTables", estimatorNameRV(), input$sigLvl,
                                              goodColor, badColor, neutrColor, textColor)))),

              fluidRow(
                shinydashboard::box(
                  title = "Fit index table",
                  width = 12,
                  HTML(makeFitsTable(fits, estimatorNameRV(), input$sigLvl,
                                     goodColor, badColor, neutrColor, textColor,
                                     modelsAbbrev)),
                  br(),
                  actionLink(paste0("showLegendFitIndexTable", groupAppend), "Show/hide legend"), # , style = "margin-left: 15px"),
                  conditionalPanel(paste0("input.showLegendFitIndexTable", groupAppend, " % 2 == 1"),
                                   makeLegend("fitIndexTable", estimatorNameRV(), input$sigLvl,
                                              goodColor, badColor, neutrColor, textColor)))),

              fluidRow(
                shinydashboard::box(
                  title = HTML("&chi;&sup2;-Comparison Table:"),
                  width = 12,
                  shinyCTT:::makeKable(combCompTable, bold_cols = 1) %>%
                    kableExtra::add_header_above(headerNames, escape = FALSE) %>%
                    HTML(),
                  br(),
                  actionLink(paste0("showLegendCombCompTable", groupAppend), "Show/hide legend"), # , style = "margin-left: 15px"),
                  conditionalPanel(paste0("input.showLegendCombCompTable", groupAppend, " % 2 == 1"),
                                   makeLegend("combCompTable", estimatorNameRV(), input$sigLvl,
                                              goodColor, badColor, neutrColor, textColor)))),

              fluidRow(
                shinydashboard::box(
                  title = "AIC/BIC-Comparison Table:",
                  width = 12,
                  paste0(
                    "<table align = \"center\", width = \"100%\"> <tr><td>
                      <table align = \"center\"> <tr><td>
                        <h5>AIC:</h5>",

                    shinyCTT:::makeKable(infCompTable$aic, bold_cols = 1),

                    "</td></tr></table>
                  </td>
                  <td>&nbsp;</td>
                  <td>
                    <table align = \"center\"> <tr><td>
                      <h5>BIC:</h5>",

                    shinyCTT:::makeKable(infCompTable$bic, bold_cols = 1),

                  "</td></tr></table>
                </td></tr></table>") %>%
                HTML(),
                actionLink(paste0("showLegendInfCompTable", groupAppend), "Show/hide legend"), # , style = "margin-left: 15px"),
                conditionalPanel(paste0("input.showLegendInfCompTable", groupAppend, " % 2 == 1"),
                                 makeLegend("infCompTable", estimatorNameRV(), input$sigLvl,
                                            goodColor, badColor, neutrColor, textColor))))

            ) # fluidPage
          })

        } ### if there are no good models ----
        else {

          output[[modelTestsContStr]] <- renderUI({

            tagList(
              wellPanel(
                h5(sprintf("Lavaan status: %i warnings, %i errors.",
                           sum(warns),
                           sum(errs))),
                lavErrsMsg,
                lavWarnsMsg))
          })
        }
      }
    )
  }) # observeEvent(input$goModels, {
}
