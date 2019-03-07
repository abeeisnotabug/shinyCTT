function(input, output, session) {
    notifications <- reactiveValues(notList = list())

    output$infoMenu <- shinydashboard::renderMenu({
        if (any(sapply(notifications$notList, grepl, pattern = "danger")))
            status <- "danger"
        else
            status <- "primary"

        shinydashboard::dropdownMenu(
            type = "notifications",
            .list = notifications$notList,
            badgeStatus = status
        )
    })

    dataMenuList <- reactiveValues()
    dataMenuList$menuList <- list(
        shinydashboard::menuItem(
            "1. Data selection",
            tabName = "dataSelectionTab",
            icon = icon("database")
        ),
        hr(),
        shinydashboard::menuItem(
            "Reload",
            tabName = "reloadTab",
            icon = icon("refresh"),
            selected = FALSE
        )
    )

    output$dataMenuOut <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
            id = "dataMenu",
            .list = dataMenuList$menuList
        )
    })

    output$objectsInWorkspace <- renderUI({
        selectInput(
            "objectFromWorkspace",
            "1b. Choose data object from Workspace",
            Filter(function(object) !is.null(dim(get(object))) && typeof(get(object)) != "character",
                   ls(envir = globalenv()))
        )
    })

    userDataRaw <- reactiveVal()
    userDataChosen <- reactiveVal()
    userDataGroup <- reactiveVal()

    observeEvent(
        list(
            input$source,
            input$objectFromWorkspace,
            input$CSVFile,
            input$SPSSFile,
            input$header,
            input$sep,
            input$quote
        ),
        {
            userDataRaw(NULL)
            shinyjs::disable("dataSelectButton")
            notifications$notList$noData <- shinydashboard::notificationItem(
                text = "No data selected",
                icon = icon("times"),
                status = "danger"
            )

            if (input$source == "CSV") {
                req(input$CSVFile)

                userDataTmp <- read.csv(
                    file = input$CSVFile$datapath,
                    header = input$header,
                    sep = input$sep,
                    quote = input$quote,
                    stringsAsFactors = FALSE
                )
            } else if (input$source == "SPSS") {
                req(input$SPSSFile)

                userDataTmp <- haven::read_spss(file = input$SPSSFile$datapath)
            } else if (input$source == "Workspace") {
                req(input$objectFromWorkspace)

                userDataTmp <- get(input$objectFromWorkspace)
            }

            if (any(sapply(userDataTmp, is.factor)))
                userDataTmp[sapply(userDataTmp, is.factor)] <- lapply(
                    userDataTmp[sapply(userDataTmp, is.factor)],
                    as.character
                )

            userDataRaw(
                data.frame(
                    userDataTmp,
                    stringsAsFactors = FALSE
                )
            )

            notifications$notList$noData <- NULL

            # Test the data for problems
            if (!any(sapply(userDataRaw(), is.numeric)))
                notifications$notList$noNumeric <- shinydashboard::notificationItem(
                    text = "No numeric columns found",
                    icon = icon("times"),
                    status = "danger"
                )
            else
                notifications$notList$noNumeric <- NULL
            if (length(userDataRaw()) <= 1)
                notifications$notList$oneCol <- shinydashboard::notificationItem(
                    text = "Only one column found",
                    icon = icon("times"),
                    status = "danger"
                )
            else
                notifications$notList$oneCol <- NULL

            # If all is good, enable the select button
            if (
                all(
                    is.null(notifications$notList$noNumeric),
                    is.null(notifications$notList$oneCol)
                )
            )
                shinyjs::enable("dataSelectButton")
    })

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
            selected = TRUE
        )
        dataMenuList$menuList[[3]] <- hr()
        dataMenuList$menuList[[4]] <- shinydashboard::menuItem(
            "Reload",
            tabName = "reloadTab",
            icon = icon("refresh"),
            selected = FALSE
        )

        userDataChosen(userDataRaw())
    })

    observeEvent(input$dataMenu, {
        if (input$dataMenu == "reloadTab")
            shinyjs::runjs("location.reload()")
    })

    output$dataOverview <- renderDataTable(userDataRaw(), options = list(pageLength = 10))

    itemColsRV <- reactiveVal()
    groupColRV <- reactiveVal()

    output$itemColsChooser <- renderUI({
        possibleItemColumns <- colnames(userDataChosen())[sapply(userDataChosen(), is.numeric)]
        itemColsRV(length(possibleItemColumns))

        checkboxGroupInput("itemCols",
                           "2a. Select the item columns",
                           choices = possibleItemColumns,
                           selected = possibleItemColumns,
                           inline = TRUE)
    })

    output$groupColChooser <- renderUI({
        possibleGroupCols <- colnames(userDataChosen())[!(colnames(userDataChosen()) %in% input$itemCols)]
        groupColRV(length(possibleGroupCols))

        selectInput(
            "groupCol",
            "2b. Select the group column",
            choices = c(
                "No group column selected" = "noGroupSelected",
                possibleGroupCols
            )
        )
    })

    output$groupChooser <- renderUI({
        req(input$groupCol)

        if (input$groupCol != "noGroupSelected" && input$groupCol %in% colnames(userDataChosen())) {
            possibleGroups <- unique(userDataChosen()[, input$groupCol])

            if (any(c(table(userDataChosen()[, input$groupCol])) == 1)) {
                groupWarning <- "There are groups with only one observation,
                                 you might have selected an item as group column."
                possibleGroups <- NULL

                notifications$notList$noNumeric <- shinydashboard::notificationItem(
                    text = "Invalid groups found.",
                    icon = icon("times"),
                    status = "danger"
                )
            } else {
                groupWarning <- ""

                notifications$notList$noNumeric <- NULL
            }

            tagList(
                checkboxGroupInput("groups",
                                   "2c. Select which groups to include",
                                   choices = possibleGroups,
                                   selected = possibleGroups,
                                   inline = TRUE),
                helpText(groupWarning),
                conditionalPanel(
                    condition = "input.groups.length > 1",
                    shinyjs::disabled(
                        checkboxInput(
                            "doMg",
                            "Perform Multigroup Tests",
                            value = FALSE
                        )
                    )
                )
            )
        }
    })

    observeEvent(
        list(
            input$groupCol,
            input$groups,
            input$itemCols
        ), {
            #req(input$itemCols)

            if (length(input$itemCols) <= 1)
                shinyjs::disable("subsetSelectButton")
            else
                shinyjs::enable("subsetSelectButton")

            notifications$notList$numItems <- switch(
                as.character(length(input$itemCols)),
                "0" = shinydashboard::notificationItem(
                    text = "No item selected. No analysis possible.",
                    icon = icon("times"),
                    status = "danger"
                ),
                "1" = shinydashboard::notificationItem(
                    text = "Only one item selected. No analysis possible.",
                    icon = icon("times"),
                    status = "danger"
                ),
                "2" = shinydashboard::notificationItem(
                    text = HTML("Only two items selected. Unable to test the &tau;-kongeneric and
                                the ess. &tau;-equivalent model."),
                    icon = icon("exclamation-triangle"),
                    status = "warning"
                ),
                "3" = shinydashboard::notificationItem(
                    text = HTML("Only three items selected. Unable to test the &tau;-kongeneric model."),
                    icon = icon("exclamation-triangle"),
                    status = "warning"
                ),
                NULL
            )
        }
    )

    output$itemInfoBox <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = itemColsRV(),
            color = switch(
                as.character(itemColsRV()),
                "0" = "red",
                "1" = "red",
                "2" = "orange",
                "3" = "orange",
                "green"
            ),
            subtitle = "possible item column(s) found",
            icon = icon("list")
        )
    })

    output$groupInfoBox <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = groupColRV(),
            subtitle = "possible group column(s) found",
            icon = icon("users")
        )
    })

    output$naInfoBox <- shinydashboard::renderValueBox({
        naRows <- length(unique(which(is.na(userDataChosen()), arr.ind = TRUE)[, "row"]))

        shinydashboard::valueBox(
            value = naRows,
            color = if (naRows > 0) "yellow" else "green",
            subtitle = "rows with missing values found",
            icon = icon("exclamation-triangle")
        )
    })

    output$naTable <- renderUI({
        HTML(
            shinyCTT:::makeKable(data.frame(NAs = colSums(is.na(userDataChosen()))))
        )
    })

    output$obsTable <- renderUI({
        req(input$groupCol)

        if (input$groupCol != "noGroupSelected")
            HTML(shinyCTT:::makeKable(t(table(userDataChosen()[, input$groupCol], useNA = "ifany"))))
        else
            helpText("No group column selected.")
    })

    observeEvent(input$subsetSelectButton, {
        shinyjs::disable("itemCols")
        shinyjs::disable("groupCol")
        shinyjs::disable("groups")
        shinyjs::disable("subsetSelectButton")

        dataMenuList$menuList[[6]] <- dataMenuList$menuList[[4]]
        dataMenuList$menuList[[4]] <- shinydashboard::menuItem(
            "3. Statistics",
            tabName = "statisticsTab",
            icon = icon("chart-bar"),
            selected = TRUE
        )
        dataMenuList$menuList[[5]] <- hr()

        if (input$groupCol != "noGroupSelected") {
            userDataGroup(
                subset(
                    userDataChosen(),
                    subset = userDataChosen()[, input$groupCol] %in% input$groups,
                    select = c(input$groupCol, input$itemCols)
                )
            )
        } else {
            userDataGroup(
                subset(
                    userDataChosen(),
                    select = input$itemCols
                )
            )
        }
    })
}
