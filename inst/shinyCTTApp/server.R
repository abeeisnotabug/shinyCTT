function(input, output, session) {
    if (T) {
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
    naRowRV <- reactiveVal()

    output$itemColsChooser <- renderUI({
        possibleItemColumns <- colnames(userDataChosen())[sapply(userDataChosen(), is.numeric)]
        itemColsRV(length(possibleItemColumns))

        checkboxGroupInput(
            "itemCols",
            "2a. Select the item columns",
            choices = possibleItemColumns,
            selected = possibleItemColumns,
            inline = TRUE
        )
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
                checkboxGroupInput(
                    "groups",
                    "2c. Select which groups to include",
                    choices = possibleGroups,
                    selected = possibleGroups,
                    inline = TRUE
                ),
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

            if (input$dataSelectButton > 0) {
                if (length(input$itemCols) <= 1 ||
                    (input$groupCol != "noGroupSelected" && length(input$groups) == 0))
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
        naRowRV(length(unique(which(is.na(userDataChosen()), arr.ind = TRUE)[, "row"])))

        shinydashboard::valueBox(
            value = naRowRV(),
            color = if (naRowRV() > 0) "yellow" else "green",
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
        nTotal <- nrow(userDataChosen())
        nComplete <- nTotal - naRowRV()

        HTML(shinyCTT:::makeKable(data.frame(Total = nTotal, Complete = nComplete)))
    })

    output$obsPerGroupTable <- renderUI({
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

        dataMenuList$menuList[[8]] <- dataMenuList$menuList[[4]]
        dataMenuList$menuList[[4]] <- shinydashboard::menuItem(
            "3. Statistics",
            shinydashboard::menuSubItem(
                "Descriptive Statistics",
                tabName = "statisticsTab",
                selected = TRUE
            ),
            shinydashboard::menuSubItem(
                "Correlational Analysis",
                tabName = "corrTab"
            ),
            shinydashboard::menuSubItem(
                "Test on Multivariate Normality",
                tabName = "mvnTab"
            ),
            icon = icon("chart-bar"),
            startExpanded = TRUE
        )
        dataMenuList$menuList[[5]] <- shinydashboard::menuItem(
            "4. Testing Parameters",
            tabName = "testParamTab",
            icon = icon("cog")
        )
        dataMenuList$menuList[[6]] <- hr()

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

    output$descrBox <- renderUI({
        req(userDataGroup())

        table <- t(apply(
            userDataGroup()[, input$itemCols],
            2,
            function(col) c(Mean = mean(col, na.rm = TRUE),
                            Sd = sd(col, na.rm = TRUE),
                            Skew = moments::skewness(col, na.rm = TRUE),
                            Excess = moments::kurtosis(col, na.rm = TRUE) - 3))
        )

        nHeader <- c(1, 4)
        names(nHeader) <- c(
            " ",
            sprintf("n<sub>all</sub> = %i", nrow(userDataGroup()))
        )

        if (input$groupCol != "noGroupSelected") {
            groups <- unique(userDataGroup()[, input$groupCol])

            mgDescrTableList <- lapply(
                groups,
                function(group) t(
                    apply(
                        subset(
                            userDataGroup()[, input$itemCols],
                            userDataGroup()[, input$groupCol] == group
                        ),
                        2,
                        function(col)
                            c(Mean = mean(col, na.rm = TRUE), SD = sd(col, na.rm = TRUE),
                              Skew = moments::skewness(col, na.rm = TRUE),
                              Excess = moments::kurtosis(col, na.rm = TRUE) - 3)
                    )
                )
            )

            descrGroupHeader <- c(1, rep(4, length(groups)))
            names(descrGroupHeader) <- c(
                " ",
                sprintf(
                    "Group: %s (n<sub>%s</sub> = %i)",
                    groups,
                    groups,
                    c(table(userDataGroup()[, input$groupCol]))[as.character(groups)]
                )
            )

            mgDescrTableListTagged <- list()

            for (i in 1:((length(groups) + 1) %/% 2))
                mgDescrTableListTagged[i] <- kableExtra::column_spec(
                    kableExtra::add_header_above(
                        kableExtra::column_spec(
                            shinyCTT:::makeKable(
                                do.call(
                                    cbind,
                                    mgDescrTableList[(2 * i - 1):min(2 * i, length(groups))]
                                )
                            ),
                            1,
                            bold = TRUE
                        ),
                        header = descrGroupHeader[c(1, (2 * i):min(2 * i + 1, length(groups) + 1))],
                        escape = FALSE
                    ),
                    5,
                    border_right = "1px solid lightgrey"
                    #ifelse(
                    #    length(groups) > 1,
                    #    "1px solid lightgrey",
                    #    FALSE
                    #)
                )

            shinydashboard::tabBox(
                width = 6,
                title = "Descriptive statistics:",
                side = "right",
                tabPanel(
                    "Overall",
                    HTML(
                        kableExtra::add_header_above(
                            kableExtra::column_spec(
                                shinyCTT:::makeKable(table),
                                1,
                                bold = TRUE
                            ),
                            nHeader,
                            escape = FALSE
                        )
                    )
                ),
                tabPanel(
                    "Group-wise",
                    tagList(do.call(HTML, mgDescrTableListTagged))
                )
            )
        } else {
            shinydashboard::box(
                width = 6,
                title = "Descriptive statistics:",
                HTML(
                    kableExtra::add_header_above(
                        kableExtra::column_spec(
                            shinyCTT:::makeKable(table),
                            1,
                            bold = TRUE
                        ),
                        nHeader,
                        escape = FALSE
                    )
                )
            )
        }
    })

    output$histBox <- renderUI({
        output$singleHist <- renderPlot({
            ggplot2::ggplot(
                data.frame(item = userDataGroup()[, input$histItem]),
                ggplot2::aes(x = item)
            ) +
                ggplot2::geom_histogram(
                    if (input$singleDens) ggplot2::aes(y = ..density..),
                    color = "white",
                    fill = "#438BCA",
                    bins = input$singleNoBins
                ) +
                ggplot2::xlab(input$histItem) +
                ggplot2::theme_classic()
        })

        if (input$groupCol != "noGroupSelected") {
            output$groupHist <- renderPlot({
                ggplot2::ggplot(
                    data.frame(
                        group = userDataGroup()[
                            userDataGroup()[, input$groupCol] %in% input$histGroupGroups,
                            input$groupCol
                        ],
                        item = userDataGroup()[
                            userDataGroup()[, input$groupCol] %in% input$histGroupGroups,
                            input$histItemGroup
                        ]
                    ),
                    ggplot2::aes(x = item, fill = group)
                ) +
                    ggplot2::geom_histogram(
                        if (input$groupDens) ggplot2::aes(y = ..density..),
                        color = "white",
                        bins = input$groupNoBins,
                        position = "dodge"
                    ) +
                    ggplot2::xlab(input$histItemGroup) +
                    ggplot2::scale_fill_discrete(name = input$groupCol) +
                    ggplot2::theme_classic()
            })

            shinydashboard::tabBox(
                title = "Histogram:",
                side = "right",
                tabPanel(
                    "Overall",
                    fluidRow(
                        column(
                            width = 6,
                            selectInput(
                                "histItem",
                                "Select the item:",
                                input$itemCols
                            )
                        )
                    ),
                    plotOutput("singleHist"),
                    fluidRow(
                        column(
                            width = 6,
                            sliderInput(
                                "singleNoBins",
                                "Choose the number of bins:",
                                1,
                                100,
                                30,
                                1
                            )
                        ),
                        column(
                            width = 6,
                            radioButtons(
                                "singleDens",
                                "Choose the ordinate scaling:",
                                choices = c("Density" = TRUE, "Frequency" = FALSE),
                                selected = FALSE
                            )
                        )
                    )
                ),
                tabPanel(
                    "Group-wise",
                    fluidRow(
                        column(
                            width = 6,
                            selectInput(
                                "histItemGroup",
                                "Select the item:",
                                input$itemCols
                            )
                        ),
                        column(
                            width = 6,
                            checkboxGroupInput(
                                "histGroupGroups",
                                "Select the groups to include:",
                                unique(userDataGroup()[, input$groupCol]),
                                unique(userDataGroup()[, input$groupCol]),
                                inline = TRUE
                            )
                        )
                    ),
                    plotOutput("groupHist"),
                    fluidRow(
                        column(
                            width = 6,
                            sliderInput(
                                "groupNoBins",
                                "Choose the number of bins:",
                                1,
                                100,
                                30,
                                1
                            )
                        ),
                        column(
                            width = 6,
                            radioButtons(
                                "groupDens",
                                "Choose the ordinate scaling:",
                                choices = c("Density" = TRUE, "Frequency" = FALSE),
                                selected = FALSE
                            )
                        )
                    )
                )
            )
        } else {
            shinydashboard::box(
                title = "Histogram:",
                fluidRow(
                    column(
                        width = 6,
                        selectInput(
                            "histItem",
                            "Select the item:",
                            input$itemCols
                        )
                    )
                ),
                plotOutput("singleHist"),
                fluidRow(
                    column(
                        width = 6,
                        sliderInput(
                            "singleNoBins",
                            "Number of bins",
                            1,
                            100,
                            30,
                            1
                        )
                    ),
                    column(
                        width = 6,
                        radioButtons(
                            "singleDens",
                            "Choose ordinate units",
                            choices = c("Density" = TRUE, "Frequency" = FALSE),
                            selected = FALSE
                        )
                    )
                )
            )
        }
    })

    output$covMatBox <- renderUI({
        req(userDataGroup())

        table <- cov(userDataGroup()[, input$itemCols], use = "pairwise.complete.obs")
        table[upper.tri(table)] <- NA

        if (input$groupCol != "noGroupSelected") {
            groups <- unique(userDataGroup()[, input$groupCol])

            mgCovMatList <- lapply(
                groups,
                function(group)
                    cov(
                        subset(
                            userDataGroup()[, input$itemCols],
                            userDataGroup()[, input$groupCol] == group
                        ),
                        use = "pairwise.complete.obs"
                    )
            )

            for (i in 1:length(mgCovMatList))
                mgCovMatList[[i]][upper.tri(mgCovMatList[[i]])] <- NA

            mgCovMatTable <- kableExtra::column_spec(
                shinyCTT:::makeKable(do.call(rbind, mgCovMatList)),
                1,
                bold = TRUE
            )

            groupRowHeaders <- sprintf(
                "Group: %s (n = %i)",
                groups,
                c(table(userDataGroup()[, input$groupCol]))[as.character(groups)]
            )

            for (i in 1:length(groups))
                mgCovMatTable <- kableExtra::group_rows(
                    mgCovMatTable,
                    group_label = groupRowHeaders[i],
                    start_row = (i - 1) * length(input$itemCols) + 1,
                    end_row = i * length(input$itemCols),
                    label_row_css = "background-color: #666; color: #fff;"
                )

            shinydashboard::tabBox(
                width = 12,
                title = "Covariance Matrix:",
                side = "right",
                tabPanel(
                    "Overall",
                    HTML(
                        kableExtra::column_spec(
                            shinyCTT:::makeKable(table),
                            1,
                            bold = TRUE
                        )
                    )
                ),
                tabPanel(
                    "Group-wise",
                    HTML(mgCovMatTable)
                )
            )
        } else {
            shinydashboard::box(
                width = 12,
                title = "Covariance Matrix:",
                HTML(
                    kableExtra::column_spec(
                        shinyCTT:::makeKable(table),
                        1,
                        bold = TRUE
                    )
                )
            )
        }
    })

    output$corrInd <- renderUI({
        req(userDataGroup())
        req(input$corrIndEst)

        dummyModel <- paste(
            sprintf("%s ~ 1", colnames(userDataGroup()[, input$itemCols])),
            collapse = "\n"
        )

        corrIndRaw <- tryCatch(
            lavaan::cfa(
                model = dummyModel,
                data = userDataGroup(),
                estimator = input$corrIndEst
            ),
            warning = function(w) w,
            error = function(e) e
        )

        if (class(corrIndRaw)[1] == "lavaan") {
            corrInd <- unlist(shinyCTT:::extractFitParameters(corrIndRaw)[, c(2, 1, 3)])

            if (!is.na(input$corrIndSL) && input$corrIndSL < 1 && input$corrIndSL > 0) {
                if (corrInd[3] < input$corrIndSL) {
                    tagList(
                        strong("Test result:"),
                        p(
                            HTML(
                                sprintf(
                                    "The hypothesis that all correlations are equal to
                            zero has to be discarded on a significance level of
                            %s (%s-&chi;&sup2; = %.3f, df = %i, p %s).",
                                    input$corrIndSL,
                                    input$corrIndEst,
                                    corrInd[1],
                                    corrInd[2],
                                    ifelse(
                                        corrInd[3] < 0.001,
                                        "< 0.001",
                                        sprintf("= %.3f", corrInd[3]))
                                )
                            )
                        )
                    )
                } else {
                    tagList(
                        strong("Test result:"),
                        p(
                            HTML(
                                sprintf(
                                    "The hypothesis that all correlations are equal to
                            zero can be maintained on a significance level of
                            %s (%s-&chi;&sup2; = %.3f, df = %i, p %s).",
                                    input$corrIndSL,
                                    input$corrIndEst,
                                    corrInd[1],
                                    corrInd[2],
                                    ifelse(
                                        corrInd[3] < 0.001,
                                        "< 0.001",
                                        sprintf("= %.3f", corrInd[3]))
                                )
                            )
                        )
                    )
                }
            } else {
                div(
                    style = paste0("color:red"),
                    HTML("Please enter a valid significance level")
                )
            }
        } else {
            tagList(
                strong("Test result:"),
                div(
                    style = paste0("color:red"),
                    HTML(
                        paste("There was an ERROR/WARNING:",
                              corrIndRaw$message)
                    )
                )
            )
        }
    })

    output$scatterPlotBox <- renderUI({
        req(userDataGroup())

        output$singleScatter <- renderPlot({
            ggplot2::ggplot(
                data.frame(
                    itemX = userDataGroup()[, input$scatterItemX],
                    itemY = userDataGroup()[, input$scatterItemY]
                ),
                ggplot2::aes(x = itemX, y = itemY)
            ) +
                ggplot2::geom_point(color = "#438BCA") +
                ggplot2::xlab(input$scatterItemX) +
                ggplot2::ylab(input$scatterItemY) +
                ggplot2::theme_classic()
        })

        if (input$groupCol != "noGroupSelected") {
            output$groupScatter <- renderPlot({
                ggplot2::ggplot(
                    data.frame(
                        group = userDataGroup()[
                            userDataGroup()[, input$groupCol] %in% input$scatterGroupGroups,
                            input$groupCol
                        ],
                        itemX = userDataGroup()[
                            userDataGroup()[, input$groupCol] %in% input$scatterGroupGroups,
                            input$scatterItemXGroup
                        ],
                        itemY = userDataGroup()[
                            userDataGroup()[, input$groupCol] %in% input$scatterGroupGroups,
                            input$scatterItemYGroup
                        ]
                    ),
                    ggplot2::aes(x = itemX, y = itemY, color = group)
                ) +
                    ggplot2::geom_point() +
                    ggplot2::xlab(input$scatterItemXGroup) +
                    ggplot2::ylab(input$scatterItemYGroup) +
                    ggplot2::scale_color_discrete(name = input$groupCol) +
                    ggplot2::theme_classic()
            })

            shinydashboard::tabBox(
                title = "Histogram:",
                width = NULL,
                side = "right",
                tabPanel(
                    "Overall",
                    fluidRow(
                        column(
                            width = 4,
                            selectInput(
                                "scatterItemX",
                                "Select item on the abscissa:",
                                input$itemCols
                            )
                        ),
                        column(
                            width = 4,
                            selectInput(
                                "scatterItemY",
                                "Select item on the ordinate:",
                                input$itemCols,
                                selected = input$itemCols[2]
                            )
                        )
                    ),
                    plotOutput("singleScatter")
                ),
                tabPanel(
                    "Group-wise",
                    fluidRow(
                        column(
                            width = 4,
                            selectInput(
                                "scatterItemXGroup",
                                "Select item on the abscissa:",
                                input$itemCols
                            )
                        ),
                        column(
                            width = 4,
                            selectInput(
                                "scatterItemYGroup",
                                "Select item on the ordinate:",
                                input$itemCols,
                                selected = input$itemCols[2]
                            )
                        ),
                        column(
                            width = 4,
                            checkboxGroupInput(
                                "scatterGroupGroups",
                                "Select the groups to include:",
                                unique(userDataGroup()[, input$groupCol]),
                                unique(userDataGroup()[, input$groupCol]),
                                inline = TRUE
                            )
                        )
                    ),
                    plotOutput("groupScatter")
                )
            )
        } else {
            shinydashboard::box(
                title = "Scatter Plot:",
                width = NULL,
                fluidRow(
                    column(
                        width = 4,
                        selectInput(
                            "scatterItemX",
                            "Select item on the abscissa:",
                            input$itemCols
                        )
                    ),
                    column(
                        width = 4,
                        selectInput(
                            "scatterItemY",
                            "Select item on the ordinate:",
                            input$itemCols,
                            selected = input$itemCols[2]
                        )
                    )
                ),
                plotOutput("singleScatter")
            )
        }
    })

    output$corrTableBox <- renderUI({
        req(userDataGroup())

        corrTableWithCIsRaw <- list(
            cor = tryCatch(
                cor(userDataGroup()[, input$itemCols], use = input$corrTabNA),
                warning = function(w) NULL,
                error = function(e) NULL
            ),
            test = tryCatch(
                corrplot::cor.mtest(userDataGroup()[, input$itemCols],
                                    conf.level = (1 - input$corrTabSL)),
                warning = function(w) w,
                error = function(e) e
            )
        )

        corrTableLegend <- tagList(
            h5("Legend:"),
            HTML(shinyCTT:::makeKable(
                cbind(
                    kableExtra::cell_spec(
                        "Sig. pos.",
                        color = textColor,
                        background = goodColor
                    ),
                    kableExtra::cell_spec(
                        "Sig. neg.",
                        color = textColor,
                        background = badColor
                    ),
                    kableExtra::cell_spec(
                        "Insig.",
                        color = textColor,
                        background = neutrColor
                    )
                ),
                bootstrap_options = "bordered",
                position = "left"
            ))
        )

        if (class(corrTableWithCIsRaw$test)[1] == "list") {
            singleCorrTable <- HTML(
                kableExtra::column_spec(
                    shinyCTT:::makeKable(
                        shinyCTT:::makeCorrTableWithCIs(
                            corrTableWithCIsRaw,
                            goodColor,
                            badColor,
                            neutrColor,
                            textColor,
                            input$corrTabSL,
                            input$itemCols
                        ),
                        bootstrap_options = c("condensed", "striped")
                    ),
                    1,
                    bold = TRUE
                )
            )
        } else {
            singleCorrTable <- div(
                style = paste0("color:red"),
                HTML(paste("There was an ERROR/WARNING:", corrTableWithCIsRaw$test))
            )
        }

        if (input$groupCol != "noGroupSelected") {
            mgCorrTableList <- lapply(
                unique(userDataGroup()[, input$groupCol]),
                function(group)
                    shinyCTT:::makeCorrTableWithCIs(
                        list(
                            cor = suppressWarnings(cor(
                                subset(
                                    userDataGroup()[, input$itemCols],
                                    userDataGroup()[, input$groupCol] == group
                                ),
                                use = input$corrTabNA
                            )),
                            test = corrplot::cor.mtest(
                                subset(
                                    userDataGroup()[, input$itemCols],
                                    userDataGroup()[, input$groupCol] == group
                                ),
                                conf.level = (1 - input$corrTabSL)
                            )
                        ),
                        goodColor,
                        badColor,
                        neutrColor,
                        textColor,
                        input$corrTabSL,
                        input$itemCols
                    )
            )

            mgCorrTable <- kableExtra::column_spec(
                shinyCTT:::makeKable(
                    do.call(rbind, mgCorrTableList),
                    bootstrap_options = c("condensed", "striped")
                ),
                1,
                bold = TRUE
            )

            groupRowHeaders <- sprintf(
                "Group: %s",
                unique(userDataGroup()[, input$groupCol])
            )

            for (i in 1:length(unique(userDataGroup()[, input$groupCol])))
                mgCorrTable <- kableExtra::group_rows(
                    mgCorrTable,
                    group_label = groupRowHeaders[i],
                    start_row = (i - 1) * length(input$itemCols) * 2 + 1,
                    end_row = i * length(input$itemCols) * 2,
                    label_row_css = "background-color: #666; color: #fff;"
                )

            shinydashboard::tabBox(
                width = 12,
                title = "Correlation Table with Confidence Intervals:",
                side = "right",
                tabPanel(
                    "Overall",
                    singleCorrTable,
                    corrTableLegend
                ),
                tabPanel(
                    "Group-wise",
                    HTML(mgCorrTable),
                    corrTableLegend
                )
            )
        } else {
            shinydashboard::box(
                width = 12,
                title = "Correlation Table with Confidence Intervals:",
                singleCorrTable,
                corrTableLegend
            )
        }
    })

    mvnTestResult <- reactiveValues(
        raw = NULL,
        estimator = "ML"
    )

    observeEvent(input$estimator, {
        mvnTestResult$estimator <- input$estimator
    })

    output$mvnTable <- renderUI({
        req(userDataGroup())

        mvnTestResult$raw <- tryCatch(
            MVN::mvn(userDataGroup()[, input$itemCols]),
            warning = function(w) w,
            error = function(e) e
        )

        #req(mvnTestResult$raw)

        if (class(mvnTestResult$raw$multivariateNormality) == "data.frame") {
            mvnTestResult$estimator <- ifelse(
                any(
                    as.numeric(
                        as.character(
                            mvnTestResult$raw$multivariateNormality[-3, "p value"]
                        )
                    ) < input$mvnSL
                ),
                "MLR",
                "ML"
            )

            updateRadioButtons(
                session,
                "estimator",
                selected = mvnTestResult$estimator
            )
        }

        if (class(mvnTestResult$raw)[1] == "list") {
            mvnUV <- data.frame(Test = as.character(mvnTestResult$raw$univariateNormality$Test),
                                Item = as.character(mvnTestResult$raw$univariateNormality$Variable),
                                Statistic = as.numeric(mvnTestResult$raw$univariateNormality$Statistic),
                                p = suppressWarnings(as.numeric(mvnTestResult$raw$univariateNormality$`p value`)),
                                stringsAsFactors = F)

            mvnUV$p[is.na(mvnUV$p)] <- 0
            mvnUV$Signif. <- ifelse(mvnUV$p < input$mvnSL, "*", "")
            mvnUV$p <- ifelse(
                mvnUV$p < 0.001,
                "< 0.001",
                sprintf("%.3f", round(mvnUV$p, 3))
            )

            HTML(shinyCTT:::makeKable(mvnUV, bootstrap_options = "basic"))
        } else {
            div(
                style = paste0("color:red"),
                HTML(paste("There was an ERROR/WARNING:", mvnTestResult$raw$message))
            )
        }
    })

    output$mvnComment <- renderUI({
        req(userDataGroup())

        if (class(mvnTestResult$raw$multivariateNormality) == "data.frame") {
            mvnMV <- data.frame(Test = as.character(mvnTestResult$raw$multivariateNormality$Test),
                                Statistic = as.numeric(as.character(mvnTestResult$raw$multivariateNormality$Statistic)),
                                p = as.numeric(as.character(mvnTestResult$raw$multivariateNormality$`p value`)),
                                Signif. = as.character(mvnTestResult$raw$multivariateNormality$Result),
                                stringsAsFactors = F)[-3,]

            mvnMV$Signif. <- ifelse(mvnMV$p < input$mvnSL, "*", "")
            mvnMV$p <- ifelse(
                mvnMV$p < 0.001,
                "< 0.001",
                sprintf("%.3f", round(mvnMV$p, 3))
            )

            if ("*" %in% mvnMV$Signif.) {
                tagList(
                    sprintf("At least one of the hypotheses that Mardia's Skewness statistic
                            or Mardias' Kurtosis statistic matches one of a
                            normal distribution has to be discarded on a significance
                            level of %s. Test result:", input$mvnSL),
                    HTML(shinyCTT:::makeKable(mvnMV, bootstrap_options = "basic")),
                    HTML("It is thus recommended to continue with the <b>Robust Maximum Likelihood (MLR)</b> estimator.")
                )
            } else {
                tagList(
                    sprintf("The hypotheses that Mardia's Skewness statistic
                        and Mardias' Kurtosis statistic match those of a
                        normal distribution can be maintained on a significance
                        level of %s. Test result:", input$mvnSL),
                    HTML(shinyCTT:::makeKable(mvnMV, bootstrap_options = "basic")),
                    HTML("It is thus recommended to continue with the <b>Maximum Likelihood (ML)</b> estimator.")
                )
            }
        }
    })

    output$mvnPlotBox <- renderUI({
        output$mvnPlot <- renderPlot({

            if (input$mvnPlotType == "qq")
                tryCatch(
                    MVN::mvn(userDataGroup()[, input$itemCols], multivariatePlot = input$mvnPlotType),
                    warning = function(w) w,
                    error = function(e) e
                )
            else
                tryCatch(
                    MVN::mvn(userDataGroup()[, c(input$mvnItemX, input$mvnItemY)], multivariatePlot = input$mvnPlotType),
                    warning = function(w) w,
                    error = function(e) e
                )
        })

        shinydashboard::box(
            width = NULL,
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
                            "Contour Plot" = "contour"
                        )
                    )
                ),
                column(
                    width = 4,
                    conditionalPanel(
                        "input.mvnPlotType != 'qq'",
                        selectInput(
                            "mvnItemX",
                            "Select item on the abscissa:",
                            input$itemCols
                        )
                    )
                ),
                column(
                    width = 4,
                    conditionalPanel(
                        "input.mvnPlotType != 'qq'",
                        selectInput(
                            "mvnItemY",
                            "Select item on the ordinate:",
                            input$itemCols,
                            selected = input$itemCols[2]
                        )
                    )
                )
            ),
            plotOutput("mvnPlot")
        )
    })
}
