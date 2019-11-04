function(input, output, session) {
    # Preparation: Names and colors ----------------------------------------------------------------------------------------
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

    # Set up notifications -------------------------------------------------------------------------------------------------
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

    # Set up sidebar -------------------------------------------------------------------------------------------------------
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

    # Data selection tab --------------------------------------------------------------------------------------------------
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

    # Display NAs correctly in datatable
    options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

    observeEvent(userDataRaw(), {
        output$dataOverview <- DT::renderDataTable(
            DT::formatRound(
                DT::datatable(userDataRaw()),
                columns = seq_along(userDataRaw())[sapply(userDataRaw(), is.numeric)],
                digits = 3
            )
        )
    })

    itemColsRV <- reactiveVal()
    groupColRV <- reactiveVal()
    incompleteCasesRV <- reactiveVal()

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
                helpText(groupWarning)
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
            color = "blue",
            subtitle = "possible group column(s) found",
            icon = icon("users")
        )
    })

    output$naInfoBox <- shinydashboard::renderValueBox({
        incompleteCasesRV(!complete.cases(userDataChosen()))
        output$incompleteCasesBoolRV <- reactive({any(incompleteCasesRV())})
        outputOptions(output, "incompleteCasesBoolRV", suspendWhenHidden = FALSE)

        shinydashboard::valueBox(
            value = sum(incompleteCasesRV()),
            color = if (any(incompleteCasesRV())) "yellow" else "green",
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
        nComplete <- sum(!incompleteCasesRV())

        tagList(
            HTML(shinyCTT:::makeKable(data.frame(Total = nTotal, Complete = nComplete))),
            checkboxInput(
                "excludeIncompleteCases",
                "Exclude incomplete cases"
            )
        )
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
        shinyjs::disable("excludeIncompleteCases")

        dataMenuList$menuList[[7]] <- dataMenuList$menuList[[4]]
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

            if (input$excludeIncompleteCases) {
                ccSubset <- (userDataChosen()[, input$groupCol] %in% input$groups) & !incompleteCasesRV()
            } else {
                ccSubset <- userDataChosen()[, input$groupCol] %in% input$groups
            }

            userDataGroup(
                subset(
                    userDataChosen(),
                    subset = ccSubset,
                    select = c(input$groupCol, input$itemCols)
                )
            )
        } else {

            if (input$excludeIncompleteCases) {
                ccSubset <- !incompleteCasesRV()
            } else {
                ccSubset <- rep(TRUE, nrow(userDataChosen()))
            }

            userDataGroup(
                subset(
                    userDataChosen(),
                    subset = ccSubset,
                    select = input$itemCols
                )
            )
        }

        if (input$groupCol != "noGroupSelected" &&
            !any(c(table(userDataGroup()[, input$groupCol])) == 1) &&
            length(input$groups) > 1) {
            shinyjs::enable("doMg")

            updateCheckboxInput(
                session,
                "doMg",
                value = TRUE
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
                    MVN::mvn(userDataGroup()[complete.cases(userDataGroup()), input$itemCols], multivariatePlot = input$mvnPlotType),
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
            conditionalPanel(
                "input.mvnPlotType == 'qq' && !input.excludeIncompleteCases && output.incompleteCasesBoolRV",
                helpText("Warning: Incomplete cases have been removed from the data for the Q-Q Plot.")
            ),
            plotOutput("mvnPlot")
        )
    })

    observeEvent(input$sigLvl, {
        if ((input$sigLvl < 0 | input$sigLvl > 1) && !is.na(input$sigLvl)) {
            updateNumericInput(session,
                               "sigLvl",
                               value = 0.05)
        }
    })

    observeEvent(input$goModels, {
        dataMenuList$menuList[[12]] <- dataMenuList$menuList[[7]]
        if (input$doMg) {
            dataMenuList$menuList[[7]] <- shinydashboard::menuItem(
                "5. Model Comparison Tests",
                shinydashboard::menuSubItem(
                    "Single Group",
                    tabName = "modelTests",
                    selected = TRUE
                ),
                shinydashboard::menuSubItem(
                    "Multigroup",
                    tabName = "modelTestsMg"
                ),
                icon = icon("chart-bar"),
                startExpanded = TRUE
            )
            dataMenuList$menuList[[8]] <- shinydashboard::menuItem(
                "6. Parameter Tables",
                shinydashboard::menuSubItem(
                    "Single Group",
                    tabName = "parTables"
                ),
                shinydashboard::menuSubItem(
                    "Multigroup",
                    tabName = "parTablesMg"
                ),
                icon = icon("chart-bar")
            )
            dataMenuList$menuList[[9]] <- shinydashboard::menuItem(
                "7. Factor Scores",
                shinydashboard::menuSubItem(
                    "Single Group",
                    tabName = "facScores"
                ),
                shinydashboard::menuSubItem(
                    "Multigroup",
                    tabName = "facScoresMg"
                ),
                icon = icon("chart-bar")
            )
            dataMenuList$menuList[[10]] <- shinydashboard::menuItem(
                "8. Model Code",
                shinydashboard::menuSubItem(
                    "Single Group",
                    tabName = "modelCode"
                ),
                shinydashboard::menuSubItem(
                    "Multigroup",
                    tabName = "modelCodeMg"
                ),
                icon = icon("chart-bar")
            )
        } else {
            dataMenuList$menuList[[7]] <- shinydashboard::menuItem(
                "5. Model Comparison Tests",
                shinydashboard::menuSubItem(
                    "Single Group",
                    tabName = "modelTests",
                    selected = TRUE
                ),
                icon = icon("chart-bar"),
                startExpanded = TRUE
            )
            dataMenuList$menuList[[8]] <- shinydashboard::menuItem(
                "6. Parameter Tables",
                shinydashboard::menuSubItem(
                    "Single Group",
                    tabName = "parTables"
                ),
                icon = icon("chart-bar")
            )
            dataMenuList$menuList[[9]] <- shinydashboard::menuItem(
                "7. Factor Scores",
                shinydashboard::menuSubItem(
                    "Single Group",
                    tabName = "facScores"
                ),
                icon = icon("chart-bar")
            )
            dataMenuList$menuList[[10]] <- shinydashboard::menuItem(
                "8. Model Code",
                shinydashboard::menuSubItem(
                    "Single Group",
                    tabName = "modelCode"
                ),
                icon = icon("chart-bar")
            )
        }
        dataMenuList$menuList[[11]] <- hr()

        modelsToTest <- models[sapply(models, function(thisModel) input[[thisModel]])]

        lapply(
            append(list(FALSE), if (isTRUE(input$doMg)) input$groupCol),
            function(groupName) {

                # Try fitting and capture warning and error messages -------------------------------------------------------
                modelCodes <- shinyCTT:::makeModelCodes(inputData = userDataGroup(),
                                                        itemCols = input$itemCols,
                                                        group = groupName)

                if (isFALSE(groupName)) {
                    fittedModelsWarns <- lapply(
                        modelCodes[modelsToTest],
                        FUN = function(model) {
                            tryCatch(lavaan::cfa(model = model,
                                                 data = userDataGroup(),
                                                 meanstructure = TRUE,
                                                 estimator = mvnTestResult$estimator),
                                     error = function(e) e,
                                     warning = function(w) w)
                        }
                    )
                    fittedModelsErrs <- lapply(
                        modelCodes[modelsToTest],
                        FUN = function(model) {
                            suppressWarnings(
                                tryCatch(lavaan::cfa(model = model,
                                                     data = userDataGroup(),
                                                     meanstructure = TRUE,
                                                     estimator = mvnTestResult$estimator),
                                         error = function(e) e)
                            )
                        }
                    )
                } else {
                    fittedModelsWarns <- lapply(
                        modelCodes[modelsToTest],
                        FUN = function(model) {
                            tryCatch(lavaan::cfa(model = model,
                                                 data = userDataGroup(),
                                                 meanstructure = TRUE,
                                                 group = groupName,
                                                 group.equal = c("loadings", "intercepts"),
                                                 estimator = mvnTestResult$estimator),
                                     error = function(e) e,
                                     warning = function(w) w)
                        }
                    )
                    fittedModelsErrs <- lapply(
                        modelCodes[modelsToTest],
                        FUN = function(model) {
                            suppressWarnings(
                                tryCatch(lavaan::cfa(model = model,
                                                     data = userDataGroup(),
                                                     meanstructure = TRUE,
                                                     group = groupName,
                                                     group.equal = c("loadings", "intercepts"),
                                                     estimator = mvnTestResult$estimator),
                                         error = function(e) e)
                            )
                        }
                    )
                }

                # Warning and error counting and capturing -----------------------------------------------------------------
                warns <- sapply(
                    lapply(fittedModelsWarns, class),
                    function(code) code[1] == "simpleWarning"
                )
                errs <- sapply(
                    lapply(fittedModelsErrs, class),
                    function(code) code[1] == "simpleError"
                )

                goodModels <- modelsToTest[!warns & !errs]
                errModels <- modelsToTest[errs]
                warnModels <- modelsToTest[warns]

                if (sum(warns) > 0) {
                    lavWarnsMsg <- tagList(
                        h6("The following models produced warnings:"),
                        div(style = "color:orange",
                            HTML(
                                kableExtra::column_spec(
                                    kableExtra::kable(
                                        cbind(
                                            paste0(modelsLong[warnModels],
                                                   ":&emsp;"),
                                            sapply(fittedModelsWarns[warnModels],
                                                   function(model) model$message)),
                                        row.names = FALSE,
                                        escape = FALSE),
                                    1, bold = TRUE
                                )
                            )
                        )
                    )
                } else {
                    lavWarnsMsg <- NULL
                }

                if (sum(errs) > 0) {
                    lavErrsMsg <- tagList(
                        h6("The following models produced errors:"),
                        div(style = "color:red",
                            HTML(
                                kableExtra::column_spec(
                                    kableExtra::kable(
                                        cbind(
                                            paste0(modelsLong[errModels],
                                                   ":&emsp;"),
                                            sapply(fittedModelsErrs[errModels],
                                                   function(model) model$message)),
                                        row.names = FALSE,
                                        escape = FALSE),
                                    1, bold = TRUE
                                )
                            )
                        )
                    )
                } else {
                    lavErrsMsg <- NULL
                }

                # Generate comparative fit table and tab  ------------------------------------------------------------------
                fits <- do.call(rbind, lapply(fittedModelsWarns[goodModels], shinyCTT:::extractFitParameters))
                comps <- possComps[sapply(possComps, function(thisComp) input[[thisComp]])]

                succTable <- list()

                if (length(goodModels) > 1 && !identical(goodModels, c(teq = "teq", etp = "etp"))) {
                    if ("teq" %in% goodModels) {
                        succTable$teq <- do.call(
                            lavaan::lavTestLRT,
                            args = c(object = fittedModelsWarns[[goodModels[1]]],
                                     ... = fittedModelsWarns[goodModels[-c(1, which(goodModels == "etp"))]])
                        )

                        rownames(succTable$teq) <- goodModels[which(goodModels != "etp")]
                    }
                    if ("etp" %in% goodModels) {
                        succTable$etp <- do.call(
                            lavaan::lavTestLRT,
                            args = c(object = fittedModelsWarns[[goodModels[1 + (goodModels[1] == "teq")]]],
                                     ... = fittedModelsWarns[goodModels[-c(1 + (goodModels[1] == "teq"),
                                                                           which(goodModels == "teq"))]])
                        )

                        rownames(succTable$etp) <- goodModels[which(goodModels != "teq")]
                    }
                    if (!any(c("teq", "etp") %in% goodModels)) {
                        succTable$teq <- do.call(
                            lavaan::lavTestLRT,
                            args = c(object = fittedModelsWarns[[goodModels[1]]],
                                     ... = fittedModelsWarns[goodModels[-1]])
                        )

                        rownames(succTable$teq) <- goodModels
                    }
                }

                compTable <- reactiveValues(
                    df = matrix(ncol = 5, nrow = 5),
                    chisq = matrix(ncol = 5, nrow = 5)
                )

                infCompTable <- reactiveValues(
                    aic = matrix(ncol = 5, nrow = 5),
                    bic = matrix(ncol = 5, nrow = 5)
                )

                names(compTable$df) <-
                    names(compTable$chisq) <-
                    names(infCompTable$aic) <-
                    names(infCompTable$bic) <- outer(models, models, paste0)

                compTable$chisq[lower.tri(diag(5), diag = TRUE)] <-
                    infCompTable$aic[lower.tri(diag(5), diag = TRUE)] <-
                    infCompTable$bic[lower.tri(diag(5), diag = TRUE)] <- "<span style=\"color: lightgrey;\" >X</span>"

                # Make tabs for single/multigroup --------------------------------------------------------------------------
                #appendTab(
                #    inputId = "parTabsets",
                #    tabPanel(
                #        ifelse(isFALSE(groupName),
                #               "Singlegroup",
                #               "Multigroup"),
                #        tabsetPanel(
                #            id = paste0("parTabsetTab", c("Mg")[!isFALSE(groupName)])
                #        )
                #    ),
                #    select = isFALSE(groupName)
                #)

                # Generate Paramter Tables, Fits and Fit Tables ------------------------------------------------------------
                for (model in goodModels) {
                    local({
                        thisModel <- model
                        whichModel <- which(goodModels == thisModel)

                        # Write to diag(chisq comp table) ------------------------------------------------------------------
                        if (fits[thisModel, "pvalue"] < 0.05) {
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

                        compTable$chisq[
                            paste0(thisModel, thisModel)
                            ] <- kableExtra::cell_spec(
                                sprintf(paste0("%.3f", sigAddon), fits[thisModel, "chisq"]),
                                background = sigColor,
                                color = sigTxtColor,
                                italic = TRUE
                            )
                        compTable$df[
                            paste0(thisModel, thisModel)
                            ] <- kableExtra::cell_spec(
                                sprintf("%i", fits[thisModel, "df"]),
                                background = sigColor,
                                color = sigTxtColor,
                                italic = TRUE
                            )

                        # Write to aic/bic comp table ----------------------------------------------------------------------
                        infCompTable$aic[
                            paste0(thisModel, thisModel)
                            ] <- kableExtra::cell_spec(sprintf("%.3f", fits[thisModel, "aic"]),
                                                       color = textColor,
                                                       background = neutrColor)

                        infCompTable$bic[
                            paste0(thisModel, thisModel)
                            ] <- kableExtra::cell_spec(sprintf("%.3f", fits[thisModel, "bic"]),
                                                       color = textColor,
                                                       background = neutrColor)

                        if (whichModel > 1) {
                            aicDiffs <- fits[thisModel, "aic"] - fits[1:(whichModel - 1), "aic"]
                            bicDiffs <- fits[thisModel, "bic"] - fits[1:(whichModel - 1), "bic"]

                            infCompTable$aic[paste0(thisModel, rownames(fits)[1:(whichModel - 1)])] <-
                                kableExtra::cell_spec(
                                    sprintf(
                                        ifelse(
                                            aicDiffs < 0,
                                            "%.3f",
                                            "+%.3f"
                                        ),
                                        aicDiffs
                                    ),
                                    color = textColor,
                                    background = ifelse(aicDiffs < 0, goodColor, badColor)
                                )
                            infCompTable$bic[paste0(thisModel, rownames(fits)[1:(whichModel - 1)])] <-
                                kableExtra::cell_spec(
                                    sprintf(
                                        ifelse(
                                            bicDiffs < 0,
                                            "%.3f",
                                            "+%.3f"
                                        ),
                                        bicDiffs
                                    ),
                                    color = textColor,
                                    background = ifelse(bicDiffs < 0, goodColor, badColor)
                                )
                        }

                        # Write to lower.tri(chisq comp table) -------------------------------------------------------------
                        compsWithThisModel <- substring(
                            comps[grep(thisModel, substr(comps, 1, 3))],
                            4,
                            6
                        )

                        compsWithThisModel <- compsWithThisModel[compsWithThisModel %in% goodModels]
                        names(compsWithThisModel) <- compsWithThisModel

                        fitCompsWithThisModel <- sapply(
                            compsWithThisModel,
                            function(thisComp) {
                                tmpTbl <- lavaan::lavTestLRT(fittedModelsWarns[[thisModel]], fittedModelsWarns[[thisComp]])
                                unlist(tmpTbl[2, c(5, 6, 7)])
                            }
                        )

                        for (thisComp in compsWithThisModel) {
                            if (fitCompsWithThisModel[3, thisComp] < 0.05) {
                                sigAddon <- "*"
                                sigColor <- badColor
                                sigTxtColor <- textColor

                                if (fitCompsWithThisModel[3, thisComp] < 0.01)
                                    sigAddon <- paste0(sigAddon, "*")
                                if (fitCompsWithThisModel[3, thisComp] < 0.001)
                                    sigAddon <- paste0(sigAddon, "*")

                            } else {
                                sigAddon <- ""
                                sigColor <- goodColor
                                sigTxtColor <- textColor
                            }

                            compTable$chisq[
                                paste0(thisModel, thisComp)
                                ] <- kableExtra::cell_spec(
                                    sprintf(paste0("+%.3f", sigAddon), fitCompsWithThisModel[1, thisComp]),
                                    background = sigColor,
                                    color = sigTxtColor
                                )

                            compTable$df[
                                paste0(thisModel, thisComp)
                                ] <- kableExtra::cell_spec(
                                    sprintf("+%i", fitCompsWithThisModel[2, thisComp]),
                                    background = sigColor,
                                    color = sigTxtColor
                                )
                        }

                        # Parameter tables ---------------------------------------------------------------------------------
                        parTableWithCIs <- kableExtra::add_header_above(
                            kableExtra::row_spec(
                                kableExtra::column_spec(
                                    shinyCTT:::makeKable(
                                        shinyCTT:::extractParameters(
                                            fittedModelsWarns[[thisModel]],
                                            alpha = input$sigLvl
                                        ),
                                        col.names = c(
                                            "Item",
                                            "&lambda;&#x302;<sub>i</sub>",
                                            "Est.", paste0(c("SE", "CI"),
                                                           "<sub>",
                                                           mvnTestResult$estimator,
                                                           "</sub>"),
                                            "Std. Est.", paste0(c("SE", "CI"),
                                                                "<sub>",
                                                                mvnTestResult$estimator,
                                                                "</sub>"),
                                            "&alpha;&#x302;<sub>i</sub>",
                                            "Est.", paste0(c("SE", "CI"),
                                                           "<sub>",
                                                           mvnTestResult$estimator,
                                                           "</sub>"),
                                            "&sigma;&#x302;&sup2;<sub>&epsilon;<sub>i</sub></sub>",
                                            "Est.", paste0(c("SE", "CI"),
                                                           "<sub>",
                                                           mvnTestResult$estimator,
                                                           "</sub>"),
                                            "R&#x302;<sub>i</sub>",
                                            "Est.", paste0(c("SE", "CI"),
                                                           "<sub>",
                                                           mvnTestResult$estimator,
                                                           "</sub>")
                                        )
                                    ),
                                    1,
                                    bold = TRUE),
                                (length(input$itemCols) + 1) * 1:fittedModelsWarns[[thisModel]]@Data@ngroups,
                                bold = TRUE),
                            c(" ",
                              "Discrimination Parameters (Factor Loadings)" = 7,
                              "Easiness Parameters" = 4,
                              "Variances" = 4,
                              "Reliabilities" = 4)
                        )

                        if (!isFALSE(groupName))
                            for (i in 1:fittedModelsWarns[[thisModel]]@Data@ngroups) {
                                groupRowHeaders <- sprintf(
                                    "Group: %s",
                                    fittedModelsWarns[[thisModel]]@Data@group.label
                                )

                                parTableWithCIs <- kableExtra::group_rows(
                                    parTableWithCIs,
                                    group_label = groupRowHeaders[i],
                                    start_row = (i - 1) * (length(input$itemCols) + 1) + 1,
                                    end_row = i * (length(input$itemCols) + 1),
                                    label_row_css = "background-color: #666; color: #fff;"
                                )
                            }

                        # Factor Scores ------------------------------------------------------------------------------------
                        output[[
                            paste0(thisModel, "Scores", c("Mg")[!isFALSE(groupName)])
                            ]] <<- DT::renderDataTable(
                                shinyCTT:::getPredictedScores(
                                    fittedModelsWarns[[thisModel]],
                                    userDataGroup()[, input$groupCol]
                                ),
                                options = list(pageLength = 10)
                            )

                        output[[
                            paste0(thisModel, "ScoresDownload", c("Mg")[!isFALSE(groupName)])
                            ]] <<- downloadHandler(
                                filename = function() {
                                    input[[paste0(thisModel, "Filename", c("Mg")[!isFALSE(groupName)])]]
                                },
                                content = function(file) {
                                    write.table(
                                        shinyCTT:::getPredictedScores(
                                            fittedModelsWarns[[thisModel]],
                                            userDataGroup()[, input$groupCol]
                                        ),
                                        file,
                                        sep = input[[paste0(thisModel, "Sep")]],
                                        dec = input[[paste0(thisModel, "Dec")]],
                                        row.names = FALSE
                                    )
                                },
                                contentType = "text/csv"
                            )

                        # Model code ---------------------------------------------------------------------------------------
                        output[[
                            paste0(thisModel, "Code", c("Mg")[!isFALSE(groupName)])
                            ]] <<- renderPrint({
                                if (input$groupCol != "noGroupSelected") {
                                    if (length(unique(userDataGroup()[, input$groupCol])) <
                                        length(unique(userDataRaw()[, input$groupCol])))
                                        isSubset <- TRUE
                                    else
                                        isSubset <- FALSE
                                } else {
                                    isSubset <- FALSE
                                }

                                cat(
                                    shinyCTT:::makeRCode(
                                        input,
                                        modelCodes[[thisModel]],
                                        mvnTestResult$estimator,
                                        isSubset,
                                        thisModel,
                                        !isFALSE(groupName)
                                    )
                                )
                            })

                        # Create Tab ---------------------------------------------------------------------------------------
                        appendTab(
                            inputId = paste0("parTabsetTab", c("Mg")[!isFALSE(groupName)]),
                            tabPanel(
                                title = HTML(modelsLong[thisModel]),
                                HTML(parTableWithCIs)
                            ),
                            select = as.logical(whichModel == 1)
                        )

                        appendTab(
                            inputId = paste0("mcTabsetTab", c("Mg")[!isFALSE(groupName)]),
                            tabPanel(
                                title = HTML(modelsLong[thisModel]),
                                h5("The following R code can be used to fit this model with lavaan:"),
                                verbatimTextOutput(paste0(thisModel, "Code", c("Mg")[!isFALSE(groupName)]))
                            ),
                            select = as.logical(whichModel == 1)
                        )

                        appendTab(
                            inputId = paste0("fsTabsetTab", c("Mg")[!isFALSE(groupName)]),
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
                                                switch(
                                                    input$source,
                                                    "Workspace" = input$objectFromWorkspace,
                                                    "CSV" = gsub("\\.csv", "", input$CSVFile$name),
                                                    "SPSS" = gsub("\\.sav|\\.zsav|\\.por", "", input$SPSSFile$name)
                                                ),
                                                thisModel
                                            )
                                        ),
                                        hr(),
                                        radioButtons(
                                            paste0(thisModel, "Sep"),
                                            "Separator",
                                            choices = c(Comma = ",",
                                                        Semicolon = ";",
                                                        Tab = "\t"),
                                            selected = ","
                                        ),
                                        radioButtons(
                                            paste0(thisModel, "Dec"),
                                            "Decimal Separator",
                                            choices = c(Comma = ",",
                                                        Dot = "."),
                                            selected = "."
                                        ),
                                        hr(),
                                        div(
                                            align = "center",
                                            downloadButton(
                                                paste0(thisModel, "ScoresDownload", c("Mg")[!isFALSE(groupName)]),
                                                "Download Factor Scores"
                                            )
                                        ),
                                        width = 3
                                    ),
                                    mainPanel(
                                        h4("Data Overview"),
                                        DT::dataTableOutput(
                                            paste0(thisModel, "Scores", c("Mg")[!isFALSE(groupName)])
                                        )
                                    )
                                )
                            ),
                            select = as.logical(whichModel == 1)
                        )
                    })
                }

                if (length(goodModels) > 0) {
                    # Hierarchical model comparison plot -------------------------------------------------------------------
                    output[[paste0("hierPlot", groupName)]] <<- renderPlot({
                        modelNumbs <- which(models %in% goodModels)

                        chisqs <- dfs <- pvalues <- rep(NA, 5)
                        names(chisqs) <-
                            names(dfs) <-
                            names(pvalues) <- c("tkoete", "eteteq", "eteetp", "teqtpa", "etptpa")

                        if (!is.null(succTable$teq)) {
                            chisqs[paste0(rownames(succTable$teq)[1:(nrow(succTable$teq) - 1)],
                                          rownames(succTable$teq)[2:nrow(succTable$teq)])] <- succTable$teq[-1, 5]
                            dfs[paste0(rownames(succTable$teq)[1:(nrow(succTable$teq) - 1)],
                                       rownames(succTable$teq)[2:nrow(succTable$teq)])] <- succTable$teq[-1, 6]
                            pvalues[paste0(rownames(succTable$teq)[1:(nrow(succTable$teq) - 1)],
                                           rownames(succTable$teq)[2:nrow(succTable$teq)])] <- succTable$teq[-1, 7]
                        }

                        if (!is.null(succTable$etp)) {
                            chisqs[paste0(rownames(succTable$etp)[1:(nrow(succTable$etp) - 1)],
                                          rownames(succTable$etp)[2:nrow(succTable$etp)])] <- succTable$etp[-1, 5]
                            dfs[paste0(rownames(succTable$etp)[1:(nrow(succTable$etp) - 1)],
                                       rownames(succTable$etp)[2:nrow(succTable$etp)])] <- succTable$etp[-1, 6]
                            pvalues[paste0(rownames(succTable$etp)[1:(nrow(succTable$etp) - 1)],
                                           rownames(succTable$etp)[2:nrow(succTable$etp)])] <- succTable$etp[-1, 7]
                        }

                        ggplot2::ggplot(
                            data.frame(name = modelsExpr,
                                       x = c(0, 0, -2, 2, 0),
                                       y = c(6, 4, 2, 2, 0),
                                       xstarts = c(0, 0, 0, -2, 2),
                                       xends = c(0, -2, 2, 0, 0),
                                       ystarts = c(5.8, 3.8, 3.8, 1.8, 1.8),
                                       yends = c(4.2, 2.2, 2.2, 0.2, 0.2),
                                       labelxs = c(0, -2, 2, -2, 2),
                                       labelys = c(5, 3, 3, 1, 1),
                                       chisq = chisqs,
                                       df = dfs,
                                       pvalue = pvalues),
                            ggplot2::aes(x = x, y = y, label = name)
                        ) + ggplot2::geom_text(parse = TRUE, fontface = "bold", size = 5) +
                            ggplot2::geom_segment(
                                ggplot2::aes(x = xstarts, y = ystarts, xend = xends, yend = yends),
                                size = 0.3
                            ) +
                            ggplot2::geom_label(
                                ggplot2::aes(
                                    x = labelxs,
                                    y = labelys,
                                    label = ifelse(
                                        is.na(chisq),
                                        "No~Comparison",
                                        sprintf(
                                            "'%s-'*Delta*chi^2==%.3f*','~Delta*df==%i*','~p%s",
                                            mvnTestResult$estimator,
                                            chisq,
                                            df,
                                            ifelse(
                                                pvalue < 0.001,
                                                "<0.001",
                                                sprintf("==%.3f", pvalue)
                                            )
                                        )
                                    ),
                                    fill = c(pvalue < 0.05)
                                ),
                                color = textColor,
                                size = 4.5,
                                parse = TRUE
                            ) +
                            ggplot2::scale_fill_manual(values = c(goodColor, badColor), na.value = neutrColor) +
                            ggplot2::guides(fill = FALSE) +
                            ggplot2::xlim(c(-4, 4)) +
                            ggplot2::coord_fixed() +
                            ggplot2::theme_void()
                    })

                    # Hierarchical model comparison table ------------------------------------------------------------------
                    hierTables <- lapply(
                        c("teq", "etp"),
                        function(model) {
                            if (!is.null(succTable[[model]])) {
                                hierTable <- as.data.frame(succTable[[model]])
                                hierTable$CFI <- fits[rownames(hierTable), "cfi"]

                                AICdiff <- diff(hierTable$AIC)
                                BICdiff <- diff(hierTable$BIC)
                                CFIdiff <- diff(hierTable$CFI)

                                hierTable <- hierTable[, c(6, 5, 7, 2, 3, 8)]

                                hierTable[-1, "Chisq diff"] <- kableExtra::cell_spec(
                                    sprintf("+%.3f", hierTable[-1, "Chisq diff"]),
                                    color = textColor,
                                    background = ifelse(
                                        hierTable[-1, "Pr(>Chisq)"] < input$sigLvl,
                                        badColor,
                                        goodColor
                                    )
                                )
                                hierTable[-1, "Df diff"] <- kableExtra::cell_spec(
                                    sprintf("+%i", hierTable[-1, "Df diff"]),
                                    color = textColor,
                                    background = ifelse(
                                        hierTable[-1, "Pr(>Chisq)"] < input$sigLvl,
                                        badColor,
                                        goodColor
                                    )
                                )
                                hierTable[-1, "Pr(>Chisq)"] <- kableExtra::cell_spec(
                                    sprintf("%.3f", hierTable[-1, "Pr(>Chisq)"]),
                                    color = textColor,
                                    background = ifelse(
                                        hierTable[-1, "Pr(>Chisq)"] < input$sigLvl,
                                        badColor,
                                        goodColor
                                    )
                                )
                                hierTable[-1, "AIC"] <- kableExtra::cell_spec(
                                    sprintf("%.3f", hierTable[-1, "AIC"]),
                                    color = textColor,
                                    background = ifelse(
                                        AICdiff < 0,
                                        goodColor,
                                        badColor
                                    )
                                )
                                hierTable[-1, "BIC"] <- kableExtra::cell_spec(
                                    sprintf("%.3f", hierTable[-1, "BIC"]),
                                    color = textColor,
                                    background = ifelse(
                                        BICdiff < 0,
                                        goodColor,
                                        badColor
                                    )
                                )
                                hierTable[-1, "CFI"] <- kableExtra::cell_spec(
                                    sprintf("%.3f", hierTable[-1, "CFI"]),
                                    color = textColor,
                                    background = ifelse(
                                        CFIdiff > 0,
                                        goodColor,
                                        badColor
                                    )
                                )

                                hierTable$AIC[1] <- sprintf("%.3f", as.numeric(hierTable$AIC[1]))
                                hierTable$BIC[1] <- sprintf("%.3f", as.numeric(hierTable$BIC[1]))
                                hierTable$CFI[1] <- sprintf("%.3f", as.numeric(hierTable$CFI[1]))

                                names(hierTable) <- c("&Delta;df",
                                                      paste0(mvnTestResult$estimator, "-&Delta;&chi;&sup2;"),
                                                      "p",
                                                      "AIC",
                                                      "BIC",
                                                      paste0(mvnTestResult$estimator, "-CFI"))

                                rownames(hierTable) <- modelsAbbrev[rownames(hierTable)]

                                kableExtra::row_spec(
                                    kableExtra::column_spec(
                                        shinyCTT:::makeKable(hierTable),
                                        1,
                                        bold = TRUE
                                    ),
                                    1,
                                    background = "lightgrey"
                                )
                            } else {
                                NULL
                            }
                        }
                    )

                    # Table with all fit indices ---------------------------------------------------------------------------
                    fits$df <- kableExtra::cell_spec(
                        sprintf("%i", fits$df),
                        color = textColor,
                        background = ifelse(fits$pvalue < input$sigLvl,
                                            badColor,
                                            goodColor)
                    )
                    fits$chisq <- kableExtra::cell_spec(
                        sprintf("%.3f", fits$chisq),
                        color = textColor,
                        background = ifelse(fits$pvalue < input$sigLvl,
                                            badColor,
                                            goodColor)
                    )
                    fits$pvalue <- kableExtra::cell_spec(
                        sprintf("%.3f", fits$pvalue),
                        color = textColor,
                        background = ifelse(fits$pvalue < input$sigLvl,
                                            badColor,
                                            goodColor)
                    )
                    fits$rmsea <- kableExtra::cell_spec(
                        sprintf("%.3f", fits$rmsea),
                        color = textColor,
                        background = ifelse(fits$rmsea < 0.05,
                                            goodColor,
                                            badColor)
                    )
                    fits$rmsea.pvalue <- kableExtra::cell_spec(
                        sprintf("%.3f", fits$rmsea.pvalue),
                        color = textColor,
                        background = ifelse(fits$rmsea.pvalue < input$sigLvl,
                                            badColor,
                                            goodColor)
                    )
                    fits$rmsea.ci <- kableExtra::cell_spec(
                        fits$rmsea.ci,
                        color = textColor,
                        background = ifelse(as.numeric(
                            substr(fits$rmsea.ci, 9, 13)
                        ) < 0.05,
                        goodColor,
                        ifelse(as.numeric(
                            substr(fits$rmsea.ci, 2, 6)
                        ) < 0.05,
                        neutrColor,
                        badColor))
                    )
                    fits$cfi <- kableExtra::cell_spec(
                        sprintf("%.3f", fits$cfi),
                        color = textColor,
                        background = ifelse(fits$cfi < 0.95,
                                            badColor,
                                            goodColor)
                    )
                    fits$srmr <- kableExtra::cell_spec(
                        sprintf("%.3f", fits$srmr),
                        color = textColor,
                        background = ifelse(fits$srmr < 0.05,
                                            goodColor,
                                            badColor)
                    )

                    rownames(fits) <- modelsAbbrev[rownames(fits)]

                    # Chisq comparison table -------------------------------------------------------------------------------
                    combCompTable <- matrix(NA, nrow = 5, ncol = 10)

                    combCompTable[, seq(1, 10, 2)] <- matrix(compTable$df, ncol = 5, nrow = 5)
                    combCompTable[, seq(2, 10, 2)] <- matrix(compTable$chisq, ncol = 5, nrow = 5)

                    colnames(combCompTable) <- rep(
                        c(
                            "&Delta;df",
                            paste0(mvnTestResult$estimator, "-&Delta;&chi;&sup2;")
                        ),
                        5
                    )

                    headerNames <- c(1, rep(2, 5))
                    names(headerNames) <- c(" ", modelsAbbrev)

                    # AIC comparison table ---------------------------------------------------------------------------------
                    dim(infCompTable$aic) <- dim(infCompTable$bic) <- c(5, 5)

                    rownames(combCompTable) <-
                        rownames(infCompTable$aic) <-
                        rownames(infCompTable$bic) <-
                        colnames(infCompTable$aic) <-
                        colnames(infCompTable$bic) <-
                        modelsAbbrev

                    # Put them in a tab ------------------------------------------------------------------------------------
                    output[[
                        paste0("modelTestsCont", c("Mg")[!isFALSE(groupName)])
                        ]] <<- renderUI({
                            fluidPage(
                                fluidRow(
                                    shinydashboard::box(
                                        title = "Hierarchical model comparison plot:",
                                        width = 12,
                                        plotOutput(paste0("hierPlot", groupName))
                                    )
                                ),
                                fluidRow(
                                    shinydashboard::box(
                                        title = "Hierarchical model comparison table:",
                                        width = 12,
                                        HTML(
                                            paste0(
                                                "<table align = \"center\", width = \"100%\"><tr><td>",
                                                hierTables[[1]],
                                                "</td><td>&nbsp;</td><td>",
                                                hierTables[[2]],
                                                "</td></tr></table>"
                                            )
                                        )
                                    )
                                ),
                                fluidRow(
                                    shinydashboard::box(
                                        title = "Fit index table",
                                        width = 12,
                                        HTML(
                                            kableExtra::column_spec(
                                                kableExtra::column_spec(
                                                    shinyCTT:::makeKable(
                                                        fits[, -c(9, 10)],
                                                        col.names = c("df",
                                                                      paste0(mvnTestResult$estimator, "-&chi;&sup2;"),
                                                                      "p",
                                                                      "RMSEA",
                                                                      "p",
                                                                      "95%-CI",
                                                                      paste0(mvnTestResult$estimator, "-CFI"),
                                                                      "SRMR")
                                                    ),
                                                    1,
                                                    bold = TRUE
                                                ),
                                                c(4, 7),
                                                border_right = "1px solid lightgrey"
                                            )
                                        )
                                    )
                                ),
                                fluidRow(
                                    shinydashboard::box(
                                        title = HTML("&chi;&sup2;-Comparison Table:"),
                                        width = 12,
                                        HTML(
                                            kableExtra::add_header_above(
                                                kableExtra::column_spec(
                                                    shinyCTT:::makeKable(combCompTable),
                                                    1,
                                                    bold = TRUE
                                                ),
                                                headerNames,
                                                escape = FALSE
                                            )
                                        )
                                    )
                                ),
                                fluidRow(
                                    shinydashboard::box(
                                        title = "AIC/BIC-Comparison Table:",
                                        width = 12,
                                        HTML(
                                            paste0("<table align = \"center\", width = \"100%\">
                                <tr><td>
                                <table align = \"center\">
                                <tr><td><h5>AIC:</h5>",
                                                   kableExtra::column_spec(
                                                       shinyCTT:::makeKable(infCompTable$aic),
                                                       1,
                                                       bold = TRUE
                                                   ),
                                                   "</td></tr></table>
                                </td>
                                <td>&nbsp;</td>
                                <td><table align = \"center\">
                                <tr><td><h5>BIC:</h5>",
                                                   kableExtra::column_spec(
                                                       shinyCTT:::makeKable(infCompTable$bic),
                                                       1,
                                                       bold = TRUE
                                                   ),
                                                   "</td></tr></table>
                                </td></tr></table>")
                                        )
                                    )
                                )
                            )
                        })

                    #appendTab(
                    #    inputId = "compTabsets",
                    #    tabPanel(
                    #        ifelse(isFALSE(groupName),
                    #               "Singlegroup",
                    #               "Multigroup"),
                    #        wellPanel(
                    #            h5(sprintf(
                    #                "Lavaan status: %i warnings, %i errors.",
                    #                sum(warns),
                    #                sum(errs)
                    #            )),
                    #            lavErrsMsg,
                    #            lavWarnsMsg
                    #        ),
                    #
                    #    ),
                    #    select = isFALSE(groupName)
                    #)
                } else {
                    output[[
                        paste0("modelTestsCont", c("Mg")[!isFALSE(groupName)])
                        ]] <- renderUI({
                            tagList(
                                wellPanel(
                                    h5(sprintf(
                                        "Lavaan status: %i warnings, %i errors.",
                                        sum(warns),
                                        sum(errs)
                                    )),
                                    lavErrsMsg,
                                    lavWarnsMsg
                                )
                            )
                        })
                    #appendTab(
                    #    inputId = "compTabsets",
                    #    tabPanel(
                    #        ifelse(isFALSE(groupName),
                    #               "No Singlegroup Models have been fitted",
                    #               "No Multigroup Models have been fitted"),
                    #        wellPanel(
                    #            h5(sprintf(
                    #                "Lavaan status: %i warnings, %i errors.",
                    #                sum(warns),
                    #                sum(errs)
                    #            )),
                    #            lavErrsMsg,
                    #            lavWarnsMsg
                    #        )
                    #    ),
                    #    select = isFALSE(groupName)
                    #)
                    #removeTab(inputId = "navbar",
                    #          target = "panelParTables")
                }
            }
        )
    })
}
