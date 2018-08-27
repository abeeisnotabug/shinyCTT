# Define server logic required to draw a histogram
function(input, output, session) {

    # Data input -------------------------------------------------------------------------------

    output$objectsInWorkspace <- renderUI({
        selectInput("objectFromWorkspace",
                    "Choose data object from Workspace:",
                    Filter(function(object) !is.null(dim(get(object))), ls(envir = globalenv())))
    })

    userData <- reactive({
        if (input$source == "CSV") {
            req(input$CSVFile)

            userDataTmp <- read.csv(input$CSVFile$datapath,
                                    header = input$header,
                                    sep = input$sep,
                                    quote = input$quote)
        } else if (input$source == "Workspace") {
            req(input$objectFromWorkspace)

            userDataTmp <- get(input$objectFromWorkspace)
        }

        if (is.null(colnames(userDataTmp))) colnames(userDataTmp) <- paste("var", 1:ncol(userDataTmp), sep = "_")

        userDataTmp
    })

    possibleItemColumns <- reactive({
        if (mode(userData()) == "list") {
            colnames(userData())[sapply(userData(), is.numeric)]
        } else {
            colnames(userData())
        }
    })

    output$dataOverview <- renderDataTable({as.data.frame(userData())})

    output$itemColsChooser <- renderUI({
        checkboxGroupInput("itemCols",
                           "Select which columns contain items",
                           choices = possibleItemColumns(),
                           selected = possibleItemColumns(),
                           inline = TRUE)
    })

    output$groupColChooser <- renderUI({
        selectInput("groupCol",
                    "Select which column contains the group",
                    choices = c("Data does not contain group" = "random_group",
                                colnames(userData())))
    })

    observeEvent(input$goDescrStats, {

        observe({
            if ("showDescrTable" %in% input$toggleStats) {
                output$descrTable <- renderTable({
                    isolate({
                        t(apply(userData()[, input$itemCols], 2, function(col) c(Mean = mean(col),
                                                                                 Sd = sd(col),
                                                                                 Skew = moments::skewness(col),
                                                                                 Excess = moments::kurtosis(col) - 3)))
                    })
                }, digits = 3, rownames = TRUE)

                output$descrTableUI <- renderUI({
                    tagList(
                        h3("Mean, Standard Deviation, Skewness, Excess:"),
                        div(align = "center",
                            tableOutput("descrTable"))
                    )
                })
            } else {
                output$descrTableUI <- NULL
            }

            if ("showCorrPlot" %in% input$toggleStats) {
                output$corrPlot <- renderPlot({corrplot::corrplot.mixed(cor(userData()[, input$itemCols]),
                                                                        number.digits = 3)
                    })

                output$corrPlotUI <- renderUI({
                    tagList(
                        h3("Correlation table and plot:"),
                        plotOutput("corrPlot")
                    )
                })
            } else {
                output$corrPlotUI <- NULL
            }

            if ("showCovMat" %in% input$toggleStats) {
                output$covMat <- renderTable({
                    cov(userData()[, input$itemCols])
                }, digits = 3, rownames = TRUE)

                output$covMatUI <- renderUI({
                    tagList(
                        h3("Covariance Matrix:"),
                        div(align = "center",
                            tableOutput("covMat")
                        )
                    )
                })
            } else {
                output$covMatUI <- NULL
            }
        })

        appendTab(inputId = "navbar",
            tabPanel("Descriptive Statistics",
                     value = "panelDescrStats",
                sidebarLayout(
                     sidebarPanel(
                         checkboxGroupInput("toggleStats",
                                            "Show statistics:",
                                            choices = c("Mean, Standard Deviation, Skewness, Excess" = "showDescrTable",
                                                        "Correlation table and plot" = "showCorrPlot",
                                                        "Covariance Matrix" = "showCovMat"),
                                            selected = c("showDescrTable",
                                                         "showCorrPlot",
                                                         "showCovMat"))
                     ),
                     mainPanel(
                         uiOutput("descrTableUI"),
                         uiOutput("corrPlotUI"),
                         uiOutput("covMatUI")
                     )
                )
            ),
            select = TRUE
        )

        removeTab(inputId = "navbar",
                  target = "panelDataInput")

        # Warning message to be implemented later!! ------------------

        #output$descrWarning <-   renderUI({
        #    val <- correlation()
        #    if(is.null(val)) {
        #        tags$i("Waiting for data input...")
        #    } else {
        #        isNA <- is.na(val)
        #        if(sum(isNA)) {
        #            tags$div(
        #                tags$h4("Warning: The following pairs in calculated correlation have been converted to zero because they produced NAs!"),
        #                helpText("Consider using an approriate NA Action to exclude missing data"),
        #                renderTable(expand.grid(attr(val, "dimnames"))[isNA,]))
        #        }
        #    }
        #})
    })

    # Descr. statistics -------------------------------------------------------------------------------

    observeEvent(input$descrStats, {
        output$caption1 <- renderText({"Mean, standard deviation, skewness, excess, covariance matrix:"})
        output$descrTable <- renderTable({
            isolate({
                t(apply(userData()[, itemCols()], 2, function(col) c(Mean = mean(col),
                                                                     Sd = sd(col),
                                                                     Skew = moments::skewness(col),
                                                                     Excess = moments::kurtosis(col) - 3)))
            })
        },
        rownames = TRUE)
    })
}
