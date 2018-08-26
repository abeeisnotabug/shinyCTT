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
                           selected = possibleItemColumns())
    })

    output$groupColChooser <- renderUI({
        selectInput("groupCol",
                    "Select which column contains the group",
                    choices = c("Data does not contain group" = "random_group",
                                colnames(userData())))
    })

    observeEvent(input$goDescrStats, {
        appendTab(inputId = "navbar",
            tabPanel("Descriptive Statistics",
                     value = "panelDescrStats",
                sidebarLayout(
                     sidebarPanel(
                     ),
                     mainPanel(
                         h3("Mean, standard deviation, skewness, excess:"),
                         #uiOutput("descrWarning"),
                         tableOutput("descrTable"),
                         h3("Correlation Plot:"),
                         plotOutput("corrPlot")
                     )
                )
            ),
            select = TRUE
        )

        removeTab(inputId = "navbar",
                  target = "panelDataInput")

        output$descrTable <- renderTable({
            isolate({
                t(apply(userData()[, input$itemCols], 2, function(col) c(Mean = mean(col),
                                                                     Sd = sd(col),
                                                                     Skew = moments::skewness(col),
                                                                     Excess = moments::kurtosis(col) - 3)))
            })
        }, rownames = TRUE)

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

        output$corrPlot <- renderPlot({corrplot::corrplot.mixed(cor(userData()[, input$itemCols]))})
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
