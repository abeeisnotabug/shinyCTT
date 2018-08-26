# Define server logic required to draw a histogram
function(input, output) {

    output$objectsInWorkspace <- renderUI({
        selectInput("objectFromWorkspace",
                    "Choose data object from Workspace:",
                    Filter(function(object) !is.null(dim(get(object))), ls(envir = globalenv())))
    })

    userData <- reactive({
        if (input$source == "CSV") {
            req(input$CSVFile)

            read.csv(input$CSVFile$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
        } else if (input$source == "Workspace") {
            req(input$objectFromWorkspace)

            get(input$objectFromWorkspace)
        }
    })

    itemCols <- reactive({
        eval(parse(text = input$itemColsString))
    })

    numericCols <- reactive({
        colnames(userData())[sapply(userData(), is.numeric)]
    })

    output$dataOverview <- renderTable({

        # Limit max number of columns to display --------
        #nColsToDisplay <- min(ncol(userData), 9)
        #return(head(userData[, 1:nColsToDisplay]))

        userDataToDisplay <- userData()

        if (is.null(colnames(userData()))) {
            colnames(userDataToDisplay) <- sprintf("(%i)", 1:ncol(userData()))
        } else {
            colnames(userDataToDisplay) <- sprintf("%s (%s)", colnames(userData()), 1:ncol(userData()))
        }

        return(head(userDataToDisplay))
    })

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
