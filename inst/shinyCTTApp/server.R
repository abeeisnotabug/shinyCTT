# Define server logic required to draw a histogram
descrPanel <- tabPanel("Descriptive Statistics",
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
                                                               "showCovMat")),
                               hr(),
                               actionButton("goMVN",
                                            "Test for multivariate normality!")
                           ),
                           mainPanel(
                               uiOutput("descrTableUI"),
                               uiOutput("corrPlotUI"),
                               uiOutput("covMatUI")
                           )
                       )
)

mvnPanel <- tabPanel("MVN",
                     value = "panelMVN",
                     sidebarLayout(
                         sidebarPanel(
                             radioButtons("estimator",
                                          "Choose estimator based on test result:",
                                          choices = c("Maximum Likelihood" = "ML",
                                                      "Robust Maximum Likelihood" = "MLR"),
                                          selected = "ML"),
                             actionButton("goCorrInd",
                                          HTML("Calculate the models and</br>test for correlative independence!"))
                         ),
                         mainPanel(
                             h3("Tests on Univariate Normality:"),
                             div(align = "center",
                                 tableOutput("mvnTableUV")),
                             h3("Test on Multivariate Normality:"),
                             div(align = "center",
                                 tableOutput("mvnTableMV")),
                             textOutput("mvnComment")
                         )
                     )
)

corrIndPanel <- tabPanel("Corr. Ind.",
                         value = "panelCorrInd",
                         sidebarLayout(
                             sidebarPanel(
                                 textInput("corrIndSigLvl",
                                           "Enter the desired significance level",
                                           value = "0.05"),
                                 actionButton("goTauModel",
                                              "Test the tau-kongeneric model!")
                             ),
                             mainPanel(
                                 h3("Test on Correlative Independence:"),
                                 htmlOutput("tkModelSum")
                             )
                         )
)

function(input, output, session) {

    # Data input -------------------------------------------------------------------------------
    output$objectsInWorkspace <- renderUI({
        selectInput("objectFromWorkspace",
                    "Choose data object from Workspace:",
                    Filter(function(object) !is.null(dim(get(object))), ls(envir = globalenv())))
    })

    output$workspace <- renderPrint({ls(all.names = TRUE)})

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

    # Action to be performed upon pressing the descr. stats button ----------------------------------------------------
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
                output$corrPlot <- renderPlot({
                    corrplot::corrplot.mixed(cor(userData()[, input$itemCols]),
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
                            tableOutput("covMat"))
                    )
                })
            } else {
                output$covMatUI <- NULL
            }
        })

        appendTab(inputId = "navbar",
                  descrPanel,
                  select = TRUE)

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

    # Action to be performed upon pressing the MVN button! ------------------------------------------------------------
    mvnTestResult <- reactive({
        rawResult <- MVN::mvn(userData()[, input$itemCols])

        names(rawResult$univariateNormality)[5] <- "Signif."
        rawResult$univariateNormality$Signif.[grep("NO", rawResult$univariateNormality$Signif.)] <- "*"
        rawResult$univariateNormality$Signif.[grep("YES", rawResult$univariateNormality$Signif.)] <- ""
        rawResult$univariateNormality$Statistic <- as.numeric(rawResult$univariateNormality$Statistic)
        rawResult$univariateNormality$`p value` <- as.numeric(rawResult$univariateNormality$`p value`)

        rawResult$multivariateNormality <- rawResult$multivariateNormality[-3, ]
        names(rawResult$multivariateNormality)[4] <- "Signif."
        rawResult$multivariateNormality$Signif. <- as.character(rawResult$multivariateNormality$Signif.)
        rawResult$multivariateNormality$Signif.[grep("NO", rawResult$multivariateNormality$Signif.)] <- "*"
        rawResult$multivariateNormality$Signif.[grep("YES", rawResult$multivariateNormality$Signif.)] <- ""
        rawResult$multivariateNormality$Statistic <- as.numeric(levels(rawResult$multivariateNormality$Statistic))
        rawResult$multivariateNormality$`p value` <- as.numeric(levels(rawResult$multivariateNormality$`p value`))

        rawResult
    })

    observeEvent(input$goMVN, {
        # Check if tab already exists, if not, create it
        if (F) {

        } else {
            output$mvnTableUV <- renderTable({mvnTestResult()$univariateNormality}, digits = 3)
            output$mvnTableMV <- renderTable({mvnTestResult()$multivariateNormality}, digits = 3)

            observe({
                if ("*" %in% mvnTestResult()$multivariateNormality$Signif.) {
                    output$mvnComment <- renderText({"At least one of the hypotheses that Mardia's Skewness statistic
                                             or Mardias' Kurtosis statistic matches one of a
                                             normal distribution has to be discarded on a significance
                                             level of 0.05.

                                             It is thus recommended to continue with the Robust Maximum Likelihood (MLR) estimator."})
                    selectedEstimator <- "MLR"
                } else {
                    output$mvnComment <- renderText({"The hypotheses that Mardia's Skewness statistic
                                             and Mardias' Kurtosis statistic match those of a
                                             normal distribution can be maintained on a significance
                                             level of 0.05.

                                             It is thus recommended to continue with the Maximum Likelihood (ML) estimator."})

                    selectedEstimator <- "ML"
                }

                updateRadioButtons(session,
                                   "estimator",
                                   selected = selectedEstimator)
            })

            appendTab(inputId = "navbar",
                      mvnPanel,
                      select = TRUE)
        }
    })

    observeEvent(input$goCorrInd, {

        model_codes <- make_model_codes(data = userData(),
                                        item_cols = input$itemCols,
                                        multi_group = FALSE)

        fitted_models <- reactive({lapply(model_codes,
                                          FUN = cfa,
                                          data = rtdata,
                                          meanstructure = T,
                                          estimator = input$estimator)})

        fit_params <- reactive({lapply(fitted_models(),
                                       lavInspect,
                                       what = "fit")})

        corr_ind <- extract_corr_ind(fit_params()[["tau-kongeneric"]],
                                     estimator = input$estimator)

        observe({
            req(input$corrIndSigLvl)
            if (corr_ind[3] < as.numeric(input$corrIndSigLvl)) {
                output$tkModelSum <- renderText(HTML(
                    sprintf("The hypothesis that all correlations between the items are equal to zero
                        has to be discarded on a significance level of %s (%s).",
                            input$corrIndSigLvl,
                            test_result_output(corr_ind[1], corr_ind[2], corr_ind[3]))
                ))
            } else {
                output$tkModelSum <- renderText(HTML(
                    sprintf("The hypothesis that all correlations between the items are equal to zero
                        can be maintained on a significance level of %s, (%s). It is thus not advised to
                        conduct any further analysis.",
                            input$corrIndSigLvl,
                            test_result_output(corr_ind[1], corr_ind[2], corr_ind[3]))
                ))
            }
        })

        appendTab(inputId = "navbar",
                  corrIndPanel,
                  select = TRUE)
    })
}
