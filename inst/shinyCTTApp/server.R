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
                               conditionalPanel(condition = "input.goMVN == 0",
                                                actionButton("goMVN",
                                                             "Test for multivariate normality!")),
                               conditionalPanel(condition = "input.goMVN > 0",
                                                helpText("Test on MVN has been performed."))
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
                             conditionalPanel(condition = "input.goCorrInd == 0",
                                              radioButtons("estimator",
                                                           "Choose estimator based on test result:",
                                                           choices = c("Maximum Likelihood" = "ML",
                                                                       "Robust Maximum Likelihood" = "MLR"),
                                                           selected = "ML")),
                             conditionalPanel(condition = "input.goCorrInd > 0",
                                              #helpText("The following estimator has been chosen:"),
                                              textOutput("selectedEstimator")),
                             hr(),
                             conditionalPanel(condition = "input.goCorrInd == 0",
                                              actionButton("goCorrInd",
                                                           HTML("Calculate the models and</br>test for correlative independence!")),
                                              div(align = "center",
                                                  helpText("This may take a few seconds."))),
                             conditionalPanel(condition = "input.goCorrInd > 0",
                                              helpText("The models have been calculated and the test
                                                            for correlative independence has been performed."))
                         ),
                         mainPanel(
                             h3("Tests on Univariate Normality:"),
                             div(align = "center",
                                 tableOutput("mvnTableUV")),
                             h3("Test on Multivariate Normality:"),
                             div(align = "center",
                                 tableOutput("mvnTableMV")),
                             textOutput("mvnComment"),
                             verbatimTextOutput("buttonValue")
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
                                 conditionalPanel(condition = "input.goModels == 0",
                                                  actionButton("goModels",
                                                               "Test the models!"),
                                                  div(align = "center",
                                                      helpText("This may take a few seconds."))),
                                 conditionalPanel(condition = "input.goModels > 0",
                                                  helpText("The models have been printed."))
                             ),
                             mainPanel(
                                 withMathJax(),
                                 h3("Correlation Table with Confidence Intervals:"),
                                 tableOutput("corTableWithCIs"),
                                 h3("Test on Correlative Independence:"),
                                 uiOutput("corrIndText")
                             )
                         )
)

modelTestPanel <- tabPanel("Models",
                           value = "panelModelTests",
                           tabsetPanel(
                               tabPanel(HTML("&tau;-kongeneric"),
                                        h3("Test on Model Fit:"),
                                        uiOutput("tkModelFitText"),
                                        h3("Estimated paramters with Standard Errors and Confidence Intervals:"),
                                        uiOutput("tkTable")),
                               tabPanel(HTML("essentially &tau;-equivalent")),
                               tabPanel(HTML("&tau;-equivalent")),
                               tabPanel(HTML("essentially &tau;-parallel")),
                               tabPanel(HTML("&tau;-parallel"))
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

                output$selectedEstimator <- renderText({sprintf("The %s estimator has been chosen.", input$estimator)})
            })

            appendTab(inputId = "navbar",
                      mvnPanel,
                      select = TRUE)
        }
    })

    # Initialise variables to use them later
    fitted_models <- NULL
    fit_params <- NULL

    observeEvent(input$goCorrInd, {

        model_codes <- make_model_codes(input_data = userData()[, input$itemCols],
                                        multi_group = FALSE)

        fitted_models <<- reactive({lapply(model_codes,
                                          FUN = cfa,
                                          data = userData(),
                                          meanstructure = T,
                                          estimator = input$estimator)})

        fit_params <<- reactive({lapply(fitted_models(),
                                       lavInspect,
                                       what = "fit")})

        corr_ind <- extract_fit_params(fit_params()[["tau-kongeneric"]],
                                       estimator = input$estimator,
                                       what = "corr_ind")

        observe({
            req(input$corrIndSigLvl)

            output$corrIndText <- renderUI({
                if (corr_ind[3] < as.numeric(input$corrIndSigLvl)) {#HTML(
                    withMathJax(tags$p(sprintf("The hypothesis that all correlations between the items are equal to zero
                        has to be discarded on a significance level of %s. Test result: %s",
                            #"0.05",
                            input$corrIndSigLvl,
                            test_result_output(corr_ind[1], corr_ind[2], corr_ind[3], input$estimator))))
                } else {
                    withMathJax(tags$p(sprintf("The hypothesis that all correlations between the items are equal to zero
                        can be maintained on a significance level of %s. Test result: %s

                        It is thus not advised to conduct any further analysis.",
                            #"0.05",
                            input$corrIndSigLvl,
                            test_result_output(corr_ind[1], corr_ind[2], corr_ind[3], input$estimator))))
                }
            })

            #output$corrIndTextUI <- renderUI({
            #    ))
            #})

            output$corTableWithCIs <- renderTable({create_corr_table_with_cis(userData()[, input$itemCols],
                                                                              alpha = as.numeric(input$corrIndSigLvl))},
                                                  rownames = TRUE)
        })

        appendTab(inputId = "navbar",
                  corrIndPanel,
                  select = TRUE)
    })

    observeEvent(input$goModels, {

        # Tau-kongeneric ---------------------------------------------------------------------------------
        #output$tkTable <- renderTable({
        #    extract_parameters(fitted_models()[["tau-kongeneric"]])
        #}, digits = 3)

        output$tkTable <- renderUI({
            M <- print(xtable::xtable(extract_parameters(fitted_models()[["tau-kongeneric"]]), digits = 3),
                                      floating = FALSE,
                       tabular.environment = "array",
                       comment = FALSE,
                       print.results = FALSE,
                       sanitize.text.function = identity)

            withMathJax(HTML(M))
        })

        tk_fit <- extract_fit_params(fit_params()[["tau-kongeneric"]],
                                     what = "model_fit",
                                     estimator = input$estimator)

        observe({
            output$tkModelFitText <- renderUI({
                if (tk_fit[3] < as.numeric(input$corrIndSigLvl)) {
                    withMathJax(HTML(sprintf("The hypothesis that the model implied covariance matrix and the meanstructure
                            match the empirical ones has to be discarded on a significance level of %s. Test result: %s",
                            input$corrIndSigLvl,
                            test_result_output(tk_fit[1], tk_fit[2], tk_fit[3], input$estimator))))
                } else {
                    withMathJax(HTML(sprintf("The hypothesis that the model implied covariance matrix and the meanstructure
                            match the empirical ones can be maintained on a significance level of %s. Test result: %s",
                            input$corrIndSigLvl,
                            test_result_output(tk_fit[1], tk_fit[2], tk_fit[3], input$estimator))))
                }
            })
        })

        appendTab(inputId = "navbar",
                  modelTestPanel,
                  select = TRUE)
    })
}
