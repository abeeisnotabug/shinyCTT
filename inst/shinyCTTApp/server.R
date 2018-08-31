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
                output$descrTable <- reactive({
                        tmpTbl <- t(apply(userData()[, input$itemCols], 2, function(col) c(Mean = mean(col),
                                                                                           Sd = sd(col),
                                                                                           Skew = moments::skewness(col),
                                                                                           Excess = moments::kurtosis(col) - 3)))

                        kableExtra::kable_styling(
                            kableExtra::kable(tmpTbl,
                                              digits = 3),
                            full_width = F,
                            position = "center")
                })

                output$descrTableUI <- renderUI({
                    tagList(
                        h3("Mean, Standard Deviation, Skewness, Excess:"),
                        tableOutput("descrTable"))
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
                output$covMat <- reactive({
                    do.call(kableExtra::kable_styling,
                            append(kableStylingOptions,
                                   list(kableExtra::kable(cov(userData()[, input$itemCols]),
                                                          digits = 3))))

                    #kableExtra::kable_styling(
                    #    kableExtra::kable(cov(userData()[, input$itemCols]),
                    #                      digits = 3),
                    #    full_width = F,
                    #    position = "center")
                })

                output$covMatUI <- renderUI({
                    tagList(
                        h3("Covariance Matrix:"),
                        tableOutput("covMat")
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
        rawResult$univariateNormality$`p value` <- suppressWarnings(as.numeric(rawResult$univariateNormality$`p value`))

        rawResult$multivariateNormality <- rawResult$multivariateNormality[-3, ]
        names(rawResult$multivariateNormality)[4] <- "Signif."
        rawResult$multivariateNormality$Signif. <- as.character(rawResult$multivariateNormality$Signif.)
        rawResult$multivariateNormality$Signif.[grep("NO", rawResult$multivariateNormality$Signif.)] <- "*"
        rawResult$multivariateNormality$Signif.[grep("YES", rawResult$multivariateNormality$Signif.)] <- ""
        rawResult$multivariateNormality$Statistic <- as.numeric(levels(rawResult$multivariateNormality$Statistic))
        rawResult$multivariateNormality$`p value` <- suppressWarnings(as.numeric(levels(rawResult$multivariateNormality$`p value`)))

        rawResult
    })

    observeEvent(input$goMVN, {
        output$mvnTableUV <- renderTable({mvnTestResult()$univariateNormality}, digits = 3)

        observe({
            if ("*" %in% mvnTestResult()$multivariateNormality$Signif.) {
                output$mvnComment <- renderUI({
                    tagList(
                        div("At least one of the hypotheses that Mardia's Skewness statistic
                                 or Mardias' Kurtosis statistic matches one of a
                                 normal distribution has to be discarded on a significance
                                 level of 0.05. Test result:"),
                        div(align = "center",
                            renderTable({mvnTestResult()$multivariateNormality}, digits = 3)),
                        div("It is thus recommended to continue with the Robust Maximum Likelihood (MLR) estimator.")
                    )
                })

                selectedEstimator <- "MLR"
            } else {
                output$mvnComment <- renderUI({
                    tagList(
                        div("The hypotheses that Mardia's Skewness statistic
                                 and Mardias' Kurtosis statistic match those of a
                                 normal distribution can be maintained on a significance
                                level of 0.05."),
                        div(align = "center",
                            renderTable({mvnTestResult()$multivariateNormality}, digits = 3)),
                        br("It is thus recommended to continue with the Maximum Likelihood (ML) estimator.")
                    )
                })

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
    })

    observeEvent(input$goCorrInd, {

        dummyModel <- paste(sprintf("%s ~~ %s",
                                    colnames(userData()[, input$itemCols])[1],
                                    colnames(userData()[, input$itemCols])[-1]),
                            collapse = "\n")

        fittedCorrIndModel <- cfa(model = dummyModel,
                                  data = userData(),
                                  estimator = input$estimator)

        corr_ind <- extract_fit_params(lavInspect(fittedCorrIndModel, what = "fit"),
                                       estimator = input$estimator,
                                       what = "corr_ind")

        observe({
            req(input$corrIndSigLvl)

            output$corrIndText <- renderUI({
                if (corr_ind[3] < as.numeric(input$corrIndSigLvl)) {
                    tagList(
                        div(sprintf("The hypothesis that all correlations between the items are equal to zero
                        has to be discarded on a significance level of %s. Test result:",
                                                         input$corrIndSigLvl)),
                        withMathJax(test_result_output(corr_ind[1], corr_ind[2], corr_ind[3], input$estimator))
                    )
                } else {
                    tagList(
                        div(style = "color:red", sprintf("The hypothesis that all correlations between the items are equal to zero
                        can be maintained on a significance level of %s. Test result:",
                                                         input$corrIndSigLvl)),
                        withMathJax(test_result_output(corr_ind[1], corr_ind[2], corr_ind[3], input$estimator)),
                        div(style = "color:red", "It is thus not advised to conduct any further analysis.")
                    )
                }
            })

            output$corTableWithCIs <- renderTable({create_corr_table_with_cis(userData()[, input$itemCols],
                                                                              alpha = as.numeric(input$corrIndSigLvl))},
                                                  rownames = TRUE)
        })

        appendTab(inputId = "navbar",
                  corrIndPanel,
                  select = TRUE)
    })

    observeEvent(input$goModels, {

        model_codes <- make_model_codes(input_data = userData()[, input$itemCols],
                                        multi_group = FALSE)

        fitted_models <- reactive({lapply(model_codes,
                                          FUN = cfa,
                                          data = userData(),
                                          meanstructure = T,
                                          estimator = input$estimator)})

        fit_params <- reactive({lapply(fitted_models(),
                                        lavInspect,
                                        what = "fit")})

        # Tau-kongeneric ---------------------------------------------------------------------------------
        output$tkTable <- renderUI({
            tkTableRaw <- print(xtable::xtable(extract_parameters(fitted_models()[["tau-kongeneric"]]), digits = 3),
                                floating = FALSE,
                                tabular.environment = "array",
                                comment = FALSE,
                                print.results = FALSE,
                                sanitize.text.function = identity)

            withMathJax(HTML(tkTableRaw))
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
