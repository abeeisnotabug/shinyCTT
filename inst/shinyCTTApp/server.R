function(input, output, session) {

    # Data input -----------------------------------------------------------------------------------------------------------
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

        as.data.frame(userDataTmp)
    })

    output$dataOverview <- renderDataTable({userData()})

    output$itemColsChooser <- renderUI({
        possibleItemColumns <- {
            if (mode(userData()) == "list") {
                colnames(userData())[sapply(userData(), is.numeric)]
            } else {
                colnames(userData())
            }
        }

        checkboxGroupInput("itemCols",
                           "Select which columns contain items",
                           choices = possibleItemColumns,
                           selected = possibleItemColumns,
                           inline = TRUE)
    })

    output$groupColChooser <- renderUI({
        selectInput("groupCol",
                    "Select which column contains the group",
                    choices = c("Data does not contain group" = "no",
                                colnames(userData())[!(colnames(userData()) %in% input$itemCols)]))
    })

    observeEvent(input$sigLvl, {
        if ((input$sigLvl < 0 | input$sigLvl > 1) & !is.na(input$sigLvl)) {
            updateNumericInput(session,
                               "sigLvl",
                               value = 0.05)
        }
    })

    # Descriptives ---------------------------------------------------------------------------------------------------------
    output$descrTable <- renderText({
        if (all(any(input$itemCols %in% colnames(userData())),
                length(input$itemCols) <= ncol(userData()),
                length(input$itemCols) > 1)) {

            table <- t(apply(userData()[, input$itemCols], 2, function(col) c(Mean = mean(col),
                                                                              Sd = sd(col),
                                                                              Skew = moments::skewness(col),
                                                                              Excess = moments::kurtosis(col) - 3)))

            makeKable(table)
        }
    })

    output$covMat <- renderText({
        if (all(any(input$itemCols %in% colnames(userData())),
                length(input$itemCols) <= ncol(userData()),
                length(input$itemCols) > 1)) {

            table <- cov(userData()[, input$itemCols])
            table[upper.tri(table)] <- NA

            kableExtra::column_spec(
                makeKable(table),
                1, bold = TRUE
            )
        }
    })

    corrTableWithCIsRaw <- reactive({
        req(input$sigLvl)

        if (all(any(input$itemCols %in% colnames(userData())),
                length(input$itemCols) <= ncol(userData()),
                length(input$itemCols) > 1,
                input$sigLvl > 0 & input$sigLvl < 1)) {

            corrTableRaw <- cor(userData()[, input$itemCols])

            CIs <- corrplot::cor.mtest(userData()[, input$itemCols],
                                       conf.level = (1 - input$sigLvl))

            corrTable <- matrix("", nrow = length(input$itemCols), ncol = length(input$itemCols))

            for (i in 2:length(input$itemCols)) {
                for (j in 1:(i-1)) {
                    corrTable[i, j] <- sprintf(
                        if (CIs$p[i, j] <= input$sigLvl & corrTableRaw[i, j] >= 0) {
                            "%.3f<hr>[%.3f; %.3f]"
                        } else if (CIs$p[i, j] <= input$sigLvl & corrTableRaw[i, j] < 0) {
                            "<span style=\"color:red;\">%.3f<hr>[%.3f; %.3f]</span>"
                        } else if (CIs$p[i, j] >= input$sigLvl & corrTableRaw[i, j] >= 0) {
                            "<span style=\"color:lightgrey;\">%.3f<hr>[%.3f; %.3f]</span>"
                        } else if (CIs$p[i, j] >= input$sigLvl & corrTableRaw[i, j] < 0) {
                            "<span style=\"color:lightcoral;\">%.3f<hr>[%.3f; %.3f]</span>"
                        },
                        corrTableRaw[i, j],
                        CIs$lowCI[i, j],
                        CIs$uppCI[i, j]
                    )
                }
            }

            diag(corrTable) <- sprintf("<span style=\"font-weight:bold;\">%s<hr>CI</span>", input$itemCols)

            corrTable
        }
    })

    output$corrTableWithCIs <- renderUI({
        if (length(input$itemCols) > 1) {

            tagList(
                HTML(makeKable(corrTableWithCIsRaw())),
                h5("Legend:"),
                HTML(makeKable(matrix(c("Sign. pos. cor.",
                                        "<span style=\"color:red;\">Sign. neg. cor.</span>",
                                        "<span style=\"color:lightgrey;\">Insign. pos. cor.</span>",
                                        "<span style=\"color:lightcoral;\">Insign. neg. cor.</span>"),
                                      ncol = 2),
                               bootstrap_options = "bordered",
                               position = "left"))
            )
        }
    })

    corrCheck <- reactive({
        list(neg = !any(grep("red|coral", corrTableWithCIsRaw())),
             insig = !any(grep("light", corrTableWithCIsRaw())))
    })

    # Calculate test on MVN ------------------------------------------------------------------------------------------------
    mvnTestResult <- reactive({
        MVN::mvn(userData()[, input$itemCols])
    })

    output$mvnTableUV <- renderText({
        req(input$sigLvl)

        if (all(any(input$itemCols %in% colnames(userData())),
                length(input$itemCols) <= ncol(userData()),
                length(input$itemCols) > 1)) {

            mvnUV <- data.frame(Test = as.character(mvnTestResult()$univariateNormality$Test),
                                Item = as.character(mvnTestResult()$univariateNormality$Variable),
                                Statistic = as.numeric(mvnTestResult()$univariateNormality$Statistic),
                                p = suppressWarnings(as.numeric(mvnTestResult()$univariateNormality$`p value`)),
                                stringsAsFactors = F)

            mvnUV$p[is.na(mvnUV$p)] <- 0
            mvnUV$Signif. <- ifelse(mvnUV$p < input$sigLvl, "*", "")
            mvnUV$p <- sapply(mvnUV$p,
                              function(value) if (value < 0.001) "< 0.001" else sprintf("%.3f", round(value, 3)))

            makeKable(mvnUV, bootstrap_options = "basic")
        }
    })

    mvnTestResultMV <- reactive({
        req(input$sigLvl)

        if (all(any(input$itemCols %in% colnames(userData())),
                length(input$itemCols) <= ncol(userData()),
                length(input$itemCols) > 1)) {

            mvnMV <- data.frame(Test = as.character(mvnTestResult()$multivariateNormality$Test),
                                Statistic = as.numeric(as.character(mvnTestResult()$multivariateNormality$Statistic)),
                                p = as.numeric(as.character(mvnTestResult()$multivariateNormality$`p value`)),
                                Signif. = as.character(mvnTestResult()$multivariateNormality$Result),
                                stringsAsFactors = F)[-3,]

            mvnMV$Signif. <- ifelse(mvnMV$p < input$sigLvl, "*", "")
            mvnMV$p <- sapply(mvnMV$p,
                              function(value) if (value < 0.001) "< 0.001" else sprintf("%.3f", round(value, 3)))

            mvnMV
        }
    })

    observe({
        updateRadioButtons(session,
                           "estimator",
                           selected = if ("*" %in% mvnTestResultMV()$Signif.) "MLR" else "ML")
    })

    output$mvnComment <- renderUI({
        req(input$sigLvl)

        if ("*" %in% mvnTestResultMV()$Signif.) {
            tagList(
                sprintf("At least one of the hypotheses that Mardia's Skewness statistic
                            or Mardias' Kurtosis statistic matches one of a
                            normal distribution has to be discarded on a significance
                            level of %s Test result:", input$sigLvl),
                HTML(makeKable(mvnTestResultMV(), bootstrap_options = "basic")),
                HTML("It is thus recommended to continue with the <b>Robust Maximum Likelihood (MLR)</b> estimator.")
            )
        } else if (!("*" %in% mvnTestResultMV()$Signif.)){
            tagList(
                sprintf("The hypotheses that Mardia's Skewness statistic
                        and Mardias' Kurtosis statistic match those of a
                        normal distribution can be maintained on a significance
                        level of %s", input$sigLvl),
                HTML(makeKable(mvnTestResultMV(), bootstrap_options = "basic")),
                HTML("It is thus recommended to continue with the <b>Maximum Likelihood (ML)</b> estimator.")
            )
        } else {
            tagList()
        }
    })

    # Control data before analysis -----------------------------------------------------------------------------------------
    observe({
        # Check the number of items ----------------------------------------------------------------------------------------
        if (length(input$itemCols) == 1) {
            itemCheck <- "&#10005;"
            itemColor <- "red"
            itemTag <- sprintf("ERROR: Only one item selected. No analysis possible.")

            output$oneItem <- reactive({FALSE})
        } else if (length(input$itemCols) == 2) {
            itemCheck <- "(&#10003;)"
            itemColor <- "orange"
            itemTag <- HTML(sprintf("WARNING: Only two items selected. The &tau;-kongeneric and
                                        the essentially &tau;-equivalt model can not be tested."))

            output$oneItem <- reactive({TRUE})
        } else if (length(input$itemCols) == 3) {
            itemCheck <- "(&#10003;)"
            itemColor <- "orange"
            itemTag <- HTML(sprintf("WARNING: Only three items selected. The &tau;-kongeneric
                                        model can not be tested."))

            output$oneItem <- reactive({TRUE})
        } else if (length(input$itemCols) >= 4) {
            itemCheck <- "&#10003;"
            itemColor <- "black"
            itemTag <- sprintf("%i items selected. All models can be tested.", length(input$itemCols))

            output$oneItem <- reactive({TRUE})
        } else {
            itemCheck <- ""
            itemColor <- "black"
            itemTag <- ""
        }

        # Check the number of observations ---------------------------------------------------------------------------------
        if (length(input$itemCols) < nrow(userData())) {
            req(input$groupCol)

            if (input$groupCol == "no") {
                enoughObsCheck <- "&#10003;"
                enoughObsColor <- "black"
                enoughObsTag <- "There are more observations than items."

                output$obsOk <- reactive({TRUE})
            } else if (input$groupCol %in% colnames(userData())) {
                nObsInGroups <- sapply(split(userData(), userData()[, input$groupCol]), nrow)

                if (all(length(input$itemCols) < nObsInGroups)) {
                    enoughObsCheck <- "&#10003;"
                    enoughObsColor <- "black"
                    enoughObsTag <- "There are more observations than items in all groups."

                    output$obsOk <- reactive({TRUE})
                } else {
                    enoughObsCheck <- "&#10005;"
                    enoughObsColor <- "red"
                    enoughObsTag <- "ERROR: There are fewer observations than items in some groups."

                    output$obsOk <- reactive({FALSE})
                }
            }
        } else {
            enoughObsCheck <- "&#10005;"
            enoughObsColor <- "red"
            enoughObsTag <- "ERROR: There are fewer observations than items."

            output$obsOk <- reactive({TRUE})
        }

        # Check for correlations (neg/insig) -------------------------------------------------------------------------------
        if (corrCheck()$neg & corrCheck()$insig) {
            corrNegSigCheck <- "&#10003;"
            corrNegSigColor <- "black"
            corrNegSigTag <- "All correlations are positive and no confidence interval contains 0."
        } else {
            corrNegSigCheck <- "?"
            corrNegSigColor <- "orange"
            corrNegSigTag <- "WARNING: There appear to be negative and/or insignificant correlations. Recheck items."
        }

        # Check for correlative independence -------------------------------------------------------------------------------
        req(input$sigLvl)

        if (all(any(input$itemCols %in% colnames(userData())),
                length(input$itemCols) <= ncol(userData()),
                length(input$itemCols) > 1,
                input$sigLvl > 0 & input$sigLvl < 1)) {

            dummyModel <- paste(sprintf("%s ~ 1", colnames(userData()[, input$itemCols]), collapse = "\n"))

            fittedDummyModel <- tryCatch(cfa(model = dummyModel,
                                             data = userData(),
                                             estimator = input$estimator),
                                         warning = function(w) w,
                                         error = function(e) e)

            if (class(fittedDummyModel)[1] == "lavaan") {
                corrInd <- extract_fit_parameters(fittedDummyModel,
                                                  what = "model_fit")

                if (corrInd[3] < input$sigLvl) {
                    corrIndCheck <- "&#10003;"
                    corrIndColor <- "black"
                    corrIndTag <- tagList(
                        sprintf("The hypothesis that all correlations between the items are equal to zero
                                    has to be discarded on a significance level of %s. Test result:",
                                input$sigLvl),
                        withMathJax(test_result_output(corrInd, input$estimator))
                    )
                    output$isCorrInd <- reactive({TRUE})
                } else {
                    corrIndCheck <- "&#10005;"
                    corrIndColor <- "red"
                    corrIndTag <- tagList(
                        div(style = "color:red",
                            sprintf("ERROR: The hypothesis that all correlations between the items are equal to zero
                                    can be maintained on a significance level of %s. Test result:",
                                    input$sigLvl),
                            withMathJax(
                                paste0("$$\\color{red}",
                                       substring(test_result_output(corrInd, input$estimator), 3))),
                            "It is thus not advised to conduct any further analysis.")
                    )
                    output$isCorrInd <- reactive({FALSE})
                }
            } else {
                corrIndCheck <- "&#10005;"
                corrIndColor <- "red"
                corrIndTag <- tagList(
                    div(style = "color:red",
                        sprintf("Lavaan produced an ERROR/WARNING: %s",
                                fittedDummyModel$message))
                )
                output$isCorrInd <- reactive({TRUE})
            }
        } else {
            corrIndCheck <- ""
            corrIndColor <- "black"
            corrIndTag <- ""
        }

        output$checks <- renderUI({
            if (all(any(input$itemCols %in% colnames(userData())),
                    input$groupCol %in% colnames(userData()) | input$groupCol == "no",
                    length(input$itemCols) <= ncol(userData()))) {

                tagList(
                    div(style = paste0("color:", itemColor),
                        h4(HTML(paste("Number of items:", itemCheck))),
                        itemTag
                    ),
                    div(style = paste0("color:", enoughObsColor),
                        h4(HTML(paste("Number of observations:", enoughObsCheck))),
                        enoughObsTag
                    ),
                    div(style = paste0("color:", corrNegSigColor),
                        h4(HTML(paste("Item correlations:", corrNegSigCheck))),
                        corrNegSigTag
                    ),
                    div(style = paste0("color:", corrIndColor),
                        h4(HTML(paste("Test on correlative independence:", corrIndCheck))),
                        corrIndTag
                    )
                )
            }
        })

        output$allExist <- reactive({
            req(mvnTestResultMV())
            TRUE
        })

        outputOptions(output, "isCorrInd", suspendWhenHidden = FALSE)
        outputOptions(output, "obsOk", suspendWhenHidden = FALSE)
        outputOptions(output, "oneItem", suspendWhenHidden = FALSE)
        outputOptions(output, "allExist", suspendWhenHidden = FALSE)
    })

    # Generate the models --------------------------------------------------------------------------------------------------
    observeEvent(input$goModels, {
        modelsLong <- c("tk" = "&tau;-kongeneric",
                        "ete" = "essentially &tau;-equivalent",
                        "te" = "&tau;-equivalent",
                        "etp" = "essentially &tau;-parallel",
                        "tp" = "&tau;-parallel")

        models <- c("tk", "ete", "te", "etp", "tp")
        names(models) <- models
        modelsToTest <- models[c(input$tk, input$ete, input$te, input$etp, input$tp)]

        modelCodes <- make_model_codes(input_data = userData(),
                                       item_cols = input$itemCols,
                                       group = FALSE)

        fittedModelsWarns <- lapply(modelCodes[modelsToTest],
                                    FUN = function(model) {
                                        tryCatch(cfa(model = model,
                                                     data = userData(),
                                                     meanstructure = TRUE,
                                                     estimator = input$estimator),
                                                 error = function(e) e,
                                                 warning = function(w) w)
                                    })

        fittedModelsErrs <- lapply(modelCodes[modelsToTest],
                                   FUN = function(model) {suppressWarnings(
                                       tryCatch(cfa(model = model,
                                                    data = userData(),
                                                    meanstructure = TRUE,
                                                    estimator = input$estimator),
                                                error = function(e) e)
                                   )})

        warns <- sapply(
            lapply(fittedModelsWarns,
                   class),
            function(code) code[1] == "simpleWarning"
        )
        errs <- sapply(
            lapply(fittedModelsErrs,
                   class),
            function(code) code[1] == "simpleError"
        )

        goodModels <- modelsToTest[!warns & !errs]
        errModels <- modelsToTest[errs]
        warnModels <- modelsToTest[warns]

        if (sum(warns) > 0) {
            output$lavWarnsMsg <- renderUI({
                tagList(
                    h5("The following models produced warnings:"),
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
            })
        }
        if (sum(errs) > 0) {
            output$lavErrsMsg <- renderUI({
                tagList(
                    h5("The following models produced errors:"),
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
            })
        }

        appendTab(inputId = "navbar",
                  tabPanel("Models",
                           value = "panelModelTests",
                           wellPanel(
                               verbatimTextOutput("fits"),
                               h4(HTML(sprintf("Lavaan status: %i warnings, %i errors.", sum(warns), sum(errs)))),
                               htmlOutput("lavErrsMsg"),
                               htmlOutput("lavWarnsMsg")
                           ),
                           tabsetPanel(id = "modelTabsets"
                           )
                  ),
                  select = TRUE)

        # Generate Paramter Tables -----------------------------------------------------------------------------------------
        fits <- lapply(goodModels,
                       function(model) {
                           extract_fit_parameters(fittedModelsWarns[[model]],
                                                  what = "model_fit")
                       })

        output$fits <- renderPrint({
            fits
        })

        for (model in goodModels) {
            local({
                this_model <- model
                output[[paste0(model, "Table")]] <- renderText({
                    kableExtra::add_header_above(
                        kableExtra::row_spec(
                            kableExtra::column_spec(
                                makeKable(
                                    extract_parameters(fittedModelsWarns[[this_model]],
                                                       alpha = input$sigLvl),
                                    col.names = c("Item", rep(c("Par.", "Est.", paste0(input$estimator, "-SE"), "CI"), 4))
                                ),
                                1, bold = TRUE
                            ), length(input$itemCols) + 1, bold = TRUE
                        ),
                        c(" ",
                          "Discrimination Parameters" = 4,
                          "Easiness Parameters" = 4,
                          "Variances" = 4,
                          "Reliabilities" = 4)
                    )

                    output[[paste0(this_model,"ModelFitText")]] <- renderUI({

                    })
                })

                appendTab("modelTabsets",
                          tabPanel(HTML(modelsLong[this_model]),
                                   h3("Test on Model Fit:"),
                                   uiOutput(paste0(this_model,"ModelFitText")),
                                   h3("Test on Model Fit in Subgroups:"),
                                   uiOutput(paste0(this_model,"ModelFitMgText")),
                                   h3("Estimated Paramters with Standard Errors and Confidence Intervals:"),
                                   htmlOutput(paste0(this_model,"Table"))),
                          select = FALSE)
            })
        }

        # Write the selected values! ---------------------------------------------------------------------------------------
        output$selectedData <- renderText({
            paste("The following data was used:",
                  switch(input$source,
                         "Workspace" = sprintf("Object \"%s\" from workspace", input$objectFromWorkspace),
                         "CSV" = sprintf("CSV-File \"%s\"", input$CSVFile$name),
                         "SPSS" = sprintf("SPSS-File \"%s\"", input$SPSSFile$name))
            )
        })

        output$selectedItems <- renderText({
            paste("The following items have been chosen:",
                  paste(input$itemCols, collapse = ", "))
        })

        output$selectedGroup <- renderText({
            if (input$groupCol == "no") {
                "No group variable has been chosen."
            } else {
                paste("The following group column has been chosen:", input$groupCol)
            }
        })

        output$selectedEstimator <- renderText({sprintf("The %s estimator has been chosen.", input$estimator)})

        output$selectedSigLvl <- renderText({
            paste("All tests have been performed on a significance level of", input$sigLvl)
        })

        if (F) {
        # Tau-kongeneric ---------------------------------------------------------------------------------------------------
        tkFit <- extract_fit_parameters(fittedModelsWarns[["tau-kongeneric"]],
                                        what = "model_fit")

        output$tkModelFitText <- renderUI({
            if (tkFit[3] < input$sigLvl) {
                tagList(
                    div(style = "color:red",
                        sprintf("The hypothesis that the model implied covariance matrix and the meanstructure
                                match the empirical ones has to be discarded on a significance level of %s. Test result:",
                                input$sigLvl)
                    ),
                    withMathJax(test_result_output(tkFit, input$estimator)),
                    div(style = "color:red",
                        HTML("The &tau;-kongeneric model is the least restrictive model. Since it does not seem to fit
                        the data it is advised not to continue with the more restrictive models.")
                    )
                )
            } else {
                withMathJax(
                        sprintf("The hypothesis that the model implied covariance matrix and the meanstructure
                                match the empirical ones can be maintained on a significance level of %s. Test result: %s",
                                input$sigLvl,
                                test_result_output(tkFit, input$estimator))
                )
            }
        })

        # essentially tau-equivalent ---------------------------------------------------------------------------------------
        tkEteModelComp <- lavTestLRT(fittedModelsWarns[["tau-kongeneric"]],
                                     fittedModelsWarns[["essentially tau-equivalent"]])

        tkEteModelCompVec <- c("\\chi^2" = tkEteModelComp$`Chisq diff`[2],
                               "df" = tkEteModelComp$`Df diff`[2],
                               "p" = tkEteModelComp$`Pr(>Chisq)`[2])

        output$tkEteModelCompText <- renderUI({
            if (tkEteModelComp$`Pr(>Chisq)`[2] < input$sigLvl) {
                tagList(
                    div(style = "color:red",
                        sprintf("The hypothesis that the essentially &tau;-equivalent model explains the
                                data as good as the &tau;-kongeneric model has to be discarded on a
                                significance level of %s. Test result:",
                                input$sigLvl)
                    ),
                    withMathJax(test_result_output(tkEteModelCompVec,
                                                   input$estimator)),
                    div(style = "color:red",
                        HTML("It is thus advised to stick with the &tau;-kongeneric model and not continue
                             with the more restrictive models.")
                    )
                )
            } else {
                tagList(
                    HTML(sprintf("The hypothesis that the essentially &tau;-equivalent model explains the
                                data as good as the &tau;-kongeneric can be maintained on a
                                significance level of %s. Test result:",
                            input$sigLvl)),
                    withMathJax(test_result_output(tkEteModelCompVec,
                                                   input$estimator))
                )
            }
        })

        appendTab(inputId = "navbar",
                  tabPanel("Models",
                           value = "panelModelTests",
                           tabsetPanel(
                               tabPanel(HTML("&tau;-kongeneric"),
                                        h3("Test on Model Fit:"),
                                        uiOutput("tkModelFitText"),
                                        h3("Test on Model Fit in Subgroups:"),
                                        uiOutput("tkModelFitMgText"),
                                        h3("Estimated Paramters with Standard Errors and Confidence Intervals:"),
                                        htmlOutput("tkTable")),
                               tabPanel(HTML("essentially &tau;-equivalent"),
                                        h3(HTML("Test on Model Fit against the &tau;-kongeneric Model:")),
                                        uiOutput("tkEteModelCompText"),
                                        h3(HTML("Test on Model Fit in Subgroups against the &tau;-kongeneric Model:")),
                                        uiOutput("tkEteModelCompTextMg"),
                                        h3("Estimated Paramters with Standard Errors and Confidence Intervals:"),
                                        htmlOutput("eteTable")),
                               tabPanel(HTML("&tau;-equivalent"),
                                        h3("Test on Model Fit:"),
                                        uiOutput("teModelFitText"),
                                        h3("Estimated Paramters with Standard Errors and Confidence Intervals:"),
                                        htmlOutput("teTable")),
                               tabPanel(HTML("essentially &tau;-parallel"),
                                        h3("Test on Model Fit:"),
                                        uiOutput("etpModelFitText"),
                                        h3("Estimated Paramters with Standard Errors and Confidence Intervals:"),
                                        htmlOutput("etpTable")),
                               tabPanel(HTML("&tau;-parallel"),
                                        h3("Test on Model Fit:"),
                                        uiOutput("tpModelFitText"),
                                        h3("Estimated Paramters with Standard Errors and Confidence Intervals:"),
                                        htmlOutput("tpTable"))
                           )
                  ),
                  select = TRUE)
        }
    })
}
