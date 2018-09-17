function(input, output, session) {

    ########################################################################################################################
    ## ------------------------------------------------- Data input ----------------------------------------------------- ##
    ########################################################################################################################
    output$objectsInWorkspace <- renderUI({
        selectInput("objectFromWorkspace",
                    "Choose data object from Workspace:",
                    Filter(function(object) !is.null(dim(get(object))) && typeof(get(object)) != "character",
                           ls(envir = globalenv())))
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
        possibleItemColumns <- colnames(userData())[sapply(userData(), is.numeric)]

        checkboxGroupInput("itemCols",
                           "Select which columns contain items",
                           choices = possibleItemColumns,
                           selected = possibleItemColumns,
                           inline = TRUE)
    })

    userDataItems <- reactive({
        if (length(input$itemCols) > 1) {
            tryCatch(userData()[, input$itemCols],
                     warning = function(w) NULL,
                     error = function(e) NULL)
        } else {
            NULL
        }
    })

    output$groupColChooser <- renderUI({
        selectInput("groupCol",
                    "Select which column contains the group",
                    choices = c("No group column selected" = "no",
                                colnames(userData())[!(colnames(userData()) %in% input$itemCols)]))
    })

    observeEvent(input$sigLvl, {
        if ((input$sigLvl < 0 | input$sigLvl > 1) & !is.na(input$sigLvl)) {
            updateNumericInput(session,
                               "sigLvl",
                               value = 0.05)
        }
    })

    # Update comparison logicals if deactivated ----------------------------------------------------------------------------
    modelsLong <- c("&tau;-kongeneric",
                    "essentially &tau;-equivalent",
                    "&tau;-equivalent",
                    "essentially &tau;-parallel",
                    "&tau;-parallel")
    models <- c("tko", "ete", "teq", "etp", "tpa")

    names(models) <- names(modelsLong) <- models

    possComps <- outer(models, models, paste0)[lower.tri(diag(5))][-8]

    lapply(
        models,
        function(thisModel) {
            observeEvent(input[[thisModel]], {
                lapply(
                    possComps[grep(thisModel, possComps)],
                    function(thisComp) {
                        if ((substr(thisComp, 1, 3) == thisModel && input[[substr(thisComp, 4, 6)]]) ||
                            (substr(thisComp, 4, 6) == thisModel && input[[substr(thisComp, 1, 3)]]))
                            updateCheckboxInput(
                                session,
                                thisComp,
                                value = input[[thisModel]]
                            )
                    }
                )
            })
        }
    )

    ########################################################################################################################
    ## ------------------------------------ Generate content in overview panels ----------------------------------------- ##
    ########################################################################################################################

    # Descriptives ---------------------------------------------------------------------------------------------------------
    output$descrTable <- renderUI({
        req(userDataItems())

        table <- t(apply(
            userDataItems(),
            2,
            function(col) c(Mean = mean(col),
                            Sd = sd(col),
                            Skew = moments::skewness(col),
                            Excess = moments::kurtosis(col) - 3))
        )

        tagList(
            h4("Mean, Standard Deviation, Skewness, Excess:"),
            HTML(makeKable(table))
        )
    })

    output$covMat <- renderUI({
        req(userDataItems())

        table <- cov(userDataItems())
        table[upper.tri(table)] <- NA

        tagList(
            h4("Covariance Matrix:"),
            HTML(
                kableExtra::column_spec(
                    makeKable(table),
                    1, bold = TRUE
                )
            )
        )
    })

    corrTableWithCIsRaw <- reactive({
        req(userDataItems())

        list(cor = cor(userDataItems()),
             test = tryCatch(
                 corrplot::cor.mtest(userDataItems(),
                                     conf.level = (1 - input$sigLvl)),
                 warning = function(w) w,
                 error = function(e) e
             )
        )
    })

    output$corrTableWithCIs <- renderUI({
        CIs <- corrTableWithCIsRaw()$test

        if (class(CIs)[1] == "list") {
            corrTableRaw <- corrTableWithCIsRaw()$cor
            corrTable <- matrix("", nrow = nrow(corrTableRaw), ncol = ncol(corrTableRaw))

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

            tagList(
                h4("Correlation Table with Confidence Intervals:"),
                HTML(makeKable(corrTable)),
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

    # Calculate test on MVN ------------------------------------------------------------------------------------------------
    mvnTestResultRaw <- reactive({
        tryCatch(
            MVN::mvn(userDataItems()),
            warning = function(w) w,
            error = function(e) e
        )
    })

    output$mvnTableUV <- renderUI({
        if (class(mvnTestResultRaw())[1] == "list") {

            mvnUV <- data.frame(Test = as.character(mvnTestResultRaw()$univariateNormality$Test),
                                Item = as.character(mvnTestResultRaw()$univariateNormality$Variable),
                                Statistic = as.numeric(mvnTestResultRaw()$univariateNormality$Statistic),
                                p = suppressWarnings(as.numeric(mvnTestResultRaw()$univariateNormality$`p value`)),
                                stringsAsFactors = F)

            mvnUV$p[is.na(mvnUV$p)] <- 0
            mvnUV$Signif. <- ifelse(mvnUV$p < input$sigLvl, "*", "")
            mvnUV$p <- sapply(mvnUV$p,
                              function(value) if (value < 0.001) "< 0.001" else sprintf("%.3f", round(value, 3)))

            tagList(
                h4("Tests on Univariate Normality:"),
                HTML(makeKable(mvnUV, bootstrap_options = "basic"))
            )
        }
    })

    observe({
        if (class(mvnTestResultRaw())[1] == "list") {
            req(input$sigLvl)

            if (any(as.numeric(as.character(mvnTestResultRaw()$multivariateNormality$`p value`[-3])) < input$sigLvl)) {
                updateEst <- "MLR"
            } else {
                updateEst <- "ML"
            }

            updateRadioButtons(session,
                               "estimator",
                               selected = updateEst)
        }
    })

    output$mvnComment <- renderUI({
        mvnMV <- data.frame(Test = as.character(mvnTestResultRaw()$multivariateNormality$Test),
                            Statistic = as.numeric(as.character(mvnTestResultRaw()$multivariateNormality$Statistic)),
                            p = as.numeric(as.character(mvnTestResultRaw()$multivariateNormality$`p value`)),
                            Signif. = as.character(mvnTestResultRaw()$multivariateNormality$Result),
                            stringsAsFactors = F)[-3,]

        mvnMV$Signif. <- ifelse(mvnMV$p < input$sigLvl, "*", "")
        mvnMV$p <- sapply(mvnMV$p,
                          function(value) if (value < 0.001) "< 0.001" else sprintf("%.3f", round(value, 3)))

        if (is.null(mvnTestResultRaw()$multivariateNormality)) {
            tagList()
        } else {
            if ("*" %in% mvnMV$Signif.) {
                tagList(
                    h4("Test on Multivariate Normality:"),
                    sprintf("At least one of the hypotheses that Mardia's Skewness statistic
                            or Mardias' Kurtosis statistic matches one of a
                            normal distribution has to be discarded on a significance
                            level of %s. Test result:", input$sigLvl),
                    HTML(makeKable(mvnMV, bootstrap_options = "basic")),
                    HTML("It is thus recommended to continue with the <b>Robust Maximum Likelihood (MLR)</b> estimator.")
                )
            } else {
                tagList(
                    h4("Test on Multivariate Normality:"),
                    sprintf("The hypotheses that Mardia's Skewness statistic
                        and Mardias' Kurtosis statistic match those of a
                        normal distribution can be maintained on a significance
                        level of %s. Test result:", input$sigLvl),
                    HTML(makeKable(mvnMV, bootstrap_options = "basic")),
                    HTML("It is thus recommended to continue with the <b>Maximum Likelihood (ML)</b> estimator.")
                )
            }
        }
    })

    ########################################################################################################################
    ## ----------------------------------------- Check data before analysis --------------------------------------------- ##
    ########################################################################################################################
    observe({
        # Check the number of items ----------------------------------------------------------------------------------------
        if (length(input$itemCols) == 1) {
            itemCheck <- "&#10005;"
            itemColor <- "red"
            itemTag <- sprintf("ERROR: Only one or no item selected. No analysis possible.")

            output$oneItem <- reactive({FALSE})

            for (model in models) {
                updateCheckboxInput(session,
                                    model,
                                    value = FALSE)
            }
        } else if (length(input$itemCols) == 2) {
            itemCheck <- "(&#10003;)"
            itemColor <- "orange"
            itemTag <- HTML("WARNING: Only two items selected. The &tau;-kongeneric and
                                        the essentially &tau;-equivalt model can not be tested.")

            output$oneItem <- reactive({TRUE})

            for (model in models) {
                updateCheckboxInput(session,
                                    model,
                                    value = !(model %in% c("tko", "ete")))
            }
        } else if (length(input$itemCols) == 3) {
            itemCheck <- "(&#10003;)"
            itemColor <- "orange"
            itemTag <- HTML(sprintf("WARNING: Only three items selected. The &tau;-kongeneric
                                        model can not be tested."))

            output$oneItem <- reactive({TRUE})

            for (model in models) {
                updateCheckboxInput(session,
                                    model,
                                    value = model != "tko")
            }
        } else if (length(input$itemCols) >= 4) {
            itemCheck <- "&#10003;"
            itemColor <- "black"
            itemTag <- NULL

            output$oneItem <- reactive({TRUE})

            for (model in models) {
                updateCheckboxInput(session,
                                    model,
                                    value = TRUE)
            }
        } else {
            itemCheck <- NULL
            itemColor <- "black"
            itemTag <- NULL
        }

        # Check the number of observations ---------------------------------------------------------------------------------
        if (length(input$itemCols) < nrow(userData())) {
            req(input$groupCol)

            if (input$groupCol == "no") {
                enoughObsCheck <- "&#10003;"
                enoughObsColor <- "black"
                enoughObsTag <- NULL

                output$obsOk <- reactive({TRUE})
            } else if (input$groupCol %in% colnames(userData())) {
                nObsInGroups <- sapply(split(userData(), userData()[, input$groupCol]), nrow)

                if (all(length(input$itemCols) < nObsInGroups)) {
                    enoughObsCheck <- "&#10003;"
                    enoughObsColor <- "black"
                    enoughObsTag <- NULL

                    output$obsOk <- reactive({TRUE})
                } else {
                    enoughObsCheck <- "&#10005;"
                    enoughObsColor <- "red"
                    enoughObsTag <- "ERROR: There are fewer observations than items in some groups.
                    Multigroupanalysis not possible. Please deselect the group column to continue."

                    output$obsOk <- reactive({FALSE})
                }
            }
        } else {
            enoughObsCheck <- "&#10005;"
            enoughObsColor <- "red"
            enoughObsTag <- "ERROR: There are fewer observations than items."

            output$obsOk <- reactive({FALSE})
        }

        # Check for correlations (neg/insig) -------------------------------------------------------------------------------
        if (class(corrTableWithCIsRaw()$test)[1] == "list") {
            ps <- corrTableWithCIsRaw()$test$p
            cors <- corrTableWithCIsRaw()$cor

            if (any(ps >= input$sigLvl) && any(cors < 0)) {
                corrNegSigCheck <- "?"
                corrNegSigColor <- "orange"
                corrNegSigTag <- "WARNING: There appear to be insignificant and negative correlations. Recheck items."
            } else if (any(ps >= input$sigLvl) && any(cors >= 0)) {
                corrNegSigCheck <- "?"
                corrNegSigColor <- "orange"
                corrNegSigTag <- "WARNING: There appear to be insignificant correlations. Recheck items."
            } else if (all(ps < input$sigLvl) && any(cors < 0)) {
                corrNegSigCheck <- "&#10005;"
                corrNegSigColor <- "red"
                corrNegSigTag <- "WARNING: There appear to be negative correlations. Recheck items."
            } else {
                corrNegSigCheck <- "&#10003;"
                corrNegSigColor <- "black"
                corrNegSigTag <- NULL
            }

            output$corrNegNoErr <- reactive({TRUE})
        } else {
            corrNegSigCheck <- "&#10005;"
            corrNegSigColor <- "red"
            corrNegSigTag <- paste("WARNING/ERROR:", corrTableWithCIsRaw()$test$message)

            output$corrNegNoErr <- reactive({FALSE})
        }

        # Check for correlative independence -------------------------------------------------------------------------------
        dummyModel <- reactive({
            paste(sprintf("%s ~ 1", colnames(userDataItems()), collapse = "\n"))
        })

        fittedDummyModel <- tryCatch(cfa(model = dummyModel(),
                                         data = userData(),
                                         estimator = input$estimator),
                                     warning = function(w) w,
                                     error = function(e) e)

        if (class(fittedDummyModel)[1] == "lavaan") {
            req(input$sigLvl)

            corrInd <- extractFitParameters(fittedDummyModel)$chisq

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
                    sprintf("ERROR: The hypothesis that all correlations between the items are equal to zero
                                    can be maintained on a significance level of %s. Test result:",
                            input$sigLvl),
                    withMathJax(
                        paste0("$$\\color{red}",
                               substring(test_result_output(corrInd, input$estimator), 3)))
                )
                output$isCorrInd <- reactive({FALSE})
            }
        } else {
            corrIndCheck <- "&#10005;"
            corrIndColor <- "red"
            corrIndTag <- paste("WARNING/ERROR:", fittedDummyModel$message)

            output$isCorrInd <- reactive({FALSE})
        }

        output$checks <- renderUI({
            tagList(
                div(style = paste0("color:", itemColor),
                    h5(HTML(paste("Number of items:", itemCheck))),
                    itemTag
                ),
                div(style = paste0("color:", enoughObsColor),
                    h5(HTML(paste("Number of observations:", enoughObsCheck))),
                    enoughObsTag
                ),
                div(style = paste0("color:", corrNegSigColor),
                    h5(HTML(paste("Item correlations:", corrNegSigCheck))),
                    corrNegSigTag
                ),
                div(style = paste0("color:", corrIndColor),
                    h5(HTML(paste("Test on correlative independence:", corrIndCheck))),
                    corrIndTag
                )
            )
        })

        outputOptions(output, "isCorrInd", suspendWhenHidden = FALSE)
        outputOptions(output, "obsOk", suspendWhenHidden = FALSE)
        outputOptions(output, "oneItem", suspendWhenHidden = FALSE)
        outputOptions(output, "corrNegNoErr", suspendWhenHidden = FALSE)
    })

    ########################################################################################################################
    ## --------------------------------------------- Generate the models ------------------------------------------------ ##
    ########################################################################################################################
    observeEvent(input$goModels, {

        modelsToTest <- models[sapply(models, function(thisModel) input[[thisModel]])]

        # Try fitting and capture warning and error messages ---------------------------------------------------------------
        modelCodes <- make_model_codes(input_data = userData(),
                                       item_cols = input$itemCols,
                                       group = FALSE)

        fittedModelsWarns <- lapply(
            modelCodes[modelsToTest],
            FUN = function(model) {
                tryCatch(cfa(model = model,
                             data = userData(),
                             meanstructure = TRUE,
                             estimator = input$estimator),
                         error = function(e) e,
                         warning = function(w) w)
            }
        )

        fittedModelsErrs <- lapply(
            modelCodes[modelsToTest],
            FUN = function(model) {
                suppressWarnings(
                    tryCatch(cfa(model = model,
                                 data = userData(),
                                 meanstructure = TRUE,
                                 estimator = input$estimator),
                             error = function(e) e)
                )
            }
        )

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
            output$lavWarnsMsg <- renderUI({
                tagList(
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
            })
        }
        if (sum(errs) > 0) {
            output$lavErrsMsg <- renderUI({
                tagList(
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
            })
        }

        # Compare models in groups if specified ----------------------------------------------------------------------------
        if (input$groupCol != "no") {

            # Try fitting and capture warning and error messages in the multigroup models ----------------------------------
            modelCodesMg <- make_model_codes(input_data = userData(),
                                             item_cols = input$itemCols,
                                             group = input$groupCol)

            fittedModelsWarnsMg <- lapply(
                modelCodesMg[modelsToTest],
                FUN = function(model) {
                    tryCatch(cfa(model = model,
                                 data = userData(),
                                 meanstructure = TRUE,
                                 group = input$groupCol,
                                 group.equal = c("loadings", "intercepts"),
                                 estimator = input$estimator),
                             error = function(e) e,
                             warning = function(w) w)
                }
            )

            fittedModelsErrsMg <- lapply(
                modelCodesMg[modelsToTest],
                FUN = function(model) {
                    suppressWarnings(
                        tryCatch(cfa(model = model,
                                     data = userData(),
                                     meanstructure = TRUE,
                                     group = input$groupCol,
                                     group.equal = c("loadings", "intercepts"),
                                     estimator = input$estimator),
                                 error = function(e) e)
                    )
                }
            )

            warnsMg <- sapply(
                lapply(fittedModelsWarnsMg, class),
                function(code) code[1] == "simpleWarning"
            )
            errsMg <- sapply(
                lapply(fittedModelsErrsMg, class),
                function(code) code[1] == "simpleError"
            )

            goodModelsMg <- modelsToTest[!warnsMg & !errsMg]
            errModelsMg <- modelsToTest[errsMg]
            warnModelsMg <- modelsToTest[warnsMg]

            if (sum(warnsMg) > 0) {
                output$lavWarnsMsgMg <- renderUI({
                    tagList(
                        h6("The following multigroup models produced warnings:"),
                        div(style = "color:orange",
                            HTML(
                                kableExtra::column_spec(
                                    kableExtra::kable(
                                        cbind(
                                            paste0(modelsLong[warnModelsMg],
                                                   ":&emsp;"),
                                            sapply(fittedModelsWarnsMg[warnModelsMg],
                                                   function(model) model$message)),
                                        row.names = FALSE,
                                        escape = FALSE),
                                    1, bold = TRUE
                                )
                            )
                        )
                    )
                })
            }
            if (sum(errsMg) > 0) {
                output$lavErrsMsgMg <- renderUI({
                    tagList(
                        h6("The following multigroup models produced errors:"),
                        div(style = "color:red",
                            HTML(
                                kableExtra::column_spec(
                                    kableExtra::kable(
                                        cbind(
                                            paste0(modelsLong[errModelsMg],
                                                   ":&emsp;"),
                                            sapply(fittedModelsErrsMg[errModelsMg],
                                                   function(model) model$message)),
                                        row.names = FALSE,
                                        escape = FALSE),
                                    1, bold = TRUE
                                )
                            )
                        )
                    )
                })
            }
        } else {
            warnsMg <- errsMg <- 0
        }

        # Print the error/warnings -----------------------------------------------------------------------------------------
        appendTab(
            inputId = "navbar",
            tabPanel(
                "Models",
                value = "panelModelTests",
                wellPanel(
                    h5(HTML(sprintf("Lavaan status: %i warnings, %i errors.",
                                    sum(warns) + sum(warnsMg),
                                    sum(errs) + sum(errsMg)))),
                    htmlOutput("lavErrsMsg"),
                    htmlOutput("lavWarnsMsg"),
                    htmlOutput("lavErrsMsgMg"),
                    htmlOutput("lavWarnsMsgMg")
                ),
                tabsetPanel(id = "modelTabsets")
            ),
            select = TRUE
        )

        # Generate comparative fit table and tab  --------------------------------------------------------------------------
        fits <- lapply(fittedModelsWarns[goodModels], extractFitParameters)
        comps <- possComps[sapply(possComps, function(thisComp) input[[thisComp]])]

        if (input$groupCol != "no") {
            fitsMg <- lapply(fittedModelsWarnsMg[goodModelsMg], extractFitParameters)
            compsMg <- possComps[sapply(possComps, function(thisComp) input[[thisComp]])]
        }

        compTable <- reactiveValues(
            df = matrix(NA, ncol = 5, nrow = 5),
            chisq = matrix(NA, ncol = 5, nrow = 5)
        )

        names(compTable$df) <- names(compTable$chisq) <- outer(models, models, sprintf, fmt = "%s%s")

        # Generate Paramter Tables, Fits and Fit Tables --------------------------------------------------------------------
        for (model in goodModels) {
            local({
                thisModel <- model
                output[[paste0(thisModel, "Table")]] <- renderText({
                    kableExtra::add_header_above(
                        kableExtra::row_spec(
                            kableExtra::column_spec(
                                makeKable(
                                    extractParameters(fittedModelsWarns[[thisModel]],
                                                      alpha = input$sigLvl),
                                    col.names = c("Item",
                                                  rep(c("Par.", "Est.", paste0(input$estimator, "-SE"), "CI"), 4))
                                ),
                                1,
                                bold = TRUE),
                            length(input$itemCols) + 1,
                            bold = TRUE),
                        c(" ",
                          "Discrimination Parameters" = 4,
                          "Easiness Parameters" = 4,
                          "Variances" = 4,
                          "Reliabilities" = 4)
                    )
                })

                if (fits[[thisModel]]$chisq[3] < 0.05) {
                    sigAddon <- "*"
                    sigColor <- "darkred"
                    sigTxtColor <- "white"

                    if (fits[[thisModel]]$chisq[3] < 0.01)
                        sigAddon <- paste0(sigAddon, "*")
                    if (fits[[thisModel]]$chisq[3] < 0.001)
                        sigAddon <- paste0(sigAddon, "*")

                } else {
                    sigAddon <- ""
                    sigColor <- "darkgreen"
                    sigTxtColor <- "white"
                }

                compTable$chisq[
                    paste0(thisModel, thisModel)
                    ] <- kableExtra::cell_spec(
                        sprintf(paste0("%.3f", sigAddon), fits[[thisModel]]$chisq[1]),
                        background = sigColor,
                        color = sigTxtColor,
                        italic = TRUE
                    )
                compTable$df[
                    paste0(thisModel, thisModel)
                    ] <- kableExtra::cell_spec(
                        sprintf("%i", fits[[thisModel]]$chisq[2]),
                        background = sigColor,
                        color = sigTxtColor,
                        italic = TRUE
                    )

                if (which(goodModels == thisModel) == 1 ||
                    (goodModels[1] == "teq" && thisModel == "etp")) {

                    output[[paste0(thisModel, "ModelFitText")]] <- renderUI({
                        if (fits[[thisModel]]$chisq[3] < input$sigLvl) {
                            fitColor <- "orange"
                            fitText <- sprintf(
                                "The hypothesis that the model implied covariance matrix and the meanstructure
                                match the empirical ones has to be discarded on a significance level of %s.
                                Test result:",
                                input$sigLvl
                            )
                            fitParams <- withMathJax(
                                paste0("$$\\color{orange}",
                                       substring(
                                           test_result_output(fits[[thisModel]]$chisq, input$estimator),
                                           3))
                            )
                        } else {
                            fitColor <- "black"
                            fitText <- sprintf(
                                "The hypothesis that the model implied covariance matrix and the meanstructure
                                match the empirical ones can be maintained on a significance level of %s.
                                Test result:",
                                input$sigLvl
                            )
                            fitParams <- withMathJax(
                                test_result_output(fits[[thisModel]]$chisq, input$estimator)
                            )
                        }

                        tagList(
                            div(style = paste0("color:", fitColor),
                                h4("Test on Model Fit:"),
                                fitText,
                                fitParams
                            ),
                            h5("Approximative fit indices:"),
                            HTML(
                                kableExtra::column_spec(
                                    makeKable(
                                        data.frame(
                                            Index = c(fits[[goodModels[thisModel]]]$rmsea[1],
                                                      fits[[thisModel]]$cfi,
                                                      fits[[thisModel]]$srmr),
                                            p = c(fits[[thisModel]]$rmsea[2], NA, NA),
                                            CI = c(sprintf("[%.3f; %.3f]",
                                                           fits[[thisModel]]$rmsea[3],
                                                           fits[[thisModel]]$rmsea[4]),
                                                   NA,
                                                   NA),
                                            row.names = c("RMSEA", paste0(input$estimator, "-CFI"), "SRMR")
                                        ),
                                        bootstrap_options = "basic"
                                    ),
                                    1,
                                    bold = TRUE
                                )
                            )
                        )
                    })
                }

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
                        tmpTbl <- lavTestLRT(fittedModelsWarns[[thisModel]], fittedModelsWarns[[thisComp]])
                        tmpFit <- unlist(tmpTbl[2, c(5, 6, 7)])
                        names(tmpFit) <- c("\\Delta chisq", "\\Delta df", "p")

                        tmpFit
                    }
                )

                for (thisComp in compsWithThisModel) {
                    if (fitCompsWithThisModel[3, thisComp] < 0.05) {
                        sigAddon <- "*"
                        sigColor <- "darkred"
                        sigTxtColor <- "white"

                        if (fitCompsWithThisModel[3, thisComp] < 0.01)
                            sigAddon <- paste0(sigAddon, "*")
                        if (fitCompsWithThisModel[3, thisComp] < 0.001)
                            sigAddon <- paste0(sigAddon, "*")

                    } else {
                        sigAddon <- ""
                        sigColor <- "darkgreen"
                        sigTxtColor <- "white"
                    }

                    compTable$chisq[
                        paste0(thisModel, thisComp)
                    ] <- kableExtra::cell_spec(
                        sprintf(paste0("%.3f", sigAddon), fitCompsWithThisModel[1, thisComp]),
                        background = sigColor,
                        color = sigTxtColor
                    )

                    compTable$df[
                        paste0(thisModel, thisComp)
                    ] <- kableExtra::cell_spec(
                        sprintf("%i", fitCompsWithThisModel[2, thisComp]),
                        background = sigColor,
                        color = sigTxtColor
                    )
                }

                #compTable$df[paste0(thisModel,
                #                    compsWithThisModel)] <- sprintf(
                #                        "+%i",
                #                        fitCompsWithThisModel[2, compsWithThisModel]
                #                    )

                compTagList <- lapply(
                    compsWithThisModel,
                    function(thisComp) {
if (F) {
                        if (fitCompsWithThisModel[[thisComp]][3] < input$sigLvl) {
                            compFitColor <- "orange"
                            compFitText <- HTML(sprintf(
                                "The hypothesis that the %s model explains the data as good
                                as the %s model has to be discarded on a significance level of %s.
                                Test result:",
                                modelsLong[thisModel],
                                modelsLong[thisComp],
                                input$sigLvl
                            ))
                            compFitParams <- withMathJax(
                                paste0("$$\\color{orange}",
                                       substring(
                                           test_result_output(fitCompsWithThisModel[[thisComp]], input$estimator),
                                           3))
                            )
                        } else {
                            compFitColor <- "black"
                            compFitText <- HTML(sprintf(
                                "The hypothesis that the %s model explains the data as good
                                as the %s model can be maintained on a significance level of %s.
                                Test result:",
                                modelsLong[thisModel],
                                modelsLong[thisComp],
                                input$sigLvl
                            ))
                            compFitParams <- withMathJax(
                                test_result_output(fitCompsWithThisModel[[thisComp]],
                                                   input$estimator)
                            )
                        }
                        tagList(
                            div(style = paste0("color:", compFitColor),
                                h5(HTML(sprintf(
                                    "Test on Model Fit against the <b>%s</b> model",
                                    modelsLong[thisComp]
                                ))),
                                compFitText,
                                compFitParams
                            )
                        )
}

                    }
                )

                output[[paste0(thisModel, "compTable")]] <- renderText({
                    infParams <- t(sapply(
                        c(as.character(compsWithThisModel),
                          if (thisModel == "teq" && "etp" %in% goodModels)
                              "etp",
                          if (thisModel == "etp" && "teq" %in% goodModels)
                              "teq",
                          thisModel),
                        function(thisComp) {
                            c(fits[[goodModels[[thisComp]]]]$cfi,
                              fits[[goodModels[[thisComp]]]]$aic,
                              fits[[goodModels[[thisComp]]]]$bic)
                        }
                    ))

                    rownames(infParams) <- modelsLong[rownames(infParams)]
                    colnames(infParams)[1] <- paste0(input$estimator, "-CFI")
                    colnames(infParams) <- toupper(colnames(infParams))

                    kableExtra::row_spec(
                        if ((thisModel == "teq" && "etp" %in% goodModels) ||
                            (thisModel == "etp" && "teq" %in% goodModels)) {
                            kableExtra::row_spec(
                                kableExtra::column_spec(
                                    makeKable(infParams),
                                    1,
                                    bold = TRUE
                                ),
                                length(compsWithThisModel) + 1,
                                italic = TRUE
                            )
                        } else {
                            kableExtra::column_spec(
                                makeKable(infParams),
                                1,
                                bold = TRUE
                            )
                        },
                        length(compsWithThisModel) + 1 +
                            ((thisModel == "teq" && "etp" %in% goodModels) ||
                                 (thisModel == "etp" && "teq" %in% goodModels)),
                        extra_css = "background-color:grey; color:white;"
                    )
                })

                # Do the same for the group if specified -------------------------------------------------------------------
                if (input$groupCol != "no" && thisModel %in% goodModelsMg) {
                    compsWithThisModelMg <- compsWithThisModel[compsWithThisModel %in% goodModelsMg]

                    if (which(goodModels == thisModel) == 1 ||
                        (goodModels[1] == "teq" && thisModel == "etp")) {

                        output[[paste0(goodModelsMg[1], "ModelFitTextMg")]] <- renderUI({
                            if (fitsMg[[goodModelsMg[1]]]$chisq[3] < input$sigLvl) {
                                fitColor <- "orange"
                                fitText <- sprintf(
                                    "The hypothesis that [subgroups] has to be discarded
                                    on a significance level of %s.
                                    Test result:",
                                    input$sigLvl
                                )
                                fitParams <- withMathJax(
                                    paste0("$$\\color{red}",
                                           substring(
                                               test_result_output(fitsMg[[goodModelsMg[1]]]$chisq,
                                                                  input$estimator),
                                               3)
                                    )
                                )
                            } else {
                                fitColor <- "black"
                                fitText <- sprintf(
                                    "The hypothesis that [subgroups] can be maintained
                                    on a significance level of %s.
                                    Test result:",
                                    input$sigLvl
                                )
                                fitParams <- withMathJax(
                                    test_result_output(fitsMg[[goodModelsMg[1]]]$chisq,
                                                       input$estimator)
                                )
                            }

                            tagList(
                                div(style = paste0("color:", fitColor),
                                    h4("Test on Model Fit in Subgroups:"),
                                    fitText,
                                    fitParams
                                )
                            )
                        })
                    }

                    compTagListMg <- lapply(
                        compsWithThisModelMg,
                        function(comp) {
                            tmpTable <- lavTestLRT(fittedModelsWarnsMg[[thisModel]], fittedModelsWarnsMg[[comp]])
                            tmpFit <- unlist(tmpTable[2, c(5, 6, 7)])
                            names(tmpFit) <- c("chisq", "df", "p")

                            if (tmpFit[3] < input$sigLvl) {
                                compFitColor <- "orange"
                                compFitText <- HTML(sprintf(
                                    "The hypothesis that the %s model explains the data as good
                                    as the <b>%s</b> model has to be discarded on a significance level of %s.
                                    Test result:",
                                    modelsLong[thisModel],
                                    modelsLong[comp],
                                    input$sigLvl
                                ))
                                compFitParams <- withMathJax(
                                    paste0("$$\\color{orange}",
                                           substring(
                                               test_result_output(tmpFit, input$estimator),
                                               3)
                                    )
                                )
                            } else {
                                compFitColor <- "black"
                                compFitText <- HTML(sprintf(
                                    "The hypothesis that the %s model explains the data as good
                                    as the %s model can be maintained on a significance level of %s.
                                    Test result:",
                                    modelsLong[thisModel],
                                    modelsLong[comp],
                                    input$sigLvl
                                ))
                                compFitParams <- withMathJax(
                                    test_result_output(tmpFit,
                                                       input$estimator)
                                )
                            }

                            tagList(
                                div(style = paste0("color:", compFitColor),
                                    h5(HTML(sprintf(
                                        "Test on Model Fit in Subgroups against the <b>%s</b> Model",
                                        modelsLong[comp]
                                    ))),
                                compFitText,
                                compFitParams
                                )
                            )
                        }
                    )
                } else {
                    compTagListMg <- NULL
                }

                # Create Tab -----------------------------------------------------------------------------------------------
                appendTab(
                    inputId = "modelTabsets",
                    tabPanel(
                        title = HTML(modelsLong[thisModel]),
                        verbatimTextOutput(paste0(thisModel, "Comps")),
                        uiOutput(paste0(thisModel,"ModelFitText")),
                        if (which(goodModels == thisModel) > 1 &&
                            !(goodModels[1] == "teq" && thisModel == "etp")) {

                            tagList(
                                h4("Comparative Model Fit Tests:"),
                                compTagList,
                                h5("Information and approximative fit indices:"),
                                htmlOutput(paste0(thisModel, "compTable"))
                            )
                        },
                        uiOutput(paste0(thisModel,"ModelFitTextMg")),
                        if (input$groupCol != "no" &&
                            thisModel %in% goodModelsMg &&
                            (which(goodModels == thisModel) > 1 &&
                             !(goodModels[1] == "teq" && thisModel == "etp"))) {

                            tagList(
                                h4("Comparative Model Fit Tests in Subgroups:"),
                                compTagListMg
                            )
                        },
                        h4("Estimated Paramters with Standard Errors and Confidence Intervals:"),
                        htmlOutput(paste0(thisModel, "Table"))
                    ),
                    select = FALSE
                )
            })
        }

        if (length(goodModels) > 0) {
            combCompTable <- matrix(NA, nrow = 5, ncol = 10)

            combCompTable[, seq(1, 10, 2)] <- matrix(compTable$df, ncol = 5, nrow = 5)
            combCompTable[, seq(2, 10, 2)] <- matrix(compTable$chisq, ncol = 5, nrow = 5)

            modelsAbbrev <- c("&#964;-kong.",
                              "ess. &#964;-equiv.",
                              "&#964;-equiv.",
                              "ess. &#964;-paral.",
                              "&#964;-paral.")

            rownames(combCompTable) <- modelsAbbrev

            colnames(combCompTable) <- rep(c("&Delta;df", "&Delta;chisq"), 5)

            headerNames <- c("", rep(2, 5))
            names(headerNames) <- c("", modelsAbbrev)

            prependTab(
                inputId = "modelTabsets",
                tabPanel(
                    "Comparison overview",
                    h4("Comparison Table:"),
                    HTML(
                        kableExtra::add_header_above(
                            kableExtra::column_spec(
                                makeKable(combCompTable),
                                1,
                                bold = TRUE
                            ),
                            headerNames,
                            escape = FALSE
                        )
                    )
                ),
                select = TRUE
            )
        }

        # Write the selected values ----------------------------------------------------------------------------------------
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
    })
}
