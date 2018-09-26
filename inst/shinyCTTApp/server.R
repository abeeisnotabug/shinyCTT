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

    names(models) <- names(modelsLong) <- names(modelsExpr) <- names(modelsAbbrev) <- models

    possComps <- outer(models, models, paste0)[lower.tri(diag(5))][-8]

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

            userDataTmp <- read.csv(file = input$CSVFile$datapath,
                                    header = input$header,
                                    sep = input$sep,
                                    quote = input$quote)
        } else if (input$source == "SPSS") {
            req(input$SPSSFile)

            userDataTmp <- haven::read_spss(file = input$SPSSFile$datapath)
        } else if (input$source == "Workspace") {
            req(input$objectFromWorkspace)

            userDataTmp <- get(input$objectFromWorkspace)
        }

        as.data.frame(userDataTmp)
    })

    output$dataOverview <- renderDataTable(userData())

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
            HTML(
                kableExtra::column_spec(
                    makeKable(table),
                    1,
                    bold = TRUE
                )
            )
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
                    1,
                    bold = TRUE
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
            corrTableCors <- corrTableCIs <- matrix(NA, nrow = nrow(corrTableRaw), ncol = ncol(corrTableRaw))
            corrTableComb <- rbind(corrTableCors, corrTableCIs)

            corrTableCors[lower.tri(corrTableCors)] <- kableExtra::cell_spec(
                sprintf("%.3f", corrTableRaw[lower.tri(corrTableRaw)]),
                color = textColor,
                background = ifelse(
                    CIs$p[lower.tri(CIs$p)] < input$sigLvl,
                    ifelse(
                        corrTableRaw[lower.tri(corrTableRaw)] >= 0,
                        goodColor,
                        badColor
                    ),
                    neutrColor
                )
            )
            diag(corrTableCors) <- 1

            corrTableCIs[lower.tri(corrTableCIs)] <- kableExtra::cell_spec(
                sprintf(
                    "[%.3f, %.3f]",
                    CIs$lowCI[lower.tri(CIs$lowCI)],
                    CIs$uppCI[lower.tri(CIs$uppCI)]
                ),
                color = textColor,
                background = ifelse(
                    CIs$p[lower.tri(CIs$p)] < input$sigLvl,
                    ifelse(
                        corrTableRaw[lower.tri(corrTableRaw)] >= 0,
                        goodColor,
                        badColor
                    ),
                    neutrColor
                )
            )
            diag(corrTableCIs) <- "-"

            corrTableComb[seq(1, nrow(corrTableComb), 2), ] <- corrTableCors
            corrTableComb[seq(2, nrow(corrTableComb), 2), ] <- corrTableCIs

            colnames(corrTableComb) <- input$itemCols
            rownames(corrTableComb) <- c(rbind(input$itemCols, "CI"))

            tagList(
                h4("Correlation Table with Confidence Intervals:"),
                HTML(kableExtra::column_spec(
                    makeKable(corrTableComb),
                    1,
                    bold = TRUE
                )),
                h5("Legend:"),
                HTML(makeKable(
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
            mvnUV$p <- ifelse(
                mvnUV$p < 0.001,
                "< 0.001",
                sprintf("%.3f", round(mvnUV$p, 3))
            )

            tagList(
                h4("Tests on Univariate Normality:"),
                HTML(makeKable(mvnUV, bootstrap_options = "basic"))
            )
        }
    })

    observe({
        if (class(mvnTestResultRaw())[1] == "list") {
            req(input$sigLvl)

            updateRadioButtons(session,
                               "estimator",
                               selected = ifelse(
                                   any(
                                       as.numeric(
                                           as.character(
                                               mvnTestResultRaw()$multivariateNormality[-3, "p value"]
                                           )
                                       ) < input$sigLvl
                                   ),
                                   "MLR",
                                   "ML"
                               ))
        }
    })

    output$mvnComment <- renderUI({
        mvnMV <- data.frame(Test = as.character(mvnTestResultRaw()$multivariateNormality$Test),
                            Statistic = as.numeric(as.character(mvnTestResultRaw()$multivariateNormality$Statistic)),
                            p = as.numeric(as.character(mvnTestResultRaw()$multivariateNormality$`p value`)),
                            Signif. = as.character(mvnTestResultRaw()$multivariateNormality$Result),
                            stringsAsFactors = F)[-3,]

        mvnMV$Signif. <- ifelse(mvnMV$p < input$sigLvl, "*", "")
        mvnMV$p <- ifelse(mvnMV$p < 0.001,
                          "< 0.001",
                          sprintf("%.3f", round(mvnMV$p, 3)))

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
                corrNegSigTag <- "WARNING: Insignificant and negative correlations found."
            } else if (any(ps >= input$sigLvl) && any(cors >= 0)) {
                corrNegSigCheck <- "?"
                corrNegSigColor <- "orange"
                corrNegSigTag <- "WARNING: Insignificant correlations found."
            } else if (all(ps < input$sigLvl) && any(cors < 0)) {
                corrNegSigCheck <- "&#10005;"
                corrNegSigColor <- "red"
                corrNegSigTag <- "WARNING: Negative correlations found."
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

            corrInd <- unlist(extractFitParameters(fittedDummyModel)[, c(2, 1, 3)])

            if (corrInd[3] < input$sigLvl) {
                corrIndCheck <- "&#10003;"
                corrIndColor <- "black"
                corrIndTag <- HTML(sprintf(
                    "(Test result: %s-&chi;&sup2; = %.3f, df = %i, p %s)",
                    input$estimator,
                    corrInd[1],
                    corrInd[2],
                    ifelse(corrInd[3] < 0.001,
                           "< 0.001",
                           sprintf("= %.3f", corrInd[3])))
                )
                output$isCorrInd <- reactive({TRUE})
            } else {
                corrIndCheck <- "&#10005;"
                corrIndColor <- "red"
                corrIndTag <- HTML(sprintf(
                    "(Test result: %s-&chi;&sup2; = %.3f, df = %i, p %s)",
                    input$estimator,
                    corrInd[1],
                    corrInd[2],
                    ifelse(corrInd[3] < 0.001,
                           "< 0.001",
                           sprintf("= %.3f", corrInd[3])))
                )
                output$isCorrInd <- reactive({TRUE})#({FALSE})
            }
        } else {
            corrIndCheck <- "&#10005;"
            corrIndColor <- "red"
            corrIndTag <- paste("WARNING/ERROR:", fittedDummyModel$message)

            output$isCorrInd <- reactive({TRUE})#({FALSE})
        }

        output$checks <- renderUI({
            req(userDataItems())

            tagList(
                div(style = paste0("color:", itemColor),
                    h5(HTML(paste("Number of Items:", itemCheck))),
                    itemTag
                ),
                div(style = paste0("color:", enoughObsColor),
                    h5(HTML(paste("Number of Observations:", enoughObsCheck))),
                    enoughObsTag
                ),
                div(style = paste0("color:", corrNegSigColor),
                    h5(HTML(paste("Item Correlations:", corrNegSigCheck))),
                    corrNegSigTag
                ),
                div(style = paste0("color:", corrIndColor),
                    h5(HTML(paste("Test on Correlative Independence:", corrIndCheck))),
                    corrIndTag
                )
            )
        })

        outputOptions(output, "isCorrInd", suspendWhenHidden = FALSE)
        outputOptions(output, "obsOk", suspendWhenHidden = FALSE)
        outputOptions(output, "oneItem", suspendWhenHidden = FALSE)
        outputOptions(output, "corrNegNoErr", suspendWhenHidden = FALSE)

        if (any(c(itemCheck, enoughObsCheck, corrNegSigCheck, corrIndCheck) != "&#10003;"))
            updateActionButton(session,
                               "goModels",
                               label = "Test the models (anyway)")
        else
            updateActionButton(session,
                               "goModels",
                               label = "Test the models")
    })

    ########################################################################################################################
    ## --------------------------------------------- Generate the models ------------------------------------------------ ##
    ########################################################################################################################
    observeEvent(input$goModels, {

        modelsToTest <- models[sapply(models, function(thisModel) input[[thisModel]])]

        appendTab(
            inputId = "navbar",
            tabPanel(
                "Model Comparison Tests",
                value = "panelModelTests",
                tabsetPanel(id = "compTabsets")
            ),
            select = TRUE
        )

        appendTab(
            inputId = "navbar",
            tabPanel(
                "Parameter Tables and Factor Scores",
                value = "panelParTables",
                tabsetPanel(id = "parTabsets")
            )
        )

        lapply(
            append(list(FALSE), if (input$groupCol != "no") input$groupCol),
            function(groupName) {

                # Try fitting and capture warning and error messages -------------------------------------------------------
                modelCodes <- makeModelCodes(inputData = userData(),
                                             itemCols = input$itemCols,
                                             group = groupName)

                if (isFALSE(groupName)) {
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
                } else {
                    fittedModelsWarns <- lapply(
                        modelCodes[modelsToTest],
                        FUN = function(model) {
                            tryCatch(cfa(model = model,
                                         data = userData(),
                                         meanstructure = TRUE,
                                         group = groupName,
                                         group.equal = c("loadings", "intercepts"),
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
                                             group = groupName,
                                             group.equal = c("loadings", "intercepts"),
                                             estimator = input$estimator),
                                         error = function(e) e)
                            )
                        }
                    )
                }

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
                fits <- do.call(rbind, lapply(fittedModelsWarns[goodModels], extractFitParameters))
                comps <- possComps[sapply(possComps, function(thisComp) input[[thisComp]])]

                succTable <- list()

                if (length(goodModels) > 1 & !identical(goodModels, c(teq = "teq", etp = "etp"))) {
                    if ("teq" %in% goodModels) {
                        succTable$teq <- do.call(
                            lavTestLRT,
                            args = c(object = fittedModelsWarns[[goodModels[1]]],
                                     ... = fittedModelsWarns[goodModels[-c(1, which(goodModels == "etp"))]])
                        )

                        rownames(succTable$teq) <- goodModels[which(goodModels != "etp")]
                    }
                    if ("etp" %in% goodModels) {
                        succTable$etp <- do.call(
                            lavTestLRT,
                            args = c(object = fittedModelsWarns[[goodModels[1 + (goodModels[1] == "teq")]]],
                                     ... = fittedModelsWarns[goodModels[-c(1 + (goodModels[1] == "teq"),
                                                                           which(goodModels == "teq"))]])
                        )

                        rownames(succTable$etp) <- goodModels[which(goodModels != "teq")]
                    }
                    if (!any(c("teq", "etp") %in% goodModels)) {
                        succTable$teq <- do.call(
                            lavTestLRT,
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
                    infCompTable$bic[lower.tri(diag(5), diag = TRUE)] <- "<span style=\"     color: lightgrey;\" >X</span>"

                # Generate Paramter Tables, Fits and Fit Tables ------------------------------------------------------------
                for (model in goodModels) {
                    local({
                        thisModel <- model
                        whichModel <- which(goodModels == thisModel)

                        # Write to chisq comp table ------------------------------------------------------------------------
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

                        # Some stuff to be edited later --------------------------------------------------------------------
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

                        # Create Tab ---------------------------------------------------------------------------------------
                        if (isFALSE(groupName)) {

                            # Factor Scores --------------------------------------------------------------------------------
                            output[[paste0(thisModel, "Scores")]] <- renderDataTable({
                                etaDf <- data.frame(
                                    n = 1:nrow(userData()),
                                    eta.hat = lavPredict(fittedModelsWarns[[thisModel]])
                                )

                                if (input$groupCol != "no") {
                                    etaDf[[input$groupCol]] <- userData()[, input$groupCol]

                                    etaDf <- etaDf[, c(1, 3, 2)]
                                }

                                etaDf
                            })

                            appendTab(
                                inputId = "parTabsets",
                                tabPanel(
                                    title = HTML(modelsLong[thisModel]),
                                    h4("Estimated Paramters with Standard Errors and Confidence Intervals:"),
                                    HTML(
                                        kableExtra::add_header_above(
                                            kableExtra::row_spec(
                                                kableExtra::column_spec(
                                                    makeKable(
                                                        extractParameters(
                                                            fittedModelsWarns[[thisModel]],
                                                            alpha = input$sigLvl
                                                        ),
                                                        col.names = c(
                                                            "Item",
                                                            "&lambda;&#x302;<sub>i</sub>",
                                                            "Est.", paste0(c("SE", "CI"),
                                                                           "<sub>",
                                                                           input$estimator,
                                                                           "</sub>"),
                                                            "Std. Est.", paste0(c("SE", "CI"),
                                                                                "<sub>",
                                                                                input$estimator,
                                                                                "</sub>"),
                                                            "&alpha;&#x302;<sub>i</sub>",
                                                            "Est.", paste0(c("SE", "CI"),
                                                                           "<sub>",
                                                                           input$estimator,
                                                                           "</sub>"),
                                                            "&sigma;&#x302;&sup2;<sub>&epsilon;<sub>i</sub></sub>",
                                                            "Est.", paste0(c("SE", "CI"),
                                                                           "<sub>",
                                                                           input$estimator,
                                                                           "</sub>"),
                                                            "R&#x302;<sub>i</sub>",
                                                            "Est.", paste0(c("SE", "CI"),
                                                                           "<sub>",
                                                                           input$estimator,
                                                                           "</sub>")
                                                        )
                                                    ),
                                                    1,
                                                    bold = TRUE),
                                                length(input$itemCols) + 1,
                                                bold = TRUE),
                                            c(" ",
                                              "Discrimination Parameters (Factor Loadings)" = 7,
                                              "Easiness Parameters" = 4,
                                              "Variances" = 4,
                                              "Reliabilities" = 4)
                                        )
                                    ),
                                    h4(HTML("Predicted Factor Scores (&eta;)")),
                                    dataTableOutput(paste0(thisModel, "Scores"))
                                ),
                                select = as.logical(whichModel == 1)
                            )
                        }
                    })
                }

                if (length(goodModels) > 0) {
                    # Hierarchical model comparison plot -------------------------------------------------------------------
                    output[[paste0("hierPlot", groupName)]] <- renderPlot({
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

                        ggplot(
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
                            aes(x = x, y = y, label = name)
                        ) + geom_text(parse = TRUE, fontface = "bold", size = 5) +
                            geom_segment(aes(x = xstarts, y = ystarts, xend = xends, yend = yends), size = 0.3) +
                            geom_label(aes(x = labelxs,
                                           y = labelys,
                                           label = ifelse(
                                               is.na(chisq),
                                               "No~Comparison",
                                               sprintf("'%s-'*Delta*chi^2==%.3f*','~Delta*df==%i*','~p%s",
                                                       input$estimator,
                                                       chisq,
                                                       df,
                                                       ifelse(pvalue < 0.001,
                                                              "<0.001",
                                                              sprintf("==%.3f", pvalue)))
                                           ),
                                           fill = c(pvalue < 0.05)),
                                       color = textColor,
                                       size = 4.5,
                                       parse = TRUE) +
                            scale_fill_manual(values = c(goodColor, badColor), na.value = neutrColor) +
                            guides(fill = FALSE) +
                            xlim(c(-4, 4)) +
                            coord_fixed() +
                            theme_void()
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
                                                      paste0(input$estimator, "-&Delta;&chi;&sup2;"),
                                                      "p",
                                                      "AIC",
                                                      "BIC",
                                                      paste0(input$estimator, "-CFI"))

                                rownames(hierTable) <- modelsAbbrev[rownames(hierTable)]

                                kableExtra::row_spec(
                                    kableExtra::column_spec(
                                        makeKable(hierTable),
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

                    colnames(combCompTable) <- rep(c("&Delta;df", paste0(input$estimator, "-&Delta;&chi;&sup2;")), 5)

                    headerNames <- c("", rep(2, 5))
                    names(headerNames) <- c("", modelsAbbrev)

                    # AIC comparison table ---------------------------------------------------------------------------------
                    dim(infCompTable$aic) <- dim(infCompTable$bic) <- c(5, 5)

                    rownames(combCompTable) <-
                        rownames(infCompTable$aic) <-
                        rownames(infCompTable$bic) <-
                        colnames(infCompTable$aic) <-
                        colnames(infCompTable$bic) <-
                        modelsAbbrev

                    # Put them in a tab ------------------------------------------------------------------------------------
                    appendTab(
                        inputId = "compTabsets",
                        tabPanel(
                            ifelse(isFALSE(groupName),
                                   "Singlegroup Comparison overview",
                                   "Multigroup Comparison overview"),
                            wellPanel(
                                h5(sprintf(
                                    "Lavaan status: %i warnings, %i errors.",
                                    sum(warns),
                                    sum(errs)
                                )),
                                lavErrsMsg,
                                lavWarnsMsg
                            ),
                            h4("Hierarchical model comparison plot:"),
                            plotOutput(paste0("hierPlot", groupName)),
                            h4("Hierarchical model comparison table:"),
                            HTML(paste0(
                                "<table align = \"center\", width = \"100%\"><tr><td>",
                                hierTables[[1]],
                                "</td><td>&nbsp;</td><td>",
                                hierTables[[2]],
                                "</td></tr></table>"
                            )),
                            h4("Fit index table"),
                            HTML(
                                kableExtra::column_spec(
                                    kableExtra::column_spec(
                                        makeKable(
                                            fits[, -c(9, 10)],
                                            col.names = c("df",
                                                          paste0(input$estimator, "-&chi;&sup2;"),
                                                          "p",
                                                          "RMSEA",
                                                          "p",
                                                          "95%-CI",
                                                          paste0(input$estimator, "-CFI"),
                                                          "SRMR")
                                        ),
                                        1,
                                        bold = TRUE
                                    ),
                                    c(4, 7),
                                    border_right = TRUE
                                )
                            ),
                            h4(HTML("&chi;&sup2;-Comparison Table:")),
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
                            ),
                            h4("AIC/BIC-Comparison Table:"),
                            HTML(paste0("<table align = \"center\", width = \"100%\">
                                <tr><td>
                                <table align = \"center\">
                                <tr><td><h5>AIC:</h5>",
                                        kableExtra::column_spec(
                                            makeKable(infCompTable$aic),
                                            1,
                                            bold = TRUE
                                        ),
                                        "</td></tr></table>
                                </td>
                                <td>&nbsp;</td>
                                <td><table align = \"center\">
                                <tr><td><h5>BIC:</h5>",
                                        kableExtra::column_spec(
                                            makeKable(infCompTable$bic),
                                            1,
                                            bold = TRUE
                                        ),
                                        "</td></tr></table>
                                </td></tr></table>")
                            )
                        ),
                        select = isFALSE(groupName)
                    )
                } else {
                    appendTab(
                        inputId = "compTabsets",
                        tabPanel(
                            ifelse(isFALSE(groupName),
                                   "No Singlegroup Models have been fitted",
                                   "No Multigroup Models have been fitted"),
                            wellPanel(
                                h5(sprintf(
                                    "Lavaan status: %i warnings, %i errors.",
                                    sum(warns),
                                    sum(errs)
                                )),
                                lavErrsMsg,
                                lavWarnsMsg
                            )
                        ),
                        select = isFALSE(groupName)
                    )
                    removeTab(inputId = "navbar",
                              target = "panelParTables")
                }
            }
        )

        # Print the error/warnings -----------------------------------------------------------------------------------------

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
