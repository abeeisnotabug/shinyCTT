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

    # Preparation 2: Reactive Values ---------------------------------------------------------------------------------------
    mvnTestResult <- reactiveValues(
        raw = NULL,
        estimator = "ML"
    )

    checks <- reactiveValues(
        oneItem = list(
            check = FALSE,
            symb = NULL,
            col = NULL,
            tag = NULL
        ),
        obsOk = list(
            check = FALSE,
            symb = NULL,
            col = NULL,
            tag = NULL
        ),
        corrOk = list(
            check = FALSE,
            symb = NULL,
            col = NULL,
            tag = NULL
        ),
        corrIndOk = list(
            check = FALSE,
            symb = NULL,
            col = NULL,
            tag = NULL
        )
    )

    ########################################################################################################################
    ## ------------------------------------------------- Data input ----------------------------------------------------- ##
    ########################################################################################################################
    output$objectsInWorkspace <- renderUI({
        selectInput("objectFromWorkspace",
                    "Choose data object from Workspace:",
                    Filter(function(object) !is.null(dim(get(object))) && typeof(get(object)) != "character",
                           ls(envir = globalenv())))
    })

    userDataRaw <- reactive({
        if (input$source == "CSV") {
            req(input$CSVFile)

            userDataTmp <- read.csv(file = input$CSVFile$datapath,
                                    header = input$header,
                                    sep = input$sep,
                                    quote = input$quote,
                                    stringsAsFactors = FALSE)
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

        data.frame(
            userDataTmp,
            stringsAsFactors = FALSE
        )
    })

    userData <- reactive({
        req(userDataRaw())
        req(input$groupCol)

        if (input$groupCol != "noGroupSelected" &&
            input$groupCol %in% colnames(userDataRaw())) {
            req(input$groups)

            subset(
                userDataRaw(),
                userDataRaw()[, input$groupCol] %in% input$groups
            )
        } else {
            userDataRaw()
        }
    })

    userDataItems <- reactive({
        req(userData())

        if (length(input$itemCols) > 1)
            tryCatch(userData()[, input$itemCols],
                     warning = function(w) NULL,
                     error = function(e) NULL)
        else
            NULL
    })

    userDataGroup <- reactive({
        req(userData())

        if (input$groupCol %in% colnames(userData()))
            userData()[, input$groupCol]
        else
            NULL
    })

    output$userDataExists <- reactive(!is.null(userData()))
    outputOptions(output, "userDataExists", suspendWhenHidden = FALSE)

    output$dataOverview <- renderDataTable(userDataRaw())

    output$itemColsChooser <- renderUI({
        possibleItemColumns <- colnames(userDataRaw())[sapply(userDataRaw(), is.numeric)]

        tagList(
            checkboxGroupInput("itemCols",
                               "Select the item columns",
                               choices = possibleItemColumns,
                               selected = possibleItemColumns,
                               inline = TRUE),
            if (length(possibleItemColumns) == 0)
                helpText("No numeric columns found.")
        )
    })

    output$groupColChooser <- renderUI({
        selectInput("groupCol",
                    "Select the group column",
                    choices = c("No group column selected" = "noGroupSelected",
                                colnames(userDataRaw())[!(colnames(userDataRaw()) %in% input$itemCols)]))
    })

    output$groupChooser <- renderUI({
        req(input$groupCol)

        if (input$groupCol != "noGroupSelected" && input$groupCol %in% colnames(userDataRaw())) {
            possibleGroups <- unique(userDataRaw()[, input$groupCol])

            if (any(c(table(userDataRaw()[, input$groupCol])) == 1)) {
                groupWarning <- "There are groups with only one observation,
                                 you may have selected an item as group column."
                possibleGroups <- NULL
            } else {
                groupWarning <- ""
            }

            tagList(
                checkboxGroupInput("groups",
                                   "Select which groups to include",
                                   choices = possibleGroups,
                                   selected = possibleGroups,
                                   inline = TRUE),
                helpText(groupWarning),
                conditionalPanel(
                    condition = "input.groups.length > 1",
                    checkboxInput(
                        "doMg",
                        "Perform Multigroup Tests",
                        value = FALSE
                    )
                )
            )
        }
    })

    observeEvent(input$sigLvl, {
        if ((input$sigLvl < 0 | input$sigLvl > 1) && !is.na(input$sigLvl)) {
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
                        if (
                            (
                                substr(thisComp, 1, 3) == thisModel &&
                                input[[substr(thisComp, 4, 6)]]
                            ) || (
                                substr(thisComp, 4, 6) == thisModel &&
                                input[[substr(thisComp, 1, 3)]]
                            )
                        )
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

        nHeader <- c(1, 4)
        names(nHeader) <- c(
            " ",
            sprintf("n<sub>all</sub> = %i", nrow(userDataItems()))
        )

        tagList(
            h4("Descriptive Statistics"),
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
    })

    output$covMat <- renderUI({
        req(userDataItems())

        table <- cov(userDataItems())
        table[upper.tri(table)] <- NA

        tagList(
            h4("Covariance Matrix"),
            HTML(
                kableExtra::column_spec(
                    shinyCTT:::makeKable(table),
                    1,
                    bold = TRUE
                )
            )
        )
    })

    corrTableWithCIsRaw <- reactive({
        req(userDataItems())

        list(cor = tryCatch(
            cor(userDataItems()),
            warning = function(w) NULL,
            error = function(e) NULL
        ),
             test = tryCatch(
                 corrplot::cor.mtest(userDataItems(),
                                     conf.level = (1 - input$sigLvl)),
                 warning = function(w) w,
                 error = function(e) e
             )
        )
    })

    output$corrTableWithCIs <- renderUI({
        if (class(corrTableWithCIsRaw()$test)[1] == "list") {
            tagList(
                h4("Correlation Table"),
                HTML(
                    kableExtra::column_spec(
                        shinyCTT:::makeKable(
                            shinyCTT:::makeCorrTableWithCIs(
                                corrTableWithCIsRaw(),
                                goodColor,
                                badColor,
                                neutrColor,
                                textColor,
                                input$sigLvl,
                                input$itemCols
                            ),
                            bootstrap_options = c("condensed", "striped")
                        ),
                        1,
                        bold = TRUE
                    )
                ),
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
        }
    })

    # Correlative independence ---------------------------------------------------------------------------------------------
    corrIndRaw <- reactive({
        req(userDataItems())
        req(mvnTestResult$estimator)

        dummyModel <- paste(
            sprintf("%s ~ 1", colnames(userDataItems())),
            collapse = "\n"
        )

        tryCatch(
            lavaan::cfa(
                model = dummyModel,
                data = userData(),
                estimator = mvnTestResult$estimator
            ),
            warning = function(w) w,
            error = function(e) e
        )
    })

    output$corrInd <- renderUI({
        req(userData())

        if (class(corrIndRaw())[1] == "lavaan") {
            corrInd <- unlist(shinyCTT:::extractFitParameters(corrIndRaw())[, c(2, 1, 3)])

            if (corrInd[3] < input$sigLvl) {
                tagList(
                    h4("Test on Correlative Independence"),
                    HTML(
                        sprintf(
                            "The hypothesis that all correlations are equal to
                            zero has to be discarded on a significance level of
                            %s (%s-&chi;&sup2; = %.3f, df = %i, p %s).",
                            input$sigLvl,
                            mvnTestResult$estimator,
                            corrInd[1],
                            corrInd[2],
                            ifelse(
                                corrInd[3] < 0.001,
                                "< 0.001",
                                sprintf("= %.3f", corrInd[3]))
                        )
                    )
                )
            } else {
                tagList(
                    h4("Test on Correlative Independence"),
                    HTML(
                        sprintf(
                            "The hypothesis that all correlations are equal to
                            zero can be maintained on a significance level of
                            %s (%s-&chi;&sup2; = %.3f, df = %i, p %s).",
                            input$sigLvl,
                            mvnTestResult$estimator,
                            corrInd[1],
                            corrInd[2],
                            ifelse(
                                corrInd[3] < 0.001,
                                "< 0.001",
                                sprintf("= %.3f", corrInd[3]))
                        )
                    )
                )
            }
        } else {
            tagList(
                h4("Test on Correlative Independence"),
                div(style = paste0("color:red"),
                    HTML(
                        paste("There was an ERROR/WARNING:",
                              corrIndRaw()$message)
                    )
                )
            )
        }
    })

    # Calculate test on MVN ------------------------------------------------------------------------------------------------
    observeEvent(userDataItems(), {
        mvnTestResult$raw <- tryCatch(
            MVN::mvn(userDataItems()),
            warning = function(w) w,
            error = function(e) e
        )

        req(mvnTestResult$raw)

        if (class(mvnTestResult$raw$multivariateNormality) == "data.frame") {
            mvnTestResult$estimator <- ifelse(
                any(
                    as.numeric(
                        as.character(
                            mvnTestResult$raw$multivariateNormality[-3, "p value"]
                        )
                    ) < input$sigLvl
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
    })

    observeEvent(input$estimator, {
        mvnTestResult$estimator <- input$estimator
    })

    output$mvnTableUV <- renderUI({
        req(userDataItems())

        if (class(mvnTestResult$raw)[1] == "list") {
            mvnUV <- data.frame(Test = as.character(mvnTestResult$raw$univariateNormality$Test),
                                Item = as.character(mvnTestResult$raw$univariateNormality$Variable),
                                Statistic = as.numeric(mvnTestResult$raw$univariateNormality$Statistic),
                                p = suppressWarnings(as.numeric(mvnTestResult$raw$univariateNormality$`p value`)),
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
                HTML(shinyCTT:::makeKable(mvnUV, bootstrap_options = "basic"))
            )
        } else {
            tagList(
                h4("Test on Multivariate Normality:"),
                div(style = paste0("color:red"),
                    HTML(paste("There was an ERROR/WARNING:", mvnTestResult$raw$message))
                )
            )
        }
    })

    output$mvnComment <- renderUI({
        req(userDataItems())

        if (class(mvnTestResult$raw$multivariateNormality) == "data.frame") {
            mvnMV <- data.frame(Test = as.character(mvnTestResult$raw$multivariateNormality$Test),
                                Statistic = as.numeric(as.character(mvnTestResult$raw$multivariateNormality$Statistic)),
                                p = as.numeric(as.character(mvnTestResult$raw$multivariateNormality$`p value`)),
                                Signif. = as.character(mvnTestResult$raw$multivariateNormality$Result),
                                stringsAsFactors = F)[-3,]

            mvnMV$Signif. <- ifelse(mvnMV$p < input$sigLvl, "*", "")
            mvnMV$p <- ifelse(
                mvnMV$p < 0.001,
                "< 0.001",
                sprintf("%.3f", round(mvnMV$p, 3))
            )

            if ("*" %in% mvnMV$Signif.) {
                tagList(
                    h4("Test on Multivariate Normality:"),
                    sprintf("At least one of the hypotheses that Mardia's Skewness statistic
                            or Mardias' Kurtosis statistic matches one of a
                            normal distribution has to be discarded on a significance
                            level of %s. Test result:", input$sigLvl),
                    HTML(shinyCTT:::makeKable(mvnMV, bootstrap_options = "basic")),
                    HTML("It is thus recommended to continue with the <b>Robust Maximum Likelihood (MLR)</b> estimator.")
                )
            } else {
                tagList(
                    h4("Test on Multivariate Normality:"),
                    sprintf("The hypotheses that Mardia's Skewness statistic
                        and Mardias' Kurtosis statistic match those of a
                        normal distribution can be maintained on a significance
                        level of %s. Test result:", input$sigLvl),
                    HTML(shinyCTT:::makeKable(mvnMV, bootstrap_options = "basic")),
                    HTML("It is thus recommended to continue with the <b>Maximum Likelihood (ML)</b> estimator.")
                )
            }
        }
    })

    ########################################################################################################################
    ## ----------------------------------------- Check data before analysis --------------------------------------------- ##
    ########################################################################################################################
    # Check the number of items --------------------------------------------------------------------------------------------
    observeEvent(input$itemCols, {
        checks$oneItem <- switch(
            as.character(length(input$itemCols)),
            "1" = list(
                check = FALSE,
                symb = "&#10005;",
                col = "red",
                tag = sprintf("ERROR: Only one or no item selected. No analysis possible.")
            ),
            "2" = list(
                check = TRUE,
                symb = "(&#10003;)",
                col = "orange",
                tag = HTML("WARNING: Only two items selected. The &tau;-kongeneric and
                            the essentially &tau;-equivalt model can not be tested.")
            ),
            "3" = list(
                check = TRUE,
                symb = "(&#10003;)",
                col = "orange",
                tag = HTML("WARNING: Only three items selected. The &tau;-kongeneric model can not be tested.")
            ),
            list(
                check = TRUE,
                symb = "&#10003;",
                col = "black",
                tag = NULL
            )
        )

        for (model in models)
            updateCheckboxInput(
                session,
                model,
                value = switch(
                    as.character(length(input$itemCols)),
                    "0" = FALSE,
                    "1" = FALSE,
                    "2" = !(model %in% c("tko", "ete")),
                    "3" = model != "tko",
                    TRUE
                )
            )

        output$oneItem <- reactive(checks$oneItem$check)
        outputOptions(output, "oneItem", suspendWhenHidden = FALSE)
    })

    # Check the number of observations -------------------------------------------------------------------------------------
    observeEvent(list(userData(), input$doMg), {
        req(userDataItems())

        checks$obsOk <- if (nrow(userData()) < ncol(userDataItems())) {
            list(
                check = FALSE,
                symb = "&#10005;",
                col = "red",
                tag = "ERROR: There are fewer observations than items."
            )
        } else {
            if (input$groupCol == "noGroupSelected") {
                list(
                    check = TRUE,
                    symb = "&#10003;",
                    col = "black",
                    tag = NULL
                )
            } else {
                if (any(sapply(split(userData(), userDataGroup()), nrow) < ncol(userDataItems())) &&
                    input$doMg) {
                    list(
                        check = FALSE,
                        symb = "&#10005;",
                        col = "red",
                        tag = "ERROR: There are fewer observations than items in some groups."
                    )
                } else {
                    list(
                        check = TRUE,
                        symb = "&#10003;",
                        col = "black",
                        tag = NULL
                    )
                }
            }
        }

        output$obsOk <- reactive(checks$obsOk$check)
        outputOptions(output, "obsOk", suspendWhenHidden = FALSE)
    })

    # Check for correlations (neg/insig) -----------------------------------------------------------------------------------
    observeEvent(corrTableWithCIsRaw(), {
        checks$corrOk <- if (class(corrTableWithCIsRaw()$test)[1] == "list") {
            if (any(corrTableWithCIsRaw()$test$p >= input$sigLvl)) {
                if (any(corrTableWithCIsRaw()$cor < 0)) {
                    list(
                        check = TRUE,
                        symb = "?",
                        col = "orange",
                        tag = "WARNING: Insignificant and negative correlations found."
                    )
                } else {
                    list(
                        check = TRUE,
                        symb = "?",
                        col = "orange",
                        tag = "WARNING: Insignificant correlations found."
                    )
                }
            } else {
                if (any(corrTableWithCIsRaw()$cor < 0)) {
                    list(
                        check = TRUE,
                        symb = "&#10005;",
                        col = "red",
                        tag = "WARNING: Negative correlations found."
                    )
                } else {
                    list(
                        check = TRUE,
                        symb = "&#10003;",
                        col = "black",
                        tag = NULL
                    )
                }
            }
        } else {
            list(
                check = FALSE,
                symb = "&#10005;",
                col = "red",
                tag = paste("WARNING/ERROR:", corrTableWithCIsRaw()$test$message)
            )
        }
    })

    # Check for correlative independence -----------------------------------------------------------------------------------
    observeEvent(corrIndRaw(), {
        if (class(corrIndRaw())[1] == "lavaan") {
            corrInd <- unlist(shinyCTT:::extractFitParameters(corrIndRaw())[, c(2, 1, 3)])

            checks$corrIndOk <- if (corrInd[3] < input$sigLvl) {
                list(
                    check = TRUE,
                    symb = "&#10003;",
                    col = "black",
                    tag = NULL
                )
            } else {
                list(
                    check = FALSE,
                    symb = "&#10005;",
                    col = "red",
                    tag = NULL
                )
            }
        } else {
            checks$corrIndOk <- list(
                check = FALSE,
                symb = "&#10005;",
                col = "red",
                tag = paste("WARNING/ERROR:", corrIndRaw()$message)
            )
        }
    })

    # Create the output object ---------------------------------------------------------------------------------------------
    output$oneItemCheck <- renderUI({
        req(userData())

        div(style = paste0("color:", checks$oneItem$col),
            h5(HTML(paste("Number of Items:", checks$oneItem$symb))),
            checks$oneItem$tag
        )
    })

    output$checksUI <- renderUI({
        req(userDataItems())

        tagList(
            div(style = paste0("color:", checks$obsOk$col),
                h5(HTML(paste("Number of Observations:", checks$obsOk$symb))),
                checks$obsOk$tag
            ),
            div(style = paste0("color:", checks$corrOk$col),
                h5(HTML(paste("Item Correlations:", checks$corrOk$symb))),
                checks$corrOk$tag
            ),
            div(style = paste0("color:", checks$corrIndOk$col),
                h5(HTML(paste("Test on Correlative Independence:", checks$corrIndOk$symb))),
                checks$corrIndOk$tag
            )
        )
    })

    # Add descr. stats for groups if a group column is specified -----------------------------------------------------------
    observeEvent(input$groupCol, {
        if (input$groupCol != "noGroupSelected") {
            output$mgDescrTable <- renderUI({
                req(userDataGroup())

                mgDescrTableList <- lapply(
                    unique(userDataGroup()),
                    function(group) t(
                        apply(
                            subset(
                                userDataItems(),
                                userDataGroup() == group
                            ),
                            2,
                            function(col)
                                c(Mean = mean(col), SD = sd(col),
                                  Skew = moments::skewness(col),
                                  Excess = moments::kurtosis(col) - 3)
                        )
                    )
                )

                descrGroupHeader <- c(1, rep(4, length(unique(userDataGroup()))))
                names(descrGroupHeader) <- c(
                    " ",
                    sprintf(
                        "Group: %s (n<sub>%s</sub> = %i)",
                        unique(userDataGroup()),
                        unique(userDataGroup()),
                        c(table(userDataGroup()))[as.character(unique(userDataGroup()))]
                    )
                )

                tagList(
                    h4("Multigroup Descriptive Statistics"),
                    HTML(
                        kableExtra::column_spec(
                            kableExtra::add_header_above(
                                kableExtra::column_spec(
                                    shinyCTT:::makeKable(do.call(cbind, mgDescrTableList)),
                                    1,
                                    bold = TRUE
                                ),
                                header = descrGroupHeader,
                                escape = FALSE
                            ),
                            1:max(1, length(unique(userDataGroup())) - 1) * 4 + 1,
                            border_right = ifelse(
                                length(unique(userDataGroup())) > 1,
                                "1px solid lightgrey",
                                FALSE
                            )
                        )
                    )
                )
            })

            output$mgCovMat <- renderUI({
                req(userDataGroup())

                mgCovMatList <- lapply(
                    unique(userDataGroup()),
                    function(group)
                        cov(
                            subset(
                                userDataItems(),
                                userDataGroup() == group
                            )
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
                    "Group: %s",
                    unique(userDataGroup())
                )

                for (i in 1:length(unique(userDataGroup())))
                    mgCovMatTable <- kableExtra::group_rows(
                        mgCovMatTable,
                        group_label = groupRowHeaders[i],
                        start_row = (i - 1) * length(input$itemCols) + 1,
                        end_row = i * length(input$itemCols),
                        label_row_css = "background-color: #666; color: #fff;"
                    )

                tagList(
                    h4("Multigroup Covariance Matrices:"),
                    HTML(
                        mgCovMatTable
                    )
                )
            })

            output$mgCorrTableTagList <- renderUI({
                req(userDataGroup())

                mgCorrTableList <- lapply(
                    unique(userDataGroup()),
                    function(group)
                        shinyCTT:::makeCorrTableWithCIs(
                            list(
                                cor = suppressWarnings(cor(
                                    subset(
                                        userDataItems(),
                                        userDataGroup() == group
                                    )
                                )),
                                test = corrplot::cor.mtest(
                                    subset(
                                        userDataItems(),
                                        userDataGroup() == group
                                    ),
                                    conf.level = (1 - input$sigLvl)
                                )
                            ),
                            goodColor,
                            badColor,
                            neutrColor,
                            textColor,
                            input$sigLvl,
                            input$itemCols
                        )
                )

                mgCorrTable <- kableExtra::column_spec(
                    shinyCTT:::makeKable(do.call(rbind, mgCorrTableList), bootstrap_options = c("condensed", "striped")),
                    1,
                    bold = TRUE
                )

                groupRowHeaders <- sprintf(
                    "Group: %s",
                    unique(userDataGroup())
                )

                for (i in 1:length(unique(userDataGroup())))
                    mgCorrTable <- kableExtra::group_rows(
                        mgCorrTable,
                        group_label = groupRowHeaders[i],
                        start_row = (i - 1) * length(input$itemCols) * 2 + 1,
                        end_row = i * length(input$itemCols) * 2,
                        label_row_css = "background-color: #666; color: #fff;"
                    )

                tagList(
                    h4("Multigroup Correlation Tables"),
                    HTML(mgCorrTable),
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
            })
        } else {
            output$mgDescrTable <- renderUI({tagList()})
            output$mgCovMat <- renderUI({tagList()})
            output$mgCorrTableTagList <- renderUI({tagList()})
        }
    })

    observeEvent(list(input$groupCol, input$groups), {
        updateCheckboxInput(
            session,
            "doMg",
            value = ifelse(
                input$groupCol != "noGroupSelected" &&
                    !any(c(table(userDataRaw()[, input$groupCol])) == 1) &&
                    length(input$groups) > 1,
                TRUE,
                FALSE
            )
        )
    })

    ########################################################################################################################
    ## --------------------------------------------- Generate the models ------------------------------------------------ ##
    ########################################################################################################################
    output$testing <- renderUI(helpText("The models are being tested. This may take a few seconds."))

    observeEvent(input$goModels, {
        modelsToTest <- models[sapply(models, function(thisModel) input[[thisModel]])]

        appendTab(
            inputId = "navbar",
            tabPanel(
                "Model Comparison Tests",
                value = "panelModelTests",
                tabsetPanel(id = "compTabsets", type = "pills")
            ),
            select = TRUE
        )

        appendTab(
            inputId = "navbar",
            tabPanel(
                "Parameter Tables and Factor Scores",
                value = "panelParTables",
                tabsetPanel(id = "parTabsets", type = "pills")
            )
        )

        lapply(
            append(list(FALSE), if (input$doMg) input$groupCol),
            function(groupName) {

                # Try fitting and capture warning and error messages -------------------------------------------------------
                modelCodes <- shinyCTT:::makeModelCodes(inputData = userData(),
                                                        itemCols = input$itemCols,
                                                        group = groupName)

                if (isFALSE(groupName)) {
                    fittedModelsWarns <- lapply(
                        modelCodes[modelsToTest],
                        FUN = function(model) {
                            tryCatch(lavaan::cfa(model = model,
                                                 data = userData(),
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
                                                     data = userData(),
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
                                                 data = userData(),
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
                                                     data = userData(),
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
                appendTab(
                    inputId = "parTabsets",
                    tabPanel(
                        ifelse(isFALSE(groupName),
                               "Singlegroup",
                               "Multigroup"),
                        tabsetPanel(
                            id = paste0("parTabsetTab", c("Mg")[!isFALSE(groupName)])
                        )
                    ),
                    select = isFALSE(groupName)
                )

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
                            ]] <<- renderDataTable(
                                shinyCTT:::getPredictedScores(fittedModelsWarns[[thisModel]], userDataGroup())
                            )

                        output[[
                            paste0(thisModel, "ScoresDownload", c("Mg")[!isFALSE(groupName)])
                            ]] <<- downloadHandler(
                                filename = function() {
                                    input[[paste0(thisModel, "Filename", c("Mg")[!isFALSE(groupName)])]]
                                },
                                content = function(file) {
                                    write.table(
                                        shinyCTT:::getPredictedScores(fittedModelsWarns[[thisModel]], userDataGroup()),
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
                                    if (length(unique(userData()[, input$groupCol])) <
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
                                h4("Estimated Paramters"),
                                HTML(parTableWithCIs),
                                h4("Additional Information"),
                                tabsetPanel(
                                    tabPanel(
                                        "Model Code",
                                        h5("The following R code can be used to fit this model with lavaan:"),
                                        verbatimTextOutput(paste0(thisModel, "Code", c("Mg")[!isFALSE(groupName)]))
                                    ),
                                    tabPanel(
                                        HTML("Predicted Factor Scores (&eta;&#x302;)"),
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
                                                dataTableOutput(
                                                    paste0(thisModel, "Scores", c("Mg")[!isFALSE(groupName)])
                                                )
                                            )
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
                    appendTab(
                        inputId = "compTabsets",
                        tabPanel(
                            ifelse(isFALSE(groupName),
                                   "Singlegroup",
                                   "Multigroup"),
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
                            ),
                            h4(HTML("&chi;&sup2;-Comparison Table:")),
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
                            ),
                            h4("AIC/BIC-Comparison Table:"),
                            HTML(paste0("<table align = \"center\", width = \"100%\">
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

        # Write the selected values ----------------------------------------------------------------------------------------
        output$selectedOptions <- renderUI({
            selectedData <- paste(
                "The following data was used:<br>",
                switch(input$source,
                       "Workspace" = sprintf("Object \"%s\" from workspace", input$objectFromWorkspace),
                       "CSV" = sprintf("CSV-File \"%s\"", input$CSVFile$name),
                       "SPSS" = sprintf("SPSS-File \"%s\"", input$SPSSFile$name))
            )

            selectedItems <- paste("The following items have been chosen:<br>",
                                   paste(input$itemCols, collapse = ", "))

            selectedGroup <- if (input$groupCol == "noGroupSelected")
                "No multigroup tests have been performed."
            else
                paste("Multigroup tests have been performed with group column", sprintf("\"%s\"", input$groupCol))

            selectedEstimator <- sprintf("The %s estimator has been chosen.", mvnTestResult$estimator)

            selectedSigLvl <- paste("All tests have been performed on a significance level of", input$sigLvl)

            tagList(
                h5("Selected options:"),
                HTML(selectedData),
                hr(),
                HTML(selectedItems),
                hr(),
                HTML(selectedGroup),
                hr(),
                HTML(selectedEstimator),
                hr(),
                HTML(selectedSigLvl),
                hr(),
                helpText("The models have been tested.")
            )
        })

        output$testing <- renderUI(tagList())
    })
}
