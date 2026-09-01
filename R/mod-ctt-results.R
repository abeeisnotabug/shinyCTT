## The results of a model run: the comparison page, the parameter tables, the factor scores
## and the model code.
##
## Called once per pass - once for the whole sample, once for the group-wise fit - which is
## what lets the two sets of results exist side by side without every id having "Mg" pasted
## on the end of it. The four UI functions share one id, because the four pages they fill are
## four tabs of one result.
##
## This is the CTT-specific tier: it knows the five models by name.

cttResultsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("page"))
}

cttParTablesUI <- function(id) {
  ns <- NS(id)

  fluidRow(uiOutput(ns("parTabset")))
}

cttFactorScoresUI <- function(id) {
  ns <- NS(id)

  fluidRow(uiOutput(ns("fsTabset")))
}

cttModelCodeUI <- function(id) {
  ns <- NS(id)

  fluidRow(uiOutput(ns("mcTabset")))
}

## Arguments:
##   fit        : a reactive holding one pass out of modelFitsRV() - the fits and the
##                settings they were made with. Everything here starts from it, and req()s
##                it, so the pages stay blank until the models have been fitted.
##   sigLvl     : a reactive holding the significance level
##   rmseaCiLvl : a reactive holding the confidence level of the RMSEA interval
##   the four colours : the app's, passed in the same way every make*Table() call takes them
cttResultsServer <- function(id, fit, sigLvl, rmseaCiLvl,
                             goodColor, badColor, neutrColor, textColor) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # The five models: their names, their labels, and where the hierarchy plot draws them.
    family <- cttModelFamily()

    models <- family$names
    modelsLong <- family$long
    modelsAbbrev <- family$abbrev
    modelTestDF <- family$plot


    ## The fit indices of every model that fitted ----
    # The confidence level of the RMSEA interval is a display choice, so these are worked
    # out again whenever the user changes it. No model is refitted.
    fitIndices <- reactive(
      do.call(rbind, lapply(fit()$fittedModels[fit()$goodModels],
                            extractFitIndices,
                            rmseaCiLevel = rmseaCiLvl())))

    ## The three comparison matrices ----
    # One cell per pair of models: the model's own chi-square on the diagonal, the
    # difference against an earlier model to the left of it, and the same layout again for
    # AIC and BIC. The chi-square cells are coloured by significance, so all of this is
    # redrawn when the significance level changes.
    compMatrices <- reactive({
      fits <- fitIndices()
      fittedModels <- fit()$fittedModels
      goodModels <- fit()$goodModels
      comps <- fit()$comps

      # Cells are addressed by pair, "etetko" being the ess. tau-equivalent model against
      # the tau-congeneric one. Anything still empty at the end prints as a blank cell.
      cellNames <- outer(models, models, paste0)

      chisqCells <- dfCells <- aicCells <- bicCells <-
        stats::setNames(rep(NA_character_, 25), cellNames)

      # Comparing A with B is the same test as comparing B with A, so only the diagonal
      # and the cells left of it are used. Those start as a grey X and are overwritten
      # below wherever there is something to write.
      chisqCells[lower.tri(diag(5), diag = TRUE)] <-
        aicCells[lower.tri(diag(5), diag = TRUE)] <-
        bicCells[lower.tri(diag(5), diag = TRUE)] <- "<span style=\"color: lightgrey;\" >X</span>"

      for (thisModel in goodModels) {

        whichModel <- which(goodModels == thisModel)
        thisModelStr <- paste0(thisModel, thisModel)

        ### write to diag(chisq comp table) ----
        if (fits[thisModel, "pvalue"] < sigLvl()) {
          sigAddon <- "*"
          sigColor <- badColor

          if (fits[thisModel, "pvalue"] < 0.01)
            sigAddon <- paste0(sigAddon, "*")

          if (fits[thisModel, "pvalue"] < 0.001)
            sigAddon <- paste0(sigAddon, "*")

        } else {

          sigAddon <- ""
          sigColor <- goodColor
        }

        chisqCells[thisModelStr] <-
          kableExtra::cell_spec(
            sprintf(paste0("%.2f", sigAddon), fits[thisModel, "chisq"]),
            background = sigColor,
            color = textColor,
            italic = TRUE)

        dfCells[thisModelStr] <-
          kableExtra::cell_spec(
            sprintf("%i", fits[thisModel, "df"]),
            background = sigColor,
            color = textColor,
            italic = TRUE)

        ### write to AIC/BIC comp table ----
        aicCells[thisModelStr] <-
          kableExtra::cell_spec(
            sprintf("%.1f", fits[thisModel, "aic"]),
            color = textColor,
            background = neutrColor)

        bicCells[thisModelStr] <-
          kableExtra::cell_spec(
            sprintf("%.1f", fits[thisModel, "bic"]),
            color = textColor,
            background = neutrColor)

        #### if there is more than one good model ----
        if (whichModel > 1) {

          aicDiffs <- fits[thisModel, "aic"] - fits[1:(whichModel - 1), "aic"]
          bicDiffs <- fits[thisModel, "bic"] - fits[1:(whichModel - 1), "bic"]

          aicCells[paste0(thisModel, rownames(fits)[1:(whichModel - 1)])] <-
            kableExtra::cell_spec(
              sprintf(ifelse(aicDiffs < 0, "%.1f", "+%.1f"), aicDiffs),
              color = textColor,
              background = ifelse(aicDiffs < 0, goodColor, badColor))

          bicCells[paste0(thisModel, rownames(fits)[1:(whichModel - 1)])] <-
            kableExtra::cell_spec(
              sprintf(ifelse(bicDiffs < 0, "%.1f", "+%.1f"), bicDiffs),
              color = textColor,
              background = ifelse(bicDiffs < 0, goodColor, badColor))
        }

        ### write to lower.tri(chisq comp table) ----
        compsWithThisModel <- substring(
          text = comps[grep(thisModel, substr(comps, 1, 3))],
          first = 4,
          last = 6)

        compsWithThisModel <- compsWithThisModel[compsWithThisModel %in% goodModels]
        names(compsWithThisModel) <- compsWithThisModel

        fitCompsWithThisModel <- sapply(
          compsWithThisModel,
          function(thisComp) {
            tmpTbl <- lavaan::lavTestLRT(fittedModels[[thisModel]], fittedModels[[thisComp]])
            unlist(tmpTbl[2, c("Chisq diff", "Df diff", "Pr(>Chisq)")])
        })

        for (thisComp in compsWithThisModel) {

          if (fitCompsWithThisModel["Pr(>Chisq)", thisComp] < sigLvl()) {
            sigAddon <- "*"
            sigColor <- badColor

            if (fitCompsWithThisModel["Pr(>Chisq)", thisComp] < 0.01)
              sigAddon <- paste0(sigAddon, "*")

            if (fitCompsWithThisModel["Pr(>Chisq)", thisComp] < 0.001)
              sigAddon <- paste0(sigAddon, "*")

          } else {

            sigAddon <- ""
            sigColor <- goodColor
          }

          thisModelCompStr <- paste0(thisModel, thisComp)

          chisqCells[thisModelCompStr] <- kableExtra::cell_spec(
            sprintf(paste0("+%.2f", sigAddon), fitCompsWithThisModel["Chisq diff", thisComp]),
            background = sigColor,
            color = textColor)

          dfCells[thisModelCompStr] <- kableExtra::cell_spec(
            sprintf("+%i", fitCompsWithThisModel["Df diff", thisComp]),
            background = sigColor,
            color = textColor)
        }
      }

      ### the df and the chi-square of one pair go in two columns side by side ----
      combCompTable <- matrix(NA, nrow = 5, ncol = 10)
      combCompTable[, seq(1, 10, 2)] <- matrix(dfCells, nrow = 5, ncol = 5)
      combCompTable[, seq(2, 10, 2)] <- matrix(chisqCells, nrow = 5, ncol = 5)

      rownames(combCompTable) <- modelsAbbrev
      colnames(combCompTable) <- rep(
        c("&Delta;df", paste0(fit()$estimatorName, "-&Delta;&chi;&sup2;")),
        times = 5)

      list(
        chisq = combCompTable,
        aic = matrix(aicCells, nrow = 5, ncol = 5,
                     dimnames = list(modelsAbbrev, modelsAbbrev)),
        bic = matrix(bicCells, nrow = 5, ncol = 5,
                     dimnames = list(modelsAbbrev, modelsAbbrev)))
    })

    ## the page holding the comparison of all models ----
    # Only the boxes and their headings. Each table is an output of its own below, so
    # changing the significance level redraws the table without rebuilding the page - which
    # would close any legend the user has open.
    output$page <- renderUI({

      fit <- fit()

      #### message if warnings ----
      if (sum(fit$warns) > 0) {

        lavWarnsMsg <- tagList(
          h6("The following models produced warnings:"),

          cbind(paste0(modelsLong[fit$warnModels], ":&emsp;"),
                sapply(fit$fittedModels[fit$warnModels],
                       function(model) attr(model, "shinyCTTwarning")$message)) %>%
            kableExtra::kbl(row.names = FALSE, escape = FALSE) %>%
            kableExtra::column_spec(column = 1, bold = TRUE) %>%
            HTML() %>%
            div(style = "color:orange")
        ) # tagList

      } else {
        lavWarnsMsg <- NULL
      }

      #### message if errors ----
      if (sum(fit$errs) > 0) {

        lavErrsMsg <- tagList(
          h6("The following models produced errors:"),

          cbind(paste0(modelsLong[fit$errModels], ":&emsp;"),
                sapply(fit$fittedModels[fit$errModels],
                       function(model) model$message)) %>%
            kableExtra::kbl(row.names = FALSE, escape = FALSE) %>%
            kableExtra::column_spec(column = 1, bold = TRUE) %>%
            HTML() %>%
            div(style = "color:red")
          ) # tagList

      } else {
        lavErrsMsg <- NULL
      }

      lavStatus <- if (sum(fit$warns) > 0 || sum(fit$errs) > 0) {
        wellPanel(
          h5(sprintf("Lavaan status: %i warnings, %i errors.",
                     sum(fit$warns),
                     sum(fit$errs))),
          lavErrsMsg,
          lavWarnsMsg)
      }

      #### if there are no good models, the status is the whole page ----
      if (length(fit$goodModels) == 0) return(tagList(lavStatus))

      #### otherwise, one box per comparison ----
      fluidPage(

        if (!is.null(lavStatus)) fluidRow(lavStatus),

        fluidRow(
          shinydashboard::box(
            title = "Hierarchical model comparison plot:",
            width = 12,
            plotOutput(ns("hierPlot")))),

        fluidRow(
          shinydashboard::box(
            title = "Hierarchical model comparison table:",
            width = 12,
            htmlOutput(ns("hierTable")),
            actionLink(ns("showLegendHierTable"), "Show/hide legend"),
            conditionalPanel("input.showLegendHierTable % 2 == 1",
                             htmlOutput(ns("hierTableLegend")),
                             ns = ns))),

        fluidRow(
          shinydashboard::box(
            title = "Fit index table",
            width = 12,
            htmlOutput(ns("fitsTable")),
            br(),
            actionLink(ns("showLegendFitIndexTable"), "Show/hide legend"),
            conditionalPanel("input.showLegendFitIndexTable % 2 == 1",
                             htmlOutput(ns("fitsTableLegend")),
                             ns = ns))),

        fluidRow(
          shinydashboard::box(
            title = HTML("&chi;&sup2;-comparison table:"),
            width = 12,
            htmlOutput(ns("combCompTable")),
            br(),
            actionLink(ns("showLegendCombCompTable"), "Show/hide legend"),
            conditionalPanel("input.showLegendCombCompTable % 2 == 1",
                             htmlOutput(ns("combCompTableLegend")),
                             ns = ns))),

        fluidRow(
          shinydashboard::box(
            title = "AIC/BIC-comparison table:",
            width = 12,
            htmlOutput(ns("infCompTable")),
            actionLink(ns("showLegendInfCompTable"), "Show/hide legend"),
            conditionalPanel("input.showLegendInfCompTable % 2 == 1",
                             htmlOutput(ns("infCompTableLegend")),
                             ns = ns)))

      ) # fluidPage
    })

    ## hierarchical model comparison plot ----
    output$hierPlot <- renderPlot({

      succTable <- fit()$succTable
      goodModels <- fit()$goodModels

      req(length(goodModels) > 0)

      modelNumbs <- which(models %in% goodModels)

      chisqs <- dfs <- pvalues <- rep(NA, 5)

      names(chisqs) <-
        names(dfs) <-
        names(pvalues) <- c("tkoete", "eteteq", "eteetp", "teqtpa", "etptpa")

      if (!is.null(succTable$teq)) {
        teqNames <- paste0(rownames(succTable$teq)[1:(nrow(succTable$teq) - 1)],
                           rownames(succTable$teq)[2:nrow(succTable$teq)])

        chisqs[teqNames] <- succTable$teq[-1, "Chisq diff"]
        dfs[teqNames] <- succTable$teq[-1, "Df diff"]
        pvalues[teqNames] <- succTable$teq[-1, "Pr(>Chisq)"]
      }

      if (!is.null(succTable$etp)) {
        etpNames <- paste0(rownames(succTable$etp)[1:(nrow(succTable$etp) - 1)],
                           rownames(succTable$etp)[2:nrow(succTable$etp)])

        chisqs[etpNames] <- succTable$etp[-1, "Chisq diff"]
        dfs[etpNames] <- succTable$etp[-1, "Df diff"]
        pvalues[etpNames] <- succTable$etp[-1, "Pr(>Chisq)"]
      }

      modelTestDF$chisq <- chisqs
      modelTestDF$df <- dfs
      modelTestDF$pvalue <- pvalues

      ### ggplot code ----
      ggplot2::ggplot(modelTestDF,
                      ggplot2::aes(x = .data$x, y = .data$y, label = .data$name)) +

        ggplot2::geom_text(parse = TRUE, fontface = "bold", size = 5) +
        ggplot2::geom_segment(
          ggplot2::aes(x = .data$xstarts, y = .data$ystarts,
                       xend = .data$xends, yend = .data$yends),
          linewidth = 0.3) +

        ggplot2::geom_label(
          ggplot2::aes(
            x = .data$labelxs,
            y = .data$labelys,

            label = ifelse(
              is.na(.data$chisq),
              yes = "No~Comparison",
              no = sprintf(
                "'%s-'*Delta*chi^2==%.3f*','~Delta*df==%i*','~p%s",
                fit()$estimatorName, # %s
                .data$chisq, # %.3f
                .data$df, # %i
                ifelse(.data$pvalue < 0.001, "<0.001", sprintf("==%.3f", .data$pvalue)))),

            fill = c("nsig", "sig")[c(.data$pvalue < sigLvl()) + 1]), # aes

          color = textColor,
          size = 4.5,
          parse = TRUE) + # geom_label

        ggplot2::scale_fill_manual(
          values = c("nsig" = goodColor, "sig" = badColor),
          na.value = neutrColor) +

        ggplot2::guides(fill = "none") +
        ggplot2::xlim(c(-4, 4)) +
        ggplot2::coord_fixed() +
        ggplot2::theme_void()

    }) # renderPlot

    ## hierarchical model comparison table ----
    # Two tables side by side: one down each branch of the hierarchy.
    output$hierTable <- renderUI({

      fit <- fit()
      req(length(fit$goodModels) > 0)

      hierTables <- lapply(

        c("teq", "etp"),

        function(model) {
          if (!is.null(fit$succTable[[model]])) {

            succTableTmp <- as.data.frame(fit$succTable[[model]])
            makeHierTable(succTableTmp, fitIndices()[rownames(succTableTmp), "cfi"],
                          fit$estimatorName, sigLvl(),
                          goodColor, badColor, neutrColor, textColor, modelsAbbrev)
          } else {
            NULL
          }
        } # function(model)
      ) # lapply

      paste0(
        "<table align = \"center\", width = \"100%\"><tr><td>",
        hierTables[[1]],
        "</td><td>&nbsp;</td><td>",
        hierTables[[2]],
        "</td></tr></table>") %>%
        HTML()
    })

    ## fit index table ----
    output$fitsTable <- renderUI({
      req(length(fit()$goodModels) > 0)

      HTML(makeFitsTable(fitIndices(), fit()$estimatorName, sigLvl(), rmseaCiLvl(),
                         goodColor, badColor, neutrColor, textColor,
                         modelsAbbrev))
    })

    ## chi-square comparison table ----
    output$combCompTable <- renderUI({
      req(length(fit()$goodModels) > 0)

      # One header spanning the two columns of each model.
      headerNames <- c(1, rep(2, 5))
      names(headerNames) <- c(" ", modelsAbbrev)

      makeKable(compMatrices()$chisq, bold_cols = 1) %>%
        kableExtra::add_header_above(headerNames, escape = FALSE) %>%
        HTML()
    })

    ## AIC/BIC comparison table ----
    output$infCompTable <- renderUI({
      req(length(fit()$goodModels) > 0)

      paste0(
        "<table align = \"center\", width = \"100%\"> <tr><td>
          <table align = \"center\"> <tr><td>
            <h5>AIC:</h5>",

        makeKable(compMatrices()$aic, bold_cols = 1),

        "</td></tr></table>
      </td>
      <td>&nbsp;</td>
      <td>
        <table align = \"center\"> <tr><td>
          <h5>BIC:</h5>",

        makeKable(compMatrices()$bic, bold_cols = 1),

      "</td></tr></table>
    </td></tr></table>") %>%
      HTML()
    })

    ## the four legends ----
    # Each names the significance level it is describing, so each follows it.
    output$hierTableLegend <- renderUI(
      makeLegend("hierTables", fit()$estimatorName, sigLvl(),
                 goodColor, badColor, neutrColor, textColor))

    output$fitsTableLegend <- renderUI(
      makeLegend("fitIndexTable", fit()$estimatorName, sigLvl(),
                 goodColor, badColor, neutrColor, textColor,
                 rmseaCiLvl = rmseaCiLvl()))

    output$combCompTableLegend <- renderUI(
      makeLegend("combCompTable", fit()$estimatorName, sigLvl(),
                 goodColor, badColor, neutrColor, textColor))

    output$infCompTableLegend <- renderUI(
      makeLegend("infCompTable", fit()$estimatorName, sigLvl(),
                 goodColor, badColor, neutrColor, textColor))

    ## the three tab strips ----
    # Built whole from the models that fitted, rather than a tab being added per model:
    # the models can be fitted again, and adding to the strip would give two tabs per
    # model the second time round. tabBox() takes its panels one by one, so do.call()
    # hands it the list.
    output$parTabset <- renderUI({
      panels <- lapply(
        fit()$goodModels,
        function(thisModel) tabPanel(
          title = HTML(modelsLong[thisModel]),
          htmlOutput(ns(paste0(thisModel, "ParTable")))))

      do.call(
        shinydashboard::tabBox,
        c(list(id = ns("parTabsetTab"),
               title = "Estimated parameters",
               width = 12),
          unname(panels)))
    })

    output$fsTabset <- renderUI({
      panels <- lapply(
        fit()$goodModels,
        function(thisModel) tabPanel(
          title = HTML(modelsLong[thisModel]),
          sidebarLayout(

            sidebarPanel(
              h4("Download Predicted Factor Scores as CSV"),

              textInput(
                ns(paste0(thisModel, "Filename")),
                "Filename:",
                sprintf("%s_%s_factorscores.csv", fit()$dataName, thisModel)),

              hr(),

              radioButtons(
                ns(paste0(thisModel, "Sep")),
                "Separator",
                choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                selected = ","),

              radioButtons(
                ns(paste0(thisModel, "Dec")),
                "Decimal Separator",
                choices = c(Comma = ",", Dot = "."),
                selected = "."),

              hr(),

              downloadButton(
                ns(paste0(thisModel, "ScoresDownload")),
                "Download Factor Scores") %>%

                div(align = "center"),

              width = 3
            ), # sidebarPanel

            mainPanel(
              h4("Data Overview"),
              DT::dataTableOutput(ns(paste0(thisModel, "Scores"))))

          ) # sidebarLayout
        )) # tabPanel, lapply

      do.call(
        shinydashboard::tabBox,
        c(list(id = ns("fsTabsetTab"),
               title = HTML("Predicted factor scores (&eta;&#x302;)"),
               width = 12),
          unname(panels)))
    })

    output$mcTabset <- renderUI({
      panels <- lapply(
        fit()$goodModels,
        function(thisModel) tabPanel(
          title = HTML(modelsLong[thisModel]),
          h5("The following R code can be used to fit this model with lavaan:"),
          verbatimTextOutput(ns(paste0(thisModel, "Code")))))

      do.call(
        shinydashboard::tabBox,
        c(list(id = ns("mcTabsetTab"),
               title = "Model code",
               width = 12),
          unname(panels)))
    })

    ## one set of outputs per model ----
    # All five models get theirs, whether or not they were chosen. The req() at the top of
    # each holds back the ones that were not fitted; the tab strips above only show tabs
    # for the ones that were.
    lapply(models, function(thisModel) {

      parTableStr <- paste0(thisModel, "ParTable")
      scoresStr <- paste0(thisModel, "Scores")
      scoresDLStr <- paste0(thisModel, "ScoresDownload")
      scoresDLFileStr <- paste0(thisModel, "Filename")
      sepStr <- paste0(thisModel, "Sep")
      decStr <- paste0(thisModel, "Dec")
      codeStr <- paste0(thisModel, "Code")

      ### parameter table ----
      output[[parTableStr]] <- renderUI({

        fit <- fit()
        req(thisModel %in% fit$goodModels)

        fittedModel <- fit$fittedModels[[thisModel]]
        thisModelsNgroups <- fittedModel@Data@ngroups

        parTableWithCIs <- makeParTableWithCIs(fittedModel, fit$estimatorName,
                                               sigLvl(), fit$itemCols,
                                               thisModelsNgroups)

        #### modify parameter tables if there are groups ----
        if (!isFALSE(fit$groupName)) {
          for (i in 1:thisModelsNgroups) {

            groupRowHeaders <- sprintf("Group: %s", fittedModel@Data@group.label)

            parTableWithCIs <- kableExtra::group_rows(
              parTableWithCIs,
              group_label = groupRowHeaders[i],
              start_row = (i - 1) * (length(fit$itemCols) + 1) + 1,
              end_row = i * (length(fit$itemCols) + 1),
              label_row_css = "background-color: #666; color: #fff;")
          }
        }

        HTML(parTableWithCIs)
      })

      ### factor scores ----
      output[[scoresStr]] <- DT::renderDataTable({

        req(thisModel %in% fit()$goodModels)

        getPredictedScores(
          fit()$fittedModels[[thisModel]],
          fit()$groupValues)

      }, options = list(pageLength = 10))

      output[[scoresDLStr]] <- downloadHandler(
        filename = function() input[[scoresDLFileStr]],
        content = function(file) {

          utils::write.table(
            getPredictedScores(
              fit()$fittedModels[[thisModel]],
              fit()$groupValues),

            file = file,
            sep = input[[sepStr]],
            dec = input[[decStr]],
            row.names = FALSE)
        },
        contentType = "text/csv")

      ### model code ----
      output[[codeStr]] <- renderPrint({

        fit <- fit()
        req(thisModel %in% fit$goodModels)

        cat(
          makeRCode(
            dataSource = fit$dataSource,
            groupCol = fit$groupCol,
            groups = fit$groups,
            modelCode = fit$modelCodes[[thisModel]],
            estimator = fit$estimator,
            missingMethod = fit$missingMethod,
            isSubset = fit$isSubset,
            model = thisModel,
            isMg = !isFALSE(fit$groupName)))
      })
    })  })
}
