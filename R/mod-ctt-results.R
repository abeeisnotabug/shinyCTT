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
cttResultsServer <- function(id, fit, sigLvl, rmseaCiLvl) {
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
    #
    # Each table comes out as three matrices of the same shape: what every cell says, how
    # every cell is rated, and which cells hold a model's own fit rather than a difference
    # (those are set in italics). The colours are not here - ratingStyle() has them.
    compMatrices <- reactive({
      fits <- fitIndices()
      fittedModels <- fit()$fittedModels
      goodModels <- fit()$goodModels
      comps <- fit()$comps

      # Cells are addressed by pair, "etetko" being the ess. tau-equivalent model against
      # the tau-congeneric one.
      cellNames <- outer(models, models, paste0)

      chisqText <- dfText <- aicText <- bicText <-
        stats::setNames(rep("", 25), cellNames)

      chisqRating <- dfRating <- aicRating <- bicRating <-
        stats::setNames(rep(NA_character_, 25), cellNames)

      ownFit <- stats::setNames(rep(FALSE, 25), cellNames)

      # Comparing A with B is the same test as comparing B with A, so only the diagonal
      # and the cells left of it are used. Those start as a grey X and are overwritten
      # below wherever there is something to write.
      usedCells <- lower.tri(diag(5), diag = TRUE)
      greyX <- "<span style=\"color: lightgrey;\">X</span>"

      chisqText[usedCells] <- dfText[usedCells] <-
        aicText[usedCells] <- bicText[usedCells] <- greyX

      for (thisModel in goodModels) {

        whichModel <- which(goodModels == thisModel)
        thisModelStr <- paste0(thisModel, thisModel)

        ### write to diag(chisq comp table) ----
        # The model's own chi-square, with a star per significance level passed.
        modelP <- fits[thisModel, "pvalue"]

        sigAddon <- paste(rep("*", sum(modelP < c(sigLvl(), 0.01, 0.001))), collapse = "")
        sigRating <- if (modelP < sigLvl()) "bad" else "good"

        chisqText[thisModelStr] <- sprintf(paste0("%.2f", sigAddon), fits[thisModel, "chisq"])
        dfText[thisModelStr] <- sprintf("%i", fits[thisModel, "df"])

        chisqRating[thisModelStr] <- dfRating[thisModelStr] <- sigRating
        ownFit[thisModelStr] <- TRUE

        ### write to AIC/BIC comp table ----
        aicText[thisModelStr] <- sprintf("%.1f", fits[thisModel, "aic"])
        bicText[thisModelStr] <- sprintf("%.1f", fits[thisModel, "bic"])

        aicRating[thisModelStr] <- bicRating[thisModelStr] <- "neutral"

        #### if there is more than one good model ----
        if (whichModel > 1) {

          earlierModels <- paste0(thisModel, rownames(fits)[1:(whichModel - 1)])

          aicDiffs <- fits[thisModel, "aic"] - fits[1:(whichModel - 1), "aic"]
          bicDiffs <- fits[thisModel, "bic"] - fits[1:(whichModel - 1), "bic"]

          aicText[earlierModels] <- sprintf(ifelse(aicDiffs < 0, "%.1f", "+%.1f"), aicDiffs)
          bicText[earlierModels] <- sprintf(ifelse(bicDiffs < 0, "%.1f", "+%.1f"), bicDiffs)

          aicRating[earlierModels] <- ifelse(aicDiffs < 0, "good", "bad")
          bicRating[earlierModels] <- ifelse(bicDiffs < 0, "good", "bad")
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

          compP <- fitCompsWithThisModel["Pr(>Chisq)", thisComp]

          sigAddon <- paste(rep("*", sum(compP < c(sigLvl(), 0.01, 0.001))), collapse = "")
          thisModelCompStr <- paste0(thisModel, thisComp)

          chisqText[thisModelCompStr] <- sprintf(
            paste0("+%.2f", sigAddon), fitCompsWithThisModel["Chisq diff", thisComp])

          dfText[thisModelCompStr] <- sprintf(
            "+%i", fitCompsWithThisModel["Df diff", thisComp])

          chisqRating[thisModelCompStr] <- dfRating[thisModelCompStr] <-
            if (compP < sigLvl()) "bad" else "good"
        }
      }

      ### the df and the chi-square of one pair go in two columns side by side ----
      # Ten columns, two per model, so the keys carry the model name and which of the two
      # they are: "tkoDf", "tkoChisq", "eteDf", ...
      pairColumns <- as.vector(rbind(paste0(models, "Df"), paste0(models, "Chisq")))

      # Recycling dfPart into the full 5 x 10 first keeps the matrix the same type as what
      # goes into it - these are called with text, with ratings and with TRUE/FALSE.
      interleave <- function(dfPart, chisqPart) {
        both <- matrix(dfPart, nrow = 5, ncol = 10)
        both[, seq(1, 10, 2)] <- matrix(dfPart, nrow = 5, ncol = 5)
        both[, seq(2, 10, 2)] <- matrix(chisqPart, nrow = 5, ncol = 5)
        dimnames(both) <- list(modelsAbbrev, pairColumns)
        both
      }

      squareMatrix <- function(cells) {
        matrix(cells, nrow = 5, ncol = 5, dimnames = list(modelsAbbrev, models))
      }

      list(
        chisq = list(
          shown = interleave(dfText, chisqText),
          ratings = interleave(dfRating, chisqRating),
          ownFit = interleave(ownFit, ownFit)),
        aic = list(
          shown = squareMatrix(aicText),
          ratings = squareMatrix(aicRating),
          ownFit = squareMatrix(ownFit)),
        bic = list(
          shown = squareMatrix(bicText),
          ratings = squareMatrix(bicRating),
          ownFit = squareMatrix(ownFit)))
    })

    ## drawing one comparison table ----
    # `cells` is what compMatrices() gave back for one of the three. `headers` names each
    # column; `groups`, when given, is the band above them - the combined table puts two
    # columns under each model's name.
    drawCompTable <- function(cells, headers, groups = NULL) {

      columns <- lapply(names(headers), function(column) {
        reactable::colDef(
          name = headers[[column]],
          html = TRUE,
          style = function(value, index) {
            c(ratingStyle(cells$ratings[index, column]),
              if (cells$ownFit[index, column]) list(fontStyle = "italic"))
          })
      })

      reactable::reactable(
        as.data.frame(cells$shown, stringsAsFactors = FALSE),
        rownames = TRUE,
        columns = c(
          list(.rownames = reactable::colDef(
            name = "", html = TRUE, style = list(fontWeight = "bold"))),
          stats::setNames(columns, names(headers))),
        columnGroups = groups,
        sortable = FALSE,
        pagination = FALSE,
        compact = TRUE)
    }

    ## what lavaan said about a model ----
    # The orange and the red box above the results: one row per model, its name and what
    # lavaan said about it.
    messageTable <- function(modelNames, messages) {
      tags$table(
        class = "table table-condensed",
        style = "width: auto;",

        tags$tbody(Map(function(modelName, message) {
          tags$tr(
            tags$td(HTML(paste0(modelName, ":&emsp;")), style = "font-weight: bold;"),
            tags$td(message))
        }, modelNames, messages)))
    }

    ## the page holding the comparison of all models ----
    # Only the boxes and their headings. Each table is an output of its own below, so
    # changing the significance level redraws the table without rebuilding the page - which
    # would close any legend the user has open.
    output$page <- renderUI({

      fit <- fit()

      #### message if warnings ----
      if (sum(fit$warns) > 0) {

        lavWarnsMsg <- tagList(
          h6(tr("The following models produced warnings:")),

          div(
            style = "color:orange",
            messageTable(
              modelsLong[fit$warnModels],
              vapply(fit$fittedModels[fit$warnModels],
                     function(model) attr(model, "shinyCTTwarning")$message, character(1))))
        ) # tagList

      } else {
        lavWarnsMsg <- NULL
      }

      #### message if errors ----
      if (sum(fit$errs) > 0) {

        lavErrsMsg <- tagList(
          h6(tr("The following models produced errors:")),

          div(
            style = "color:red",
            messageTable(
              modelsLong[fit$errModels],
              vapply(fit$fittedModels[fit$errModels],
                     function(model) model$message, character(1))))
          ) # tagList

      } else {
        lavErrsMsg <- NULL
      }

      lavStatus <- if (sum(fit$warns) > 0 || sum(fit$errs) > 0) {
        wellPanel(
          h5(sprintf(tr("Lavaan status: %i warnings, %i errors."),
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
            title = tr("Hierarchical model comparison plot:"),
            width = 12,
            plotOutput(ns("hierPlot")))),

        fluidRow(
          shinydashboard::box(
            title = tr("Hierarchical model comparison table:"),
            width = 12,
            htmlOutput(ns("hierTable")),
            actionLink(ns("showLegendHierTable"), tr("Show/hide legend")),
            conditionalPanel("input.showLegendHierTable % 2 == 1",
                             htmlOutput(ns("hierTableLegend")),
                             ns = ns))),

        fluidRow(
          shinydashboard::box(
            title = tr("Fit index table"),
            width = 12,
            reactable::reactableOutput(ns("fitsTable")),
            br(),
            actionLink(ns("showLegendFitIndexTable"), tr("Show/hide legend")),
            conditionalPanel("input.showLegendFitIndexTable % 2 == 1",
                             htmlOutput(ns("fitsTableLegend")),
                             ns = ns))),

        fluidRow(
          shinydashboard::box(
            title = HTML(tr("&chi;&sup2;-comparison table:")),
            width = 12,
            reactable::reactableOutput(ns("combCompTable")),
            br(),
            actionLink(ns("showLegendCombCompTable"), tr("Show/hide legend")),
            conditionalPanel("input.showLegendCombCompTable % 2 == 1",
                             htmlOutput(ns("combCompTableLegend")),
                             ns = ns))),

        fluidRow(
          shinydashboard::box(
            title = tr("AIC/BIC-comparison table:"),
            width = 12,
            htmlOutput(ns("infCompTable")),
            actionLink(ns("showLegendInfCompTable"), tr("Show/hide legend")),
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

            # Plotmath text: parse = TRUE below means a "~" here is read as a space, not a
            # literal tilde, so a German translation must use "~" the same way.
            label = ifelse(
              is.na(.data$chisq),
              yes = tr("No~Comparison"),
              no = sprintf(
                "'%s-'*Delta*chi^2==%.3f*','~Delta*df==%i*','~p%s",
                fit()$estimatorName, # %s
                .data$chisq, # %.3f
                .data$df, # %i
                ifelse(.data$pvalue < 0.001, "<0.001", sprintf("==%.3f", .data$pvalue)))),

            fill = c("nsig", "sig")[c(.data$pvalue < sigLvl()) + 1]), # aes

          color = cttColors()$text,
          size = 4.5,
          parse = TRUE) + # geom_label

        ggplot2::scale_fill_manual(
          values = c("nsig" = cttColors()$good, "sig" = cttColors()$bad),
          na.value = cttColors()$neutral) +

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
                          fit$estimatorName, sigLvl(), modelsAbbrev)
          } else {
            NULL
          }
        } # function(model)
      ) # lapply

      # Side by side, each taking half the width.
      fluidRow(
        column(width = 6, hierTables[[1]]),
        column(width = 6, hierTables[[2]]))
    })

    ## fit index table ----
    output$fitsTable <- reactable::renderReactable({
      req(length(fit()$goodModels) > 0)

      makeFitsTable(fitIndices(), fit()$estimatorName, sigLvl(), rmseaCiLvl(), modelsAbbrev)
    })

    ## chi-square comparison table ----
    output$combCompTable <- reactable::renderReactable({
      req(length(fit()$goodModels) > 0)

      cells <- compMatrices()$chisq

      # Two columns per model, headed by Delta-df and the estimator's Delta-chi-squared...
      headers <- stats::setNames(
        rep(c(tr("&Delta;df"), paste0(fit()$estimatorName, tr("-&Delta;&chi;&sup2;"))), 5),
        colnames(cells$shown))

      # ...with the model's own name in a band above each pair.
      groups <- lapply(models, function(model) {
        reactable::colGroup(
          name = modelsAbbrev[[model]],
          html = TRUE,
          columns = paste0(model, c("Df", "Chisq")))
      })

      drawCompTable(cells, headers, unname(groups))
    })

    ## AIC/BIC comparison table ----
    # The two tables side by side, each under its own heading. One column per model.
    output$infCompTable <- renderUI({
      req(length(fit()$goodModels) > 0)

      headers <- stats::setNames(modelsAbbrev[models], models)

      fluidRow(
        column(
          width = 6,
          h5(tr("AIC:")),
          drawCompTable(compMatrices()$aic, headers)),
        column(
          width = 6,
          h5(tr("BIC:")),
          drawCompTable(compMatrices()$bic, headers)))
    })

    ## the four legends ----
    # Each names the significance level it is describing, so each follows it.
    output$hierTableLegend <- renderUI(
      makeLegend("hierTables", fit()$estimatorName, sigLvl()))

    output$fitsTableLegend <- renderUI(
      makeLegend("fitIndexTable", fit()$estimatorName, sigLvl(),
                 rmseaCiLvl = rmseaCiLvl()))

    output$combCompTableLegend <- renderUI(
      makeLegend("combCompTable", fit()$estimatorName, sigLvl()))

    output$infCompTableLegend <- renderUI(
      makeLegend("infCompTable", fit()$estimatorName, sigLvl()))

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
               title = tr("Estimated parameters"),
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
              h4(tr("Download Predicted Factor Scores as CSV")),

              textInput(
                ns(paste0(thisModel, "Filename")),
                tr("Filename:"),
                sprintf("%s_%s_factorscores.csv", fit()$dataName, thisModel)),

              hr(),

              radioButtons(
                ns(paste0(thisModel, "Sep")),
                tr("Separator"),
                choiceNames = list(tr("Comma"), tr("Semicolon"), tr("Tab")),
                choiceValues = c(",", ";", "\t"),
                selected = ","),

              radioButtons(
                ns(paste0(thisModel, "Dec")),
                tr("Decimal Separator"),
                choiceNames = list(tr("Comma"), tr("Dot")),
                choiceValues = c(",", "."),
                selected = "."),

              hr(),

              downloadButton(
                ns(paste0(thisModel, "ScoresDownload")),
                tr("Download Factor Scores")) %>%

                div(align = "center"),

              width = 3
            ), # sidebarPanel

            mainPanel(
              h4(tr("Data Overview")),
              DT::dataTableOutput(ns(paste0(thisModel, "Scores"))))

          ) # sidebarLayout
        )) # tabPanel, lapply

      do.call(
        shinydashboard::tabBox,
        c(list(id = ns("fsTabsetTab"),
               title = HTML(tr("Predicted factor scores (&eta;&#x302;)")),
               width = 12),
          unname(panels)))
    })

    output$mcTabset <- renderUI({
      panels <- lapply(
        fit()$goodModels,
        function(thisModel) tabPanel(
          title = HTML(modelsLong[thisModel]),
          h5(tr("The following R code can be used to fit this model with lavaan:")),
          verbatimTextOutput(ns(paste0(thisModel, "Code")))))

      do.call(
        shinydashboard::tabBox,
        c(list(id = ns("mcTabsetTab"),
               title = tr("Model code"),
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

        # One table per group, each under its own heading, the same shape as the
        # covariance matrix and the descriptive statistics. A fit with no group column has
        # exactly one group, and then there is no heading to put above it.
        groupLabels <- fittedModel@Data@group.label

        tagList(lapply(seq_len(thisModelsNgroups), function(group) {
          tagList(
            if (!isFALSE(fit$groupName))
              h5(sprintf(tr("Group: %s"), groupLabels[group])),

            makeParTableWithCIs(fittedModel, fit$estimatorName, sigLvl(), fit$itemCols,
                                group))
        }))
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
