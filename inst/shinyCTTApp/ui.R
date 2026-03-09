shinydashboard::dashboardPage(
  # dashboardHeader ----
  shinydashboard::dashboardHeader(
    title = "shinyCTT",
    shinydashboard::dropdownMenuOutput("infoMenu")
  ),
  # dashboardSidebar ----
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenuOutput("dataMenuOut")
  ),
  # dashboardBody ----
  shinydashboard::dashboardBody(
    shinyCTT:::dashboardthemeFU,

    ## modify box look ----
    tags$head(
      tags$style(".checkbox-inline {margin: 0 !important;}"),
      tags$style(HTML(".navbar-custom-menu>.navbar-nav>li>.dropdown-menu {width:600px;}")),
      tags$style(".small-box.bg-green { background-color: #99CC00 !important; color: #FFFFFF !important; }"),
      tags$style(".small-box.bg-blue { background-color: #003F8A !important; color: #FFFFFF !important; }")),

    shinyjs::useShinyjs(),

    ## tabItems parent ----
    shinydashboard::tabItems(

      ### tabItem dataSelectionTab ----
      shinydashboard::tabItem(
        tabName = "dataSelectionTab",
        fluidRow(
          column(
            width = 3,
            shinydashboard::box(
              width = NULL,
              selectInput("source", "1a. Choose source of data",
                    choices = c("Workspace", "CSV", "SPSS"))),
            shinydashboard::box(
              width = NULL,
              conditionalPanel(
                condition = "input.source == 'Workspace'",
                uiOutput("objectsInWorkspace")),
              conditionalPanel(
                condition = "input.source == 'CSV'",
                fileInput("CSVFile", "1b. Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
                checkboxInput("header", "Header", TRUE),
                radioButtons("sep", "Separator",
                       choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                       selected = ","),
                radioButtons("quote", "Quote",
                       choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                       selected = '"')),
              conditionalPanel(
                condition = "input.source == 'SPSS'",
                fileInput("SPSSFile", "1b. Choose SPSS File",
                      multiple = FALSE,
                      accept = c(".sav",
                           ".zsav",
                           ".por")))),
            shinydashboard::box(
              width = NULL,
              actionButton("dataSelectButton", "Select", width = "100%"))),
          column(
            width = 9,
            shinydashboard::box(
              width = NULL,
              title = "Raw data:",
              DT::dataTableOutput("dataOverview")))
        ) # fluidRow
      ), # tabItem

      ### tabItem subsetSelectionTab ----
      shinydashboard::tabItem(
        tabName = "subsetSelectionTab",

        #### subsetSelectionTab first row info boxes ----
        fluidRow(
          shinydashboard::valueBoxOutput("itemInfoBox"),
          shinydashboard::valueBoxOutput("groupInfoBox"),
          shinydashboard::valueBoxOutput("naInfoBox")),

        #### subsetSelectionTab second row choosers ----
        fluidRow(
          column(
            width = 4,
            shinydashboard::box(
              width = NULL,
              uiOutput("itemColsChooser")),
            shinydashboard::box(
              width = NULL,
              uiOutput("groupColChooser"),
              conditionalPanel(
                condition = "input.groupCol != 'noGroupSelected'",
                uiOutput("groupChooser"))),
            shinydashboard::box(
              width = NULL,
              # subset of items
              actionButton("subsetSelectButton", "Select", width = "100%"))),
          column(
            width = 4,
            shinydashboard::box(
              width = NULL,
              title = "Observations:",
              htmlOutput("obsTable")),
            shinydashboard::box(
              width = NULL,
              title = "Observations per group:",
              htmlOutput("obsPerGroupTable"))),
          column(
            width = 4,
            shinydashboard::box(
              width = NULL,
              title = "Missing values per column:",
              htmlOutput("naTable")))
        ) # fluidRow
      ), # tabItem

      ### tabItem statisticsTab ----
      shinydashboard::tabItem(
        tabName = "statisticsTab",
        fluidRow(
          htmlOutput("descrBox"),
          htmlOutput("histBox")),
        fluidRow(
          htmlOutput("covMatBox"))),

      ### tabItem corrTab ----
      shinydashboard::tabItem(
        tabName = "corrTab",
        fluidRow(
          column(
            width = 4,
            shinydashboard::box(
              width = NULL,
              title = "Test on Correlative Independence:",
              radioButtons(
                "corrIndEst",
                "Choose the estimator for this test:",
                choices = c("Maximum Likelihood" = "ML",
                      "Robust Maximum Likelihood" = "MLR"),
                selected = "ML"),
              numericInput(
                "corrIndSL",
                "Enter the significance level for this test:",
                value = 0.05,
                min = 0,
                max = 1,
                step = 0.001),
              htmlOutput("corrInd")),
            shinydashboard::box(
              width = NULL,
              title = "Correlation Table with Confidence Intervals:",
              radioButtons(
                "corrTabNA",
                "Choose how to handle missing values:",
                choices = c("Use pairwise complete observations" = "pairwise.complete.obs",
                      "Use only complete observations" = "complete.obs"),
                selected = "pairwise.complete.obs"),
              numericInput(
                "corrTabSL",
                "Enter the significance level for the correlation tests:",
                value = 0.05,
                min = 0,
                max = 1,
                step = 0.001))),
          column(
            width = 8,
            htmlOutput("scatterPlotBox"))),
        fluidRow(
          htmlOutput("corrTableBox"))
      ), # tabItem

      ### tabItem mvnTab ----
      shinydashboard::tabItem(
        tabName = "mvnTab",
        fluidRow(
          column(
            width = 4,
            shinydashboard::box(
              width = NULL,
              title = "Normality tests:",
              numericInput(
                "mvnSL",
                "Enter the significance level for the tests:",
                value = 0.05,
                min = 0,
                max = 1,
                step = 0.001)),
            shinydashboard::box(
              width = NULL,
              title = "Test on Multivariate Normality:",
              htmlOutput("mvnComment")),
            shinydashboard::box(
              width = NULL,
              title = "Tests on Univariate Normality:",
              htmlOutput("mvnTable"))),
          column(
            width = 8,
            htmlOutput("mvnPlotBox"))
        ) # fluidRow
      ), # tabItem

      ### tabItem testParamTab ----
      shinydashboard::tabItem(
        tabName = "testParamTab",
        fluidRow(

          #### testParamTab left col ----
          column(
            width = 5,
            shinydashboard::box(
              width = NULL,
              fluidRow(

                ##### testParamTab left col estimator ----
                column(
                  width = 6,
                  radioButtons(
                    "estimator",
                    "Choose estimator:",
                    choices = c(
                      "Maximum Likelihood" = "ML",
                      "Robust Maximum Likelihood" = "MLR"),
                    selected = "ML")),

                ##### testParamTab left col sigLvl ----
                column(
                  width = 6,
                  numericInput(
                    "sigLvl",
                    "Enter the significance level:",
                    value = 0.05,
                    min = 0,
                    max = 1,
                    step = 0.001))),

              fluidRow(

                ##### testParamTab left col etaIntFree ----
                column(
                  width = 6,
                  radioButtons(
                    "etaIntFree",
                    "Choose parameterization:",
                    choices = c("Std. Eta" = FALSE, "Std. Alpha" = TRUE))),

                ##### testParamTab left col doMg ----
                column(
                  width = 6,
                  shinyjs::disabled(
                    checkboxInput(
                      "doMg",
                      "Perform Multigroup Tests",
                      value = FALSE)))
              ) # fluidRow
            ), # box

            ##### testParamTab left col goModels ----
            shinydashboard::box(
              width = NULL,
              actionButton("goModels", "Test the models", width = "100%"))
          ), # column

          #### testParamTab right col (model test checkbox table) ----
          column(
            width = 7,
            shinydashboard::box(
              width = NULL,
              title = "Choose models to test and compare:",

              ##### row 1: model names ----
              fluidRow(
                column(2),
                column(2, HTML("<b>&tau;-kong.</b>")),
                column(2, HTML("<b>ess. &tau;-equiv.</b>")),
                column(2, HTML("<b>&tau;-equiv.</b>")),
                column(2, HTML("<b>ess. &tau;-paral.</b>")),
                column(2, HTML("<b>&tau;-paral.</b>"))),

              ##### row 2: tko ----
              fluidRow(
                column(2, HTML("<b>&tau;-kong.</b>")),

                ###### row 2 col 2: tko ----
                column(2,
                  conditionalPanel(
                    "input.itemCols.length > 3 &&
                      input.goModels == 0",
                    checkboxInput("tko", "Include", value = TRUE)),
                  conditionalPanel(
                    "input.itemCols.length <= 3",
                    helpText("Too few items.")),
                  conditionalPanel(
                    "input.goModels > 0 && input.tko",
                    helpText("Tested.")),
                  conditionalPanel(
                  "input.goModels > 0 && !input.tko &&
                    !(input.itemCols.length <= 3)",
                  helpText("Don't test."))),
                column(2),
                column(2),
                column(2),
                column(2)),

              ##### row 3: ete ----
              fluidRow(
                column(2, HTML("<b>ess. &tau;-equiv.</b>")),

                ###### row 3 col 2: ete vs. tko ----
                column(2,
                  conditionalPanel(
                    "input.tko && input.ete &&
                      input.itemCols.length > 3 &&
                      input.goModels == 0",
                    checkboxInput("etetko", "Compare", value = TRUE)),
                  conditionalPanel(
                    "!(input.tko && input.ete) ||
                      input.itemCols.length <= 3 ||
                      (input.goModels > 0 && !input.etetko)",
                    helpText("Don't test.")),
                  conditionalPanel(
                    "input.goModels > 0 && input.etetko",
                    helpText("Tested."))),

                ###### row 3 col 3: ete ----
                column(2,
                  conditionalPanel(
                    "input.itemCols.length > 2 &&
                      input.goModels == 0",
                    checkboxInput("ete", "Include", value = TRUE)),
                  conditionalPanel(
                    "input.itemCols.length <= 2",
                    helpText("Too few items.")),
                  conditionalPanel(
                    "input.goModels > 0 && input.ete",
                      helpText("Tested.")),
                  conditionalPanel(
                    "input.goModels > 0 && !input.ete &&
                      !(input.itemCols.length <= 2)",
                    helpText("Don't test."))),
                column(2),
                column(2),
                column(2)),

              ##### row 4: teq ----
              fluidRow(
                column(2, HTML("<b>&tau;-equiv.</b>")),

                ###### row 4 col 2: teq vs. tko ----
                column(2,
                  conditionalPanel(
                    "input.tko && input.teq &&
                      input.itemCols.length > 3 &&
                      input.goModels == 0",
                    checkboxInput("teqtko", "Compare", value = TRUE)),
                  conditionalPanel(
                    "input.goModels > 0 && input.teqtko",
                    helpText("Tested.")),
                  conditionalPanel(
                    "!(input.tko && input.teq) ||
                      input.itemCols.length <= 3 ||
                      (input.goModels > 0 && !input.teqtko)",
                    helpText("Don't test."))),

                ###### row 4 col 3: teq vs. ete ----
                column(2,
                  conditionalPanel(
                    "input.ete && input.teq &&
                      input.itemCols.length > 2 &&
                      input.goModels == 0",
                    checkboxInput("teqete", "Compare", value = TRUE)),
                  conditionalPanel(
                    "input.goModels > 0 && input.teqete",
                    helpText("Tested.")),
                  conditionalPanel(
                    "!(input.ete && input.teq) ||
                      input.itemCols.length <= 2 ||
                      (input.goModels > 0 && !input.teqete)",
                    helpText("Don't test."))),

                ###### row 4 col 4: teq ----
                column(2,
                  conditionalPanel(
                    "input.itemCols.length > 1 &&
                      input.goModels == 0",
                    checkboxInput("teq", "Include", value = TRUE)),
                  conditionalPanel(
                    "input.itemCols.length < 2",
                    helpText("Too few items.")),
                  conditionalPanel(
                    "input.goModels > 0 && input.teq",
                    helpText("Tested.")),
                  conditionalPanel(
                    "input.goModels > 0 && !input.teq &&
                    !(input.itemCols.length < 2)",
                    helpText("Don't test."))),
                column(2),
                column(2)),

              ##### row 5: etp ----
              fluidRow(
                column(2, HTML("<b>ess. &tau;-paral.</b>")),

                ###### row 5 col 2: etp vs. tko ----
                column(2,
                  conditionalPanel(
                    "input.tko && input.etp &&
                      input.itemCols.length > 3 &&
                      input.goModels == 0",
                    checkboxInput("etptko", "Compare", value = TRUE)),
                  conditionalPanel(
                    "input.goModels > 0 && input.etptko",
                    helpText("Tested.")),
                  conditionalPanel(
                    "!(input.tko && input.etp) ||
                      input.itemCols.length <= 3 ||
                      (input.goModels > 0 && !input.etptko)",
                    helpText("Don't test."))),

                ###### row 5 col 3: etp vs. ete ----
                column(2,
                  conditionalPanel(
                    "input.ete && input.etp &&
                    input.itemCols.length > 2 &&
                    input.goModels == 0",
                      checkboxInput("etpete", "Compare", value = TRUE)),
                  conditionalPanel(
                    "input.goModels > 0 && input.etpete",
                    helpText("Tested.")),
                  conditionalPanel(
                    "!(input.ete && input.etp) ||
                      input.itemCols.length <= 2 ||
                      (input.goModels > 0 && !input.etpete)",
                    helpText("Don't test."))),

                ###### row 5 col 4: etp vs. teq ----
                column(2, helpText("Not testable.")),

                ###### row 5 col 5: etp ----
                column(2,
                  conditionalPanel(
                    "input.itemCols.length > 1 &&
                      input.goModels == 0",
                    checkboxInput("etp", "Include", value = TRUE)),
                  conditionalPanel(
                    "input.itemCols.length < 2",
                    helpText("Too few items.")),
                  conditionalPanel(
                    "input.goModels > 0 && input.etp",
                    helpText("Tested.")),
                  conditionalPanel(
                    "input.goModels > 0 && !input.etp &&
                      !(input.itemCols.length < 2)",
                    helpText("Don't test."))),
                column(2)),

              ##### row 6: tpa ----
              fluidRow(
                column(2, HTML("<b>&tau;-paral.</b>")),

                ###### row 6 col 2: tpa vs. tko ----
                column(2,
                  conditionalPanel(
                    "input.tko && input.tpa &&
                      input.itemCols.length > 3 &&
                      input.goModels == 0",
                  checkboxInput("tpatko", "Compare", value = TRUE)),
                  conditionalPanel(
                    "input.goModels > 0 && input.tpatko",
                    helpText("Tested.")),
                  conditionalPanel(
                    "!(input.tko && input.tpa) ||
                      input.itemCols.length <= 3 ||
                      (input.goModels > 0 && !input.tpatko)",
                    helpText("Don't test."))),

                ###### row 6 col 3: tpa vs. ete ----
                column(2,
                  conditionalPanel(
                    "input.ete && input.tpa &&
                      input.itemCols.length > 2 &&
                      input.goModels == 0",
                    checkboxInput("tpaete", "Compare", value = TRUE)),
                  conditionalPanel(
                    "input.goModels > 0 && input.tpaete",
                    helpText("Tested.")),
                  conditionalPanel(
                    "!(input.ete && input.tpa) ||
                      input.itemCols.length <= 2 ||
                      (input.goModels > 0 && !input.tpaete)",
                    helpText("Don't test."))),

                ###### row 6 col 4: tpa vs. teq ----
                column(2,
                  conditionalPanel(
                    "input.teq && input.tpa &&
                      input.itemCols.length > 1 &&
                      input.goModels == 0",
                    checkboxInput("tpateq", "Compare", value = TRUE)),
                  conditionalPanel(
                    "input.goModels > 0 && input.tpateq",
                    helpText("Tested.")),
                  conditionalPanel(
                    "!(input.teq && input.tpa) ||
                      input.itemCols.length < 2 ||
                      (input.goModels > 0 && !input.tpateq)",
                    helpText("Don't test."))),

                ###### row 6 col 5: tpa vs. etp ----
                column(2,
                  conditionalPanel(
                    "input.etp && input.tpa &&
                      input.itemCols.length > 1 &&
                      input.goModels == 0",
                    checkboxInput("tpaetp", "Compare", value = TRUE)),
                  conditionalPanel(
                    "input.goModels > 0 && input.tpaetp",
                    helpText("Tested.")),
                  conditionalPanel(
                    "!(input.etp && input.tpa) ||
                      input.itemCols.length < 2 ||
                      (input.goModels > 0 && !input.tpaetp)",
                    helpText("Don't test."))),

                ###### row 6 col 6: tpa ----
                column(2,
                  conditionalPanel(
                    "input.itemCols.length > 1 &&
                      input.goModels == 0",
                    checkboxInput("tpa", "Include", value = TRUE)),
                  conditionalPanel(
                    "input.itemCols.length < 2",
                    helpText("Too few items.")),
                  conditionalPanel(
                    "input.goModels > 0 && input.tpa",
                    helpText("Tested.")),
                  conditionalPanel(
                    "input.goModels > 0 && !input.tpa &&
                      !(input.itemCols.length < 2)",
                    helpText("Don't test.")))
              ) # fluidRow
            ) # box
          ) # column
        ) # fluidRow
      ), # tabItem

      ### tabItem modelTests ----
      shinydashboard::tabItem(
        tabName = "modelTests",
        htmlOutput("modelTestsCont")),

      ### tabItem modelTestsMg ----
      shinydashboard::tabItem(
        tabName = "modelTestsMg",
        htmlOutput("modelTestsContMg")),

      ### tabItem parTables ----
      shinydashboard::tabItem(
        tabName = "parTables",
        fluidRow(
          shinydashboard::tabBox(
            id = "parTabsetTab",
            title = "Estimated Paramters",
            width = 12))),

      ### tabItem parTablesMg ----
      shinydashboard::tabItem(
        tabName = "parTablesMg",
        fluidRow(
          shinydashboard::tabBox(
            id = "parTabsetTabMg",
            title = "Estimated Paramters",
            width = 12))),

      ### tabItem facScores ----
      shinydashboard::tabItem(
        tabName = "facScores",
        fluidRow(
          shinydashboard::tabBox(
            id = "fsTabsetTab",
            title = HTML("Predicted Factor Scores (&eta;&#x302;)"),
            width = 12))),

      ### tabItem facScoresMg ----
      shinydashboard::tabItem(
        tabName = "facScoresMg",
        fluidRow(
          shinydashboard::tabBox(
            id = "fsTabsetTabMg",
            title = HTML("Predicted Factor Scores (&eta;&#x302;)"),
            width = 12))),

      ### tabItem modelCode ----
      shinydashboard::tabItem(
        tabName = "modelCode",
        fluidRow(
          shinydashboard::tabBox(
            id = "mcTabsetTab",
            title = "Model Code",
            width = 12))),

      ### tabItem modelCodeMg ----
      shinydashboard::tabItem(
        tabName = "modelCodeMg",
        fluidRow(
          shinydashboard::tabBox(
            id = "mcTabsetTabMg",
            title = "Model Code",
            width = 12)))

    ) # tabItems
  ) # dashboardBody
) # dashboardPage
