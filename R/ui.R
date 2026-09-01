makeFUdashboardtheme <- function() {
  dashboardthemes::shinyDashboardThemeDIY(

    ### general
    appFontFamily = "Arial"
    ,appFontColor = "#000000"
    ,bodyBackColor = "#FFFFFF"

    ### header
    ,logoBackColor = "#99CC00"

    ,headerButtonBackColor = "#99CC00"
    ,headerButtonIconColor = "#FFFFFF"
    ,headerButtonBackColorHover = "#666666"
    ,headerButtonIconColorHover = "#FFFFFF"

    ,headerBackColor = "#99CC00"
    ,headerBoxShadowColor = "#AAAAAA"
    ,headerBoxShadowSize = "2px 2px 2px"

    ### sidebar
    ,sidebarBackColor = "#FFFFFF"
    ,sidebarPadding = 0

    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0

    ,sidebarShadowRadius = "3px 5px 5px"
    ,sidebarShadowColor = "#AAAAAA"

    ,sidebarUserTextColor = "#000000"

    ,sidebarSearchBackColor = "rgb(55,72,80)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "#CCCCCC"

    ,sidebarTabTextColor = "#000000"
    ,sidebarTabTextSize = 13
    ,sidebarTabBorderStyle = "none none solid none"
    ,sidebarTabBorderColor = "#CCCCCC"
    ,sidebarTabBorderWidth = 1

    ,sidebarTabBackColorSelected = "#EEEEEE"
    ,sidebarTabTextColorSelected = "#000000"
    ,sidebarTabRadiusSelected = "0px 0px 0px 0px"

    ,sidebarTabBackColorHover = "#EEEEEE"
    ,sidebarTabTextColorHover = "#000000"
    ,sidebarTabBorderStyleHover = "none none solid none"
    ,sidebarTabBorderColorHover = "#CCCCCC"
    ,sidebarTabBorderWidthHover = 1
    ,sidebarTabRadiusHover = "0px 0px 0px 0px"

    ### boxes
    ,boxBackColor = "#FFFFFF"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "#99CC00"

    ,boxPrimaryColor = "rgba(44,222,235,1)"
    ,boxInfoColor = "rgb(210,214,220)"
    ,boxSuccessColor = "rgba(0,255,213,1)"
    ,boxWarningColor = "rgb(244,156,104)"
    ,boxDangerColor = "rgb(255,88,55)"

    ,tabBoxTabColor = "#FFFFFF"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "#000000"
    ,tabBoxTabTextColorSelected = "#000000"
    ,tabBoxBackColor = "#FFFFFF"
    ,tabBoxHighlightColor = "#99CC00"
    ,tabBoxBorderRadius = 5

    ### inputs
    ,buttonBackColor = "#FFFFFF"
    ,buttonTextColor = "#000000"
    ,buttonBorderColor = "#DDDDDD"
    ,buttonBorderRadius = 5

    ,buttonBackColorHover = "#FFFFFF"
    ,buttonTextColorHover = "#000000"
    ,buttonBorderColorHover = "#999999"

    ,textboxBackColor = "rgb(255,255,255)"
    ,textboxBorderColor = "rgb(200,200,200)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(245,245,245)"
    ,textboxBorderColorSelect = "rgb(200,200,200)"

    ### tables
    ,tableBackColor = "rgb(255,255,255)"
    ,tableBorderColor = "rgb(240,240,240)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1

  ) # shinyDashboardThemeDIY
}

ui <- function(request) {
  shinydashboard::dashboardPage(
    # dashboardHeader ----
    shinydashboard::dashboardHeader(
      title = "shinyCTT",
      shinydashboard::dropdownMenuOutput("infoMenu")),

    # dashboardSidebar ----
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenuOutput("dataMenuOut")),

    # dashboardBody ----
    shinydashboard::dashboardBody(

      makeFUdashboardtheme(),

      ## modify box look ----
      tags$head(
        tags$style(".checkbox-inline {margin: 0 !important;}"),
        tags$style(HTML(".navbar-custom-menu>.navbar-nav>li>.dropdown-menu {width:600px;}")),
        tags$style(".bg-green { background-color: #99CC00 !important; color: #FFFFFF !important; }"),
        tags$style(".bg-blue { background-color: #003F8A !important; color: #FFFFFF !important; }")),

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
                            accept = c(".sav", ".zsav", ".por")))),

              shinydashboard::box(
                width = NULL,
                actionButton("dataSelectButton", "Select", width = "100%"))

            ), # column
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

              conditionalPanel(
                "output.incompleteCasesBoolRV",

                shinydashboard::box(
                  width = NULL,
                  tagList(
                    strong("2c. Choose how to handle missing values:"),
                    checkboxInput(
                      "useFIML",
                      "Use Full Information Maximum Likelihood (FIML) for all analyses in lavaan",
                      value = TRUE),

                    conditionalPanel(
                      "!input.useFIML",
                      div(
                        style = paste0("color:red"),
                        HTML("WARNING: Not using FIML in the presence of missing
                                values implies listwise deletion in lavaan.
                                This is only valid if the data are missing
                                completely at random (MCAR) and reduces
                                statistical power.")))))
              ), # conditionalPanel

              shinydashboard::box(
                width = NULL,
                # subset of items
                actionButton("subsetSelectButton", "Select", width = "100%"))
            ), # column

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
                title = "Test on correlative independence:",
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
                  min = 0.001,
                  max = 1,
                  step = 0.001),
                htmlOutput("corrInd")),

              shinydashboard::box(
                width = NULL,
                title = "Correlation table with confidence intervals:",
                shinyjs::hidden(
                  radioButtons(
                    "corrTabNA",
                    "Choose how to handle missing values:",
                    choices = c("Use pairwise complete observations" = "pairwise.complete.obs",
                                "Use only complete observations" = "complete.obs"),
                    selected = "pairwise.complete.obs")),
                numericInput(
                  "corrTabSL",
                  "Enter the significance level for the correlation tests:",
                  value = 0.05,
                  min = 0.001,
                  max = 1,
                  step = 0.001))
            ), # column

            column(
              width = 8,
              htmlOutput("scatterBox"))
          ), # fluidRow

          fluidRow(
            htmlOutput("corrTableBox"))
        ), # tabItem

        ### tabItem mvnTab ----
        shinydashboard::tabItem(
          tabName = "mvnTab",
          # fluidRow(shinydashboard::infoBox(title = "Hint:")),
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
                  min = 0.001,
                  max = 1,
                  step = 0.001)),

              shinydashboard::box(
                width = NULL,
                title = "Test on multivariate normality:",
                htmlOutput("mvnComment")),

              shinydashboard::box(
                width = NULL,
                title = "Tests on univariate normality:",
                htmlOutput("mvnTable"))
            ), # column

            column(
              width = 8,
              fluidRow(htmlOutput("mvnPlotBox")),
              fluidRow(shinydashboard::infoBox(
                title = "Hint:",
                subtitle = "For more extensive analyses on multivariate normality, load() the MVN package and open its shiny app via run_mvn_app()!",
                icon = icon("lightbulb"),
                color = "green",
                width = 12,
                fill = TRUE)))
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
                    width = 3,
                    numericInput(
                      "sigLvl",
                      "Enter the significance level:",
                      value = 0.05,
                      min = 0,
                      max = 1,
                      step = 0.001)),

                  ##### testParamTab left col rmseaCiLvl ----
                  # A confidence level, not a significance level, and set on its own.
                  # 0.90 is the interval lavaan reports by default.
                  column(
                    width = 3,
                    numericInput(
                      "rmseaCiLvl",
                      "Enter the confidence level of the RMSEA interval:",
                      value = 0.90,
                      min = 0.5,
                      max = 0.999,
                      step = 0.01))),

                fluidRow(

                  ##### testParamTab left col etaIntFree ----
                  column(
                    width = 6,
                    radioButtons(
                      "etaIntFree",
                      "Choose the mean structure parameterization:",
                      choiceNames = list(
                        HTML("Fix the latent mean (&mu;<sub>&eta;</sub> = 0)"),
                        HTML("Fix the first intercept (&alpha;<sub>1</sub> = 0)")),
                      choiceValues = c(FALSE, TRUE))),

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
                actionButton("goModels", "Test the models", width = "100%"),
                htmlOutput("goModelsError"),
                htmlOutput("refitPendingNote"))
            ), # column

            #### testParamTab right col (model test checkbox table) ----
            column(
              width = 7,
              shinydashboard::box(
                width = NULL,
                title = "Choose models to test and compare:",

                comparisonGrid(cttModelFamily())
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
            uiOutput("parTabset"))),

        ### tabItem parTablesMg ----
        shinydashboard::tabItem(
          tabName = "parTablesMg",
          fluidRow(
            uiOutput("parTabsetMg"))),

        ### tabItem facScores ----
        shinydashboard::tabItem(
          tabName = "facScores",
          fluidRow(
            uiOutput("fsTabset"))),

        ### tabItem facScoresMg ----
        shinydashboard::tabItem(
          tabName = "facScoresMg",
          fluidRow(
            uiOutput("fsTabsetMg"))),

        ### tabItem modelCode ----
        shinydashboard::tabItem(
          tabName = "modelCode",
          fluidRow(
            uiOutput("mcTabset"))),

        ### tabItem modelCodeMg ----
        shinydashboard::tabItem(
          tabName = "modelCodeMg",
          fluidRow(
            uiOutput("mcTabsetMg")))

      ) # tabItems
    ) # dashboardBody
  ) # dashboardPage
}
