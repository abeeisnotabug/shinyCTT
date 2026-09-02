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
          dataSourceUI("dataSource")
        ), # tabItem

        ### tabItem subsetSelectionTab ----
        shinydashboard::tabItem(
          tabName = "subsetSelectionTab",
          dataSubsetUI("subset")
        ), # tabItem

        ### tabItem statisticsTab ----
        shinydashboard::tabItem(
          tabName = "statisticsTab",
          fluidRow(
            descriptivesUI("descriptives"),
            histogramUI("histogram")),
          fluidRow(
            covMatrixUI("covmatrix"))),

        ### tabItem corrTab ----
        shinydashboard::tabItem(
          tabName = "corrTab",
          fluidRow(
            column(
              width = 4,

              corrIndependenceUI("corrIndependence"),

              corrTableControlsUI("corrTable")
            ), # column

            column(
              width = 8,
              scatterUI("scatter"))
          ), # fluidRow

          fluidRow(
            corrTableUI("corrTable"))
        ), # tabItem

        ### tabItem mvnTab ----
        shinydashboard::tabItem(
          tabName = "mvnTab",
          mvnUI("mvn")
        ), # tabItem

        ### tabItem testParamTab ----
        shinydashboard::tabItem(
          tabName = "testParamTab",
          fluidRow(

            #### testParamTab left col ----
            column(
              width = 5,

              ##### testParamTab left col how to fit ----
              shinydashboard::box(
                width = NULL,
                title = tr("How the models are fitted:"),
                fluidRow(

                  column(
                    width = 6,
                    radioButtons(
                      "estimator",
                      tr("Choose estimator:"),
                      choiceNames = list(
                        tr("Maximum Likelihood"),
                        tr("Robust Maximum Likelihood")),
                      choiceValues = c("ML", "MLR"),
                      selected = "ML")),

                  column(
                    width = 6,
                    radioButtons(
                      "etaIntFree",
                      tr("Choose the mean structure parameterization:"),
                      choiceNames = list(
                        HTML(tr("Fix the latent mean (&mu;<sub>&eta;</sub> = 0)")),
                        HTML(tr("Fix the first intercept (&alpha;<sub>1</sub> = 0)"))),
                      choiceValues = c(FALSE, TRUE)))
                ), # fluidRow

                # Full width rather than under the estimator buttons: in half a column this
                # is four words per line.
                htmlOutput("estimatorNote"),

                hr(),

                shinyjs::disabled(
                  checkboxInput(
                    "doMg",
                    tr("Perform Multigroup Tests"),
                    value = FALSE))
              ), # box

              ##### testParamTab left col what the tables show ----
              # Stacked rather than side by side: the two labels are different lengths, and
              # in a column this narrow they would sit at different heights and look cramped.
              shinydashboard::box(
                width = NULL,
                title = tr("What the tables show:"),
                helpText(tr("These two change the tables only. The models are not fitted again, so both can be changed after a run.")),

                numericInput(
                  "sigLvl",
                  tr("Significance level:"),
                  value = 0.05,
                  min = 0.001,
                  max = 1,
                  step = 0.001),
                htmlOutput("sigLvlNote"),

                # A confidence level, not a significance level, and set on its own.
                # 0.90 is the interval lavaan reports by default.
                numericInput(
                  "rmseaCiLvl",
                  tr("Confidence level of the RMSEA interval:"),
                  value = 0.90,
                  min = 0.5,
                  max = 0.999,
                  step = 0.01),
                htmlOutput("rmseaCiLvlNote")
              ), # box

              ##### testParamTab left col goModels ----
              shinydashboard::box(
                width = NULL,
                actionButton("goModels", tr("Fit and compare models"), width = "100%"),
                htmlOutput("goModelsError"),
                htmlOutput("refitPendingNote"))
            ), # column

            #### testParamTab right col (model test checkbox table) ----
            column(
              width = 7,
              shinydashboard::box(
                width = NULL,
                title = tr("Choose models to test and compare:"),

                comparisonGrid(cttModelFamily())
              ) # box
            ) # column
          ) # fluidRow
        ), # tabItem

        ### tabItem modelTests ----
        shinydashboard::tabItem(
          tabName = "modelTests",
          cttResultsUI("single")),

        ### tabItem modelTestsMg ----
        shinydashboard::tabItem(
          tabName = "modelTestsMg",
          cttResultsUI("multigroup")),

        ### tabItem parTables ----
        shinydashboard::tabItem(
          tabName = "parTables",
          cttParTablesUI("single")),

        ### tabItem parTablesMg ----
        shinydashboard::tabItem(
          tabName = "parTablesMg",
          cttParTablesUI("multigroup")),

        ### tabItem facScores ----
        shinydashboard::tabItem(
          tabName = "facScores",
          cttFactorScoresUI("single")),

        ### tabItem facScoresMg ----
        shinydashboard::tabItem(
          tabName = "facScoresMg",
          cttFactorScoresUI("multigroup")),

        ### tabItem modelCode ----
        shinydashboard::tabItem(
          tabName = "modelCode",
          cttModelCodeUI("single")),

        ### tabItem modelCodeMg ----
        shinydashboard::tabItem(
          tabName = "modelCodeMg",
          cttModelCodeUI("multigroup"))

      ) # tabItems
    ) # dashboardBody
  ) # dashboardPage
}
