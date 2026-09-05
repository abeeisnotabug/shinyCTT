## The page: the green bar, the menu down the left, and one panel per tab.
##
## How it all looks is helpers-look.R (fuTheme, fuStyle) and inst/styles.css.

ui <- function(request) {

  # Which language to build this page in. It comes out of the address the browser asked
  # for (...?lang=de), so two people can have the app open in different languages at the
  # same time. No ?lang= at all gives NULL, which falls back to whatever
  # shinyCTTApp(language = ) was given.
  setUiLanguage(parseQueryString(request$QUERY_STRING)$lang)

  bslib::page_sidebar(

    theme = fuTheme(),

    # fillable = FALSE: a card is as tall as what is in it and the page scrolls, which is
    # how the boxes behaved. Left TRUE they stretch to fill the window.
    fillable = FALSE,

    window_title = "shinyCTT",

    # the green bar ----
    # page_sidebar() takes any tag as its title and drops it into the bar unchanged, so the
    # whole bar - the name on the left, the language chooser and the bell on the right - is
    # written here.
    title = div(
      class = "cttHeader",

      span(class = "cttBrand", "shinyCTT"),

      div(
        class = "cttHeaderRight",

        selectInput(
          "language",
          label = NULL,
          width = "150px",
          selected = currentLanguage(),
          choices = stats::setNames(appLanguages, languageLabels()[appLanguages])),

        uiOutput("infoMenu"))),

    # the menu ----
    sidebar = bslib::sidebar(
      width = 230,
      bg = "#FFFFFF",
      padding = 0,
      gap = 0,
      uiOutput("dataMenuOut")),

    fuStyle(),

    shinyjs::useShinyjs(),

    ## the panels ----
    # One per tab, all built at startup and hidden until the menu picks one. server.R does
    # the picking with bslib::nav_select("dataMenu", ...); the menu itself only reports
    # which link was clicked.
    bslib::navset_hidden(
      id = "dataMenu",

      ### dataSelectionTab ----
      bslib::nav_panel_hidden("dataSelectionTab", dataSourceUI("dataSource")),

      ### subsetSelectionTab ----
      bslib::nav_panel_hidden("subsetSelectionTab", dataSubsetUI("subset")),

      ### statisticsTab ----
      # A card has no width, so the widths are here: the top two boxes half a row each,
      # the covariance matrix a whole one. Every box sits in a column, a full-width one
      # included (see GOTCHAS.md).
      bslib::nav_panel_hidden(
        "statisticsTab",
        fluidRow(
          column(width = 6, descriptivesUI("descriptives")),
          column(width = 6, histogramUI("histogram"))),

        fluidRow(
          column(width = 12, covMatrixUI("covmatrix")))), # bslib::nav_panel_hidden

      ### corrTab ----
      bslib::nav_panel_hidden(
        "corrTab",
        fluidRow(
          column(
            width = 4,
            corrIndependenceUI("corrIndependence"),
            corrTableControlsUI("corrTable")), # column

          column(
            width = 8,
            scatterUI("scatter"))), # fluidRow

        fluidRow(
          column(width = 12, corrTableUI("corrTable")))), # bslib::nav_panel_hidden

      ### mvnTab ----
      bslib::nav_panel_hidden("mvnTab", mvnUI("mvn")),

      ### testParamTab ----
      bslib::nav_panel_hidden("testParamTab", testingParamsUI("params")),

      ### modelTests ----
      bslib::nav_panel_hidden("modelTests", cttResultsUI("single")),

      ### modelTestsMg ----
      bslib::nav_panel_hidden("modelTestsMg", cttResultsUI("multigroup")),

      ### parTables ----
      bslib::nav_panel_hidden("parTables", cttParTablesUI("single")),

      ### parTablesMg ----
      bslib::nav_panel_hidden("parTablesMg", cttParTablesUI("multigroup")),

      ### facScores ----
      bslib::nav_panel_hidden("facScores", cttFactorScoresUI("single")),

      ### facScoresMg ----
      bslib::nav_panel_hidden("facScoresMg", cttFactorScoresUI("multigroup")),

      ### modelCode ----
      bslib::nav_panel_hidden("modelCode", cttModelCodeUI("single")),

      ### modelCodeMg ----
      bslib::nav_panel_hidden("modelCodeMg", cttModelCodeUI("multigroup"))

    ) # navset_hidden
  ) # page_sidebar
}
