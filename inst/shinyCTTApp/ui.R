fluidPage(

    # To be updated later: nice alignment of item checkbox group! ---------------------
    tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
    tags$div(HTML("<script type='text/x-mathjax-config'>
                  MathJax.Hub.Config({ TeX: { extensions: ['color.js'] }});
                  </script>")),

    # ---------------------------------------------------------------------------------

    navbarPage("shinyCTT", id = "navbar",
               tabPanel("Data input",
                        value = "panelDataInput",
                        sidebarLayout(
                            sidebarPanel(
                                conditionalPanel(condition = "input.goModels == 0",
                                                 selectInput("source", "Choose source of data:",
                                                             choices = c("Workspace", "CSV", "SPSS")),
                                                 conditionalPanel(condition = "input.source == 'Workspace'",
                                                                  uiOutput("objectsInWorkspace")),
                                                 conditionalPanel(condition = "input.source == 'CSV'",
                                                                  fileInput("CSVFile", "Choose CSV File",
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
                                                                               selected = '"')
                                                 ),
                                                 hr(),
                                                 uiOutput("itemColsChooser"),
                                                 hr(),
                                                 uiOutput("groupColChooser"),
                                                 hr(),
                                                 radioButtons("estimator",
                                                              "Choose estimator based on test result:",
                                                              choices = c("Maximum Likelihood" = "ML",
                                                                          "Robust Maximum Likelihood" = "MLR"),
                                                              selected = "ML"),
                                                 hr(),
                                                 numericInput("sigLvl",
                                                              "Enter the significance level for all tests:",
                                                              value = 0.05,
                                                              min = 0,
                                                              max = 1,
                                                              step = 0.001),
                                                 hr(),
                                                 conditionalPanel(condition = "output.oneItem == false",
                                                                  helpText("Please select more than one item.")),
                                                 conditionalPanel(condition = "output.isCorrInd == false",
                                                                  helpText("All items seem to be uncorrelated.")),
                                                 conditionalPanel(condition = "output.obsOk == false",
                                                                  helpText("Number of observations too low.")),
                                                 conditionalPanel(condition = "!(input.tk || input.ete || input.te || input.etp || input.tp)",
                                                                  helpText("You didn't select any models.")),
                                                 conditionalPanel(condition = "output.oneItem &&
                                                             output.isCorrInd &&
                                                             output.obsOk &&
                                                             output.allExist &&
                                                             (input.tk || input.ete || input.te || input.etp || input.tp)",
                                                                  div(align = "center",
                                                                      actionButton("goModels",
                                                                                   "Test the models!"),
                                                                      helpText("This may take a few seconds.")))
                                ),
                                conditionalPanel(condition = "input.goModels > 0",
                                                 h5("Selected options:"),
                                                 textOutput("selectedData"),
                                                 hr(),
                                                 textOutput("selectedItems"),
                                                 hr(),
                                                 textOutput("selectedGroup"),
                                                 hr(),
                                                 textOutput("selectedEstimator"),
                                                 hr(),
                                                 textOutput("selectedSigLvl"),
                                                 hr(),
                                                 helpText("The models have been tested.")
                                ), width = 3),
                            mainPanel(
                                wellPanel(h3("Controls"),
                                          uiOutput("checks")),
                                tabsetPanel(
                                    tabPanel("Models to test",
                                             h3("Choose models to compare:"),
                                             fluidRow(
                                                 column(2),
                                                 column(2, HTML("<b>&tau;-kong.</b>")),
                                                 column(2, HTML("<b>ess. &tau;-equiv.</b>")),
                                                 column(2, HTML("<b>&tau;-equiv.</b>")),
                                                 column(2, HTML("<b>ess. &tau;-paral.</b>")),
                                                 column(2, HTML("<b>&tau;-paral.</b>"))
                                             ),
                                             fluidRow(
                                                 column(2, HTML("<b>&tau;-kong.</b>")),
                                                 column(2, conditionalPanel("input.itemCols.length > 3",
                                                                            checkboxInput("tk",
                                                                                          "Test",
                                                                                          value = TRUE)
                                                                            ),
                                                        conditionalPanel("input.itemCols.length <= 3",
                                                                         helpText("Too few items.")
                                                                         )
                                                        ),
                                                 column(2),
                                                 column(2),
                                                 column(2),
                                                 column(2)
                                             ),
                                             fluidRow(
                                                 column(2, HTML("<b>ess. &tau;-equiv.</b>")),
                                                 column(2,
                                                        conditionalPanel("input.tk == true &&
                                                                            input.itemCols.length > 3",
                                                                         checkboxInput("tkEte",
                                                                                       "Compare",
                                                                                       value = TRUE)
                                                                         ),
                                                        conditionalPanel("input.tk == false ||
                                                                         input.itemCols.length <= 3",
                                                                         helpText("Don't test.")
                                                                         )
                                                        ),
                                                 column(2,
                                                        conditionalPanel("input.itemCols.length > 2",
                                                                         checkboxInput("ete",
                                                                                       "Test",
                                                                                       value = TRUE)
                                                                         ),
                                                        conditionalPanel("input.itemCols.length <= 2",
                                                                         helpText("Too few items.")
                                                                         )
                                                        ),
                                                 column(2),
                                                 column(2),
                                                 column(2)
                                             ),
                                             fluidRow(
                                                 column(2, HTML("<b>&tau;-equiv.</b>")),
                                                 column(2,
                                                        conditionalPanel("input.tk == true &&
                                                                            input.itemCols.length > 3",
                                                                         checkboxInput("tkTe",
                                                                                       "Compare",
                                                                                       value = TRUE)
                                                                         ),
                                                        conditionalPanel("input.tk == false ||
                                                                         input.itemCols.length <= 3",
                                                                         helpText("Don't test.")
                                                                         )
                                                        ),
                                                 column(2,
                                                        conditionalPanel("input.ete == true &&
                                                                            input.itemCols.length > 2",
                                                                         checkboxInput("eteTe",
                                                                                       "Compare",
                                                                                       value = TRUE)
                                                                         ),
                                                        conditionalPanel("input.ete == false ||
                                                                         input.itemCols.length <= 2",
                                                                         helpText("Don't test.")
                                                                         )
                                                        ),
                                                 column(2,
                                                        conditionalPanel("input.itemCols.length > 1",
                                                                         checkboxInput("te",
                                                                                       "Test",
                                                                                       value = TRUE)
                                                        ),
                                                        conditionalPanel("input.itemCols.length < 2",
                                                                         helpText("Too few items.")
                                                        )
                                                 ),
                                                 column(2),
                                                 column(2)
                                             ),
                                             fluidRow(
                                                 column(2, HTML("<b>ess. &tau;-paral.</b>")),
                                                 column(2, conditionalPanel("input.tk == true &&
                                                                            input.itemCols.length > 3",
                                                                            checkboxInput("tkEtp",
                                                                                          "Compare",
                                                                                          value = TRUE)
                                                                            ),
                                                        conditionalPanel("input.tk == false ||
                                                                         input.itemCols.length <= 3",
                                                                         helpText("Don't test.")
                                                                         )
                                                        ),
                                                 column(2, conditionalPanel("input.ete == true &&
                                                                            input.itemCols.length > 2",
                                                                            checkboxInput("eteEtp",
                                                                                          "Compare",
                                                                                          value = TRUE)
                                                                            ),
                                                        conditionalPanel("input.ete == false ||
                                                                         input.itemCols.length <= 2",
                                                                         helpText("Don't test.")
                                                                         )
                                                        ),
                                                 column(2, helpText("Not comparable")),
                                                 column(2,
                                                        conditionalPanel("input.itemCols.length > 1",
                                                                         checkboxInput("etp",
                                                                                       "Test",
                                                                                       value = TRUE)
                                                        ),
                                                        conditionalPanel("input.itemCols.length < 2",
                                                                         helpText("Too few items.")
                                                        )
                                                        ),
                                                 column(2)
                                             ),
                                             fluidRow(
                                                 column(2, HTML("<b>&tau;-paral.</b>")),
                                                 column(2, conditionalPanel("input.tk == true &&
                                                                            input.itemCols.length > 3",
                                                                            checkboxInput("tkTp",
                                                                                          "Compare",
                                                                                          value = TRUE)
                                                                            ),
                                                        conditionalPanel("input.tk == false ||
                                                                         input.itemCols.length <= 3",
                                                                         helpText("Don't test.")
                                                                         )
                                                        ),
                                                 column(2, conditionalPanel("input.ete == true &&
                                                                            input.itemCols.length > 2",
                                                                            checkboxInput("eteTp",
                                                                                          "Compare",
                                                                                          value = TRUE)
                                                                            ),
                                                        conditionalPanel("input.ete == false ||
                                                                         input.itemCols.length <= 2",
                                                                         helpText("Don't test.")
                                                                         )
                                                        ),
                                                 column(2, conditionalPanel("input.te == true &&
                                                                            input.itemCols.length > 1",
                                                                            checkboxInput("teTp",
                                                                                          "Compare",
                                                                                          value = TRUE)
                                                                            ),
                                                        conditionalPanel("input.te == false ||
                                                                         input.itemCols.length < 2",
                                                                         helpText("Don't test."))
                                                        ),
                                                 column(2, conditionalPanel("input.etp == true &&
                                                                            input.itemCols.length > 1",
                                                                            checkboxInput("etpTp",
                                                                                          "Compare",
                                                                                          value = TRUE)
                                                                            ),
                                                        conditionalPanel("input.etp == false ||
                                                                         input.itemCols.length < 2",
                                                                         helpText("Don't test.")
                                                                         )
                                                        ),
                                                 column(2,
                                                        conditionalPanel("input.itemCols.length > 1",
                                                                         checkboxInput("tp",
                                                                                       "Test",
                                                                                       value = TRUE)
                                                                         ),
                                                        conditionalPanel("input.itemCols.length < 2",
                                                                         helpText("Too few items.")
                                                        )
                                                 )
                                             )
                                    ),
                                    tabPanel("Data Overview",
                                             h3("Raw data:"),
                                             dataTableOutput("dataOverview")
                                    ),
                                    tabPanel("Statistics",
                                             uiOutput("corrWarnings"),
                                             h3("Mean, Standard Deviation, Skewness, Excess:"),
                                             htmlOutput("descrTable"),
                                             h3("Correlation Table with Confidence Intervals:"),
                                             htmlOutput("corrTableWithCIs"),
                                             uiOutput("corrTableLegend"),
                                             h3("Covariance Matrix:"),
                                             htmlOutput("covMat")
                                    ),
                                    tabPanel("Multivariate Normality",
                                             h3("Test on Multivariate Normality:"),
                                             htmlOutput("mvnComment"),
                                             div(align = "center",
                                                 tableOutput("mvnTableMV")),
                                             h3("Tests on Univariate Normality:"),
                                             div(align = "center",
                                                 tableOutput("mvnTableUV")),
                                             verbatimTextOutput("buttonValue")
                                    )
                                )
                            )
                        )
               )
    )
)
