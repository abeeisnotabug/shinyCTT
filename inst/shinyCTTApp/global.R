kableStylingOptions <- list(full_width = F, position = "center")

inputPanel <- tabPanel("Data input",
                       value = "panelDataInput",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("source", "Choose source of data:",
                                       choices = c("Workspace", "CSV", "SPSS")),
                           conditionalPanel(condition = "input.source == 'Workspace'",
                                            uiOutput("objectsInWorkspace")),
                           conditionalPanel(condition = "input.source == 'CSV'",
                                            fileInput("CSVFile", "Choose CSV File",
                                                      multiple = TRUE,
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
                           hr(),
                           uiOutput("itemColsChooser"),
                           hr(),
                           uiOutput("groupColChooser"),
                           hr(),
                           div(align = "center",
                               actionButton("goDescrStats",
                                            "Calculate descriptive statistics!"))
                         ),
                         mainPanel(
                           h3("Raw data:"),
                           dataTableOutput("dataOverview"),
                           textOutput("workspace")
                         )
                       )
)

descrPanel <- tabPanel("Descriptive Statistics",
                       value = "panelDescrStats",
                       sidebarLayout(
                         sidebarPanel(
                           checkboxGroupInput("toggleStats",
                                              "Show statistics:",
                                              choices = c("Mean, Standard Deviation, Skewness, Excess" = "showDescrTable",
                                                          "Correlation table and plot" = "showCorrPlot",
                                                          "Covariance Matrix" = "showCovMat"),
                                              selected = c("showDescrTable",
                                                           "showCorrPlot",
                                                           "showCovMat")),
                           hr(),
                           conditionalPanel(condition = "input.goMVN == 0",
                                            div(align = "center",
                                                actionButton("goMVN",
                                                             "Test for multivariate normality!"))),
                           conditionalPanel(condition = "input.goMVN > 0",
                                            helpText("Test on MVN has been performed."))
                         ),
                         mainPanel(
                           uiOutput("descrTableUI"),
                           uiOutput("corrPlotUI"),
                           uiOutput("covMatUI")
                         )
                       )
)

mvnPanel <- tabPanel("MVN",
                     value = "panelMVN",
                     sidebarLayout(
                       sidebarPanel(
                         conditionalPanel(condition = "input.goCorrInd == 0",
                                          radioButtons("estimator",
                                                       "Choose estimator based on test result:",
                                                       choices = c("Maximum Likelihood" = "ML",
                                                                   "Robust Maximum Likelihood" = "MLR"),
                                                       selected = "ML")),
                         conditionalPanel(condition = "input.goCorrInd > 0",
                                          #helpText("The following estimator has been chosen:"),
                                          textOutput("selectedEstimator")),
                         hr(),
                         conditionalPanel(condition = "input.goCorrInd == 0",
                                          div(align = "center",
                                              actionButton("goCorrInd",
                                                           "Test for correlative independence!"))),
                         conditionalPanel(condition = "input.goCorrInd > 0",
                                          helpText("The models have been calculated and the test
                                                            for correlative independence has been performed."))
                       ),
                       mainPanel(
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

corrIndPanel <- tabPanel("Corr. Ind.",
                         value = "panelCorrInd",
                         sidebarLayout(
                           sidebarPanel(
                             textInput("corrIndSigLvl",
                                       "Enter the desired significance level",
                                       value = "0.05"),
                             hr(),
                             conditionalPanel(condition = "input.goModels == 0",
                                              div(align = "center",
                                                  actionButton("goModels",
                                                               "Test the models!"),
                                                  helpText("This may take a few seconds."))),
                             conditionalPanel(condition = "input.goModels > 0",
                                              helpText("The models have been tested."))
                           ),
                           mainPanel(
                             h3("Test on Correlative Independence:"),
                             uiOutput("corrIndText"),
                             h3("Correlation Table with Confidence Intervals:"),
                             tableOutput("corTableWithCIs")
                           )
                         )
)

modelTestPanel <- tabPanel("Models",
                           value = "panelModelTests",
                           tabsetPanel(
                             tabPanel(HTML("&tau;-kongeneric"),
                                      h3("Test on Model Fit:"),
                                      uiOutput("tkModelFitText"),
                                      h3("Estimated paramters with Standard Errors and Confidence Intervals:"),
                                      uiOutput("tkTable")),
                             tabPanel(HTML("essentially &tau;-equivalent")),
                             tabPanel(HTML("&tau;-equivalent")),
                             tabPanel(HTML("essentially &tau;-parallel")),
                             tabPanel(HTML("&tau;-parallel"))
                           )
)
