fluidPage(

    # To be updated later: nice alignment of item checkbox group! ---------------------
    tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
    tags$script(type = "text/x-mathjax-config",
                HTML("MathJax.Hub.Config({ TeX: { extensions: ['color.js'] }});")),
    # ---------------------------------------------------------------------------------

    navbarPage(
        title = "shinyCTT",
        id = "navbar",
        tabPanel(
            title = "Data input",
            value = "panelDataInput",
            sidebarLayout(
                sidebarPanel(
                    conditionalPanel(
                        condition = "input.goModels == 0",
                        selectInput("source", "Choose source of data:",
                                    choices = c("Workspace", "CSV", "SPSS")),
                        conditionalPanel(
                            condition = "input.source == 'Workspace'",
                            uiOutput("objectsInWorkspace")
                        ),
                        conditionalPanel(
                            condition = "input.source == 'CSV'",
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
                        radioButtons(
                            "estimator",
                            "Choose estimator based on test result:",
                            choices = c("Maximum Likelihood" = "ML",
                                        "Robust Maximum Likelihood" = "MLR"),
                            selected = "ML"
                        ),
                        hr(),
                        numericInput(
                            "sigLvl",
                            "Enter the significance level for all tests:",
                            value = 0.05,
                            min = 0,
                            max = 1,
                            step = 0.001
                        ),
                        hr(),
                        conditionalPanel(
                            condition = "output.oneItem == false",
                            helpText("Please select more than one item.")
                        ),
                        conditionalPanel(
                            condition = "output.isCorrInd == false",
                            helpText("All items seem to be uncorrelated.")
                        ),
                        conditionalPanel(
                            condition = "output.obsOk == false",
                            helpText("Number of observations too low.")
                        ),
                        conditionalPanel(
                            condition = "!(input.tko || input.ete || input.teq || input.etp || input.tpa)",
                            helpText("You didn't select any models.")
                        ),
                        conditionalPanel(
                            condition = "output.oneItem &&
                                         output.obsOk &&
                                         output.isCorrInd &&
                                         (input.tko || input.ete || input.teq || input.etp || input.tpa)",
                            div(align = "center",
                                actionButton("goModels",
                                             "Test the models"),
                                helpText("This may take a few seconds."))
                        )
                    ),
                    conditionalPanel(
                        condition = "input.goModels > 0",
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
                    wellPanel(
                        h4("Controls"),
                        uiOutput("checks")
                    ),
                    tabsetPanel(
                        tabPanel(
                            "Data Overview",
                            h4("Raw data:"),
                            dataTableOutput("dataOverview")
                        ),
                        tabPanel(
                            "Statistics",
                            htmlOutput("descrTable"),
                            htmlOutput("corrTableWithCIs"),
                            htmlOutput("covMat")
                        ),
                        tabPanel(
                            "Multivariate Normality",
                            htmlOutput("mvnComment"),
                            htmlOutput("mvnTableUV")
                        ),
                        tabPanel(
                            "Models to test",
                            h4("Choose models to test and compare:"),
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
                                column(2,
                                       conditionalPanel(
                                           "input.itemCols.length > 3",
                                           checkboxInput("tko",
                                                         "Include",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.itemCols.length <= 3",
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
                                       conditionalPanel(
                                           "input.tko && input.ete &&
                                           input.itemCols.length > 3",
                                           checkboxInput("etetko",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "!(input.tko && input.ete) ||
                                           input.itemCols.length <= 3",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.itemCols.length > 2",
                                           checkboxInput("ete",
                                                         "Include",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.itemCols.length <= 2",
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
                                       conditionalPanel(
                                           "input.tko && input.teq &&
                                           input.itemCols.length > 3",
                                           checkboxInput("teqtko",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "!(input.tko && input.teq) ||
                                           input.itemCols.length <= 3",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.ete && input.teq &&
                                           input.itemCols.length > 2",
                                           checkboxInput("teqete",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "!(input.ete && input.teq) ||
                                           input.itemCols.length <= 2",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.itemCols.length > 1",
                                           checkboxInput("teq",
                                                         "Include",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.itemCols.length < 2",
                                           helpText("Too few items.")
                                       )
                                ),
                                column(2),
                                column(2)
                            ),
                            fluidRow(
                                column(2, HTML("<b>ess. &tau;-paral.</b>")),
                                column(2,
                                       conditionalPanel(
                                           "input.tko && input.etp &&
                                           input.itemCols.length > 3",
                                           checkboxInput("etptko",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "!(input.tko && input.etp) ||
                                           input.itemCols.length <= 3",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.ete && input.etp &&
                                           input.itemCols.length > 2",
                                           checkboxInput("etpete",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "!(input.ete && input.etp) ||
                                           input.itemCols.length <= 2",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2, helpText("Not comparable")),
                                column(2,
                                       conditionalPanel(
                                           "input.itemCols.length > 1",
                                           checkboxInput("etp",
                                                         "Include",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.itemCols.length < 2",
                                           helpText("Too few items.")
                                       )
                                ),
                                column(2)
                            ),
                            fluidRow(
                                column(2, HTML("<b>&tau;-paral.</b>")),
                                column(2,
                                       conditionalPanel(
                                           "input.tko && input.tpa &&
                                           input.itemCols.length > 3",
                                           checkboxInput("tpatko",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "!(input.tko && input.tpa) ||
                                           input.itemCols.length <= 3",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.ete && input.tpa &&
                                           input.itemCols.length > 2",
                                           checkboxInput("tpaete",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "!(input.ete && input.tpa) ||
                                           input.itemCols.length <= 2",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.teq && input.tpa &&
                                           input.itemCols.length > 1",
                                           checkboxInput("tpateq",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "!(input.teq && input.tpa) ||
                                           input.itemCols.length < 2",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.etp && input.tpa &&
                                           input.itemCols.length > 1",
                                           checkboxInput("tpaetp",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "!(input.etp && input.tpa) ||
                                           input.itemCols.length < 2",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.itemCols.length > 1",
                                           checkboxInput("tpa",
                                                         "Include",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.itemCols.length < 2",
                                           helpText("Too few items.")
                                       )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)
