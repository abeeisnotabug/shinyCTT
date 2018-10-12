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
                        conditionalPanel(
                            condition = "input.source == 'SPSS'",
                            fileInput("SPSSFile", "Choose SPSS File",
                                      multiple = FALSE,
                                      accept = c(".sav",
                                                 ".zsav",
                                                 ".por"))
                        ),
                        hr(),
                        uiOutput("itemColsChooser"),
                        hr(),
                        uiOutput("groupColChooser"),
                        conditionalPanel(
                            condition = "input.groupCol != 'no'",
                            uiOutput("groupChooser"),
                            conditionalPanel(
                                condition = "input.groups.length > 1",
                                checkboxInput(
                                    "doMg",
                                    "Perform Multigroup Tests",
                                    value = FALSE
                                )
                            )
                        ),
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
                            condition = "(input.tko || input.ete || input.teq || input.etp || input.tpa) &&
                                         output.oneItem &&
                                         output.obsOk &&
                                         output.userDataExists",
                            div(
                                align = "center",
                                actionButton("goModels",
                                             "Test the models"),
                                helpText("This may take a few seconds.")
                            )
                        ),
                        conditionalPanel(
                            condition = "!(input.tko || input.ete || input.teq || input.etp || input.tpa)",
                            helpText("You didn't select any models.")
                        ),
                        conditionalPanel(
                            condition = "!output.oneItem",
                            helpText("Please select more than one item.")
                        ),
                        conditionalPanel(
                            condition = "!output.obsOk",
                            helpText("Number of observations too low.")
                        ),
                        conditionalPanel(
                            condition = "!output.userDataExists",
                            helpText("Can't test the models since there is no valid data.")
                        ),
                        hr(),
                        checkboxInput(
                            "showControls",
                            "Show controls",
                            value = TRUE
                        ),
                        checkboxInput(
                            "showOptions",
                            "Show advanced options"
                        ),
                        conditionalPanel(
                            "input.showOptions",
                            radioButtons(
                                "para",
                                "Choose parameterization:",
                                choices = c("Std. Eta", "Std. Alpha")
                            )
                        )
                    ),
                    conditionalPanel(
                        condition = "input.goModels > 0",
                        h5("Selected options:"),
                        htmlOutput("selectedData"),
                        hr(),
                        htmlOutput("selectedItems"),
                        hr(),
                        textOutput("selectedGroup"),
                        hr(),
                        textOutput("selectedEstimator"),
                        hr(),
                        textOutput("selectedSigLvl"),
                        hr(),
                        helpText("The models have been tested.")
                    ),
                    width = 3
                ),
                mainPanel(
                    conditionalPanel(
                        "input.showControls",
                        wellPanel(
                            h4("Controls"),
                            uiOutput("oneItemCheck"),
                            uiOutput("checksUI")
                        )
                    ),
                    tabsetPanel(id = "preTestTabs",
                        tabPanel(
                            "Data Overview",
                            h4("Raw data:"),
                            dataTableOutput("dataOverview")
                        ),
                        # A table with all the models to test in checkboxes ------------------------------------------------
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
                                           "input.itemCols.length > 3 &&
                                           input.goModels == 0",
                                           checkboxInput("tko",
                                                         "Include",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.itemCols.length <= 3",
                                           helpText("Too few items.")
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.tko",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && !input.tko &&
                                           !(input.itemCols.length <= 3)",
                                           helpText("Don't test.")
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
                                           input.itemCols.length > 3 &&
                                           input.goModels == 0",
                                           checkboxInput("etetko",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "!(input.tko && input.ete) ||
                                           input.itemCols.length <= 3 ||
                                           (input.goModels > 0 && !input.etetko)",
                                           helpText("Don't test.")
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.etetko",
                                           helpText("Tested.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.itemCols.length > 2 &&
                                           input.goModels == 0",
                                           checkboxInput("ete",
                                                         "Include",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.itemCols.length <= 2",
                                           helpText("Too few items.")
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.ete",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && !input.ete &&
                                           !(input.itemCols.length <= 2)",
                                           helpText("Don't test.")
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
                                           input.itemCols.length > 3 &&
                                           input.goModels == 0",
                                           checkboxInput("teqtko",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.teqtko",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "!(input.tko && input.teq) ||
                                           input.itemCols.length <= 3 ||
                                           (input.goModels > 0 && !input.teqtko)",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.ete && input.teq &&
                                           input.itemCols.length > 2 &&
                                           input.goModels == 0",
                                           checkboxInput("teqete",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.teqete",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "!(input.ete && input.teq) ||
                                           input.itemCols.length <= 2 ||
                                           (input.goModels > 0 && !input.teqete)",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.itemCols.length > 1 &&
                                           input.goModels == 0",
                                           checkboxInput("teq",
                                                         "Include",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.itemCols.length < 2",
                                           helpText("Too few items.")
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.teq",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && !input.teq &&
                                           !(input.itemCols.length < 2)",
                                           helpText("Don't test.")
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
                                           input.itemCols.length > 3 &&
                                           input.goModels == 0",
                                           checkboxInput("etptko",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.etptko",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "!(input.tko && input.etp) ||
                                           input.itemCols.length <= 3 ||
                                           (input.goModels > 0 && !input.etptko)",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.ete && input.etp &&
                                           input.itemCols.length > 2 &&
                                           input.goModels == 0",
                                           checkboxInput("etpete",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.etpete",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "!(input.ete && input.etp) ||
                                           input.itemCols.length <= 2 ||
                                           (input.goModels > 0 && !input.etpete)",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2, helpText("Not testable.")),
                                column(2,
                                       conditionalPanel(
                                           "input.itemCols.length > 1 &&
                                           input.goModels == 0",
                                           checkboxInput("etp",
                                                         "Include",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.itemCols.length < 2",
                                           helpText("Too few items.")
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.etp",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && !input.etp &&
                                           !(input.itemCols.length < 2)",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2)
                            ),
                            fluidRow(
                                column(2, HTML("<b>&tau;-paral.</b>")),
                                column(2,
                                       conditionalPanel(
                                           "input.tko && input.tpa &&
                                           input.itemCols.length > 3 &&
                                           input.goModels == 0",
                                           checkboxInput("tpatko",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.tpatko",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "!(input.tko && input.tpa) ||
                                           input.itemCols.length <= 3 ||
                                           (input.goModels > 0 && !input.tpatko)",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.ete && input.tpa &&
                                           input.itemCols.length > 2 &&
                                           input.goModels == 0",
                                           checkboxInput("tpaete",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.tpaete",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "!(input.ete && input.tpa) ||
                                           input.itemCols.length <= 2 ||
                                           (input.goModels > 0 && !input.tpaete)",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.teq && input.tpa &&
                                           input.itemCols.length > 1 &&
                                           input.goModels == 0",
                                           checkboxInput("tpateq",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.tpateq",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "!(input.teq && input.tpa) ||
                                           input.itemCols.length < 2 ||
                                           (input.goModels > 0 && !input.tpateq)",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.etp && input.tpa &&
                                           input.itemCols.length > 1 &&
                                           input.goModels == 0",
                                           checkboxInput("tpaetp",
                                                         "Compare",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.tpaetp",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "!(input.etp && input.tpa) ||
                                           input.itemCols.length < 2 ||
                                           (input.goModels > 0 && !input.tpaetp)",
                                           helpText("Don't test.")
                                       )
                                ),
                                column(2,
                                       conditionalPanel(
                                           "input.itemCols.length > 1 &&
                                           input.goModels == 0",
                                           checkboxInput("tpa",
                                                         "Include",
                                                         value = TRUE)
                                       ),
                                       conditionalPanel(
                                           "input.itemCols.length < 2",
                                           helpText("Too few items.")
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && input.tpa",
                                           helpText("Tested.")
                                       ),
                                       conditionalPanel(
                                           "input.goModels > 0 && !input.tpa &&
                                           !(input.itemCols.length < 2)",
                                           helpText("Don't test.")
                                       )
                                )
                            )
                        ),
                        # End of table -------------------------------------------------------------------------------------
                        tabPanel(
                            "Statistics",
                            htmlOutput("descrTable"),
                            uiOutput("mgDescrTable"),
                            htmlOutput("covMat"),
                            uiOutput("mgCovMat")
                        ),
                        tabPanel(
                            "Correlations",
                            htmlOutput("corrInd"),
                            htmlOutput("corrTableWithCIs"),
                            uiOutput("mgCorrTableTagList")
                        ),
                        tabPanel(
                            "Multivariate Normality",
                            htmlOutput("mvnComment"),
                            htmlOutput("mvnTableUV")
                        )
                    )
                )
            )
        )
    )
)
