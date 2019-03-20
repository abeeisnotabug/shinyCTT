shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
        title = "shinyCTT",
        shinydashboard::dropdownMenuOutput("infoMenu")
    ),
    shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenuOutput("dataMenuOut")
    ),
    shinydashboard::dashboardBody(
        tags$head(tags$style(".checkbox-inline {margin: 0 !important;}"),
                  tags$style(HTML(".navbar-custom-menu>.navbar-nav>li>.dropdown-menu {width:600px;}"))),
        shinyjs::useShinyjs(),
        shinydashboard::tabItems(
            shinydashboard::tabItem(
                tabName = "dataSelectionTab",
                fluidRow(
                    column(
                        width = 3,
                        shinydashboard::box(
                            width = NULL,
                            selectInput("source", "1a. Choose source of data",
                                        choices = c("Workspace", "CSV", "SPSS"))
                        ),
                        shinydashboard::box(
                            width = NULL,
                            conditionalPanel(
                                condition = "input.source == 'Workspace'",
                                uiOutput("objectsInWorkspace")
                            ),
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
                                             selected = '"')
                            ),
                            conditionalPanel(
                                condition = "input.source == 'SPSS'",
                                fileInput("SPSSFile", "1b. Choose SPSS File",
                                          multiple = FALSE,
                                          accept = c(".sav",
                                                     ".zsav",
                                                     ".por"))
                            )
                        ),
                        shinydashboard::box(
                            width = NULL,
                            actionButton("dataSelectButton", "Select", width = "100%")
                        )
                    ),
                    column(
                        width = 9,
                        shinydashboard::box(
                            width = NULL,
                            title = "Raw data:",
                            dataTableOutput("dataOverview")
                        )
                    )
                )
            ),
            shinydashboard::tabItem(
                tabName = "subsetSelectionTab",
                fluidRow(
                    shinydashboard::valueBoxOutput("itemInfoBox"),
                    shinydashboard::valueBoxOutput("groupInfoBox"),
                    shinydashboard::valueBoxOutput("naInfoBox")
                ),
                fluidRow(
                    column(
                        width = 4,
                        shinydashboard::box(
                            width = NULL,
                            uiOutput("itemColsChooser")
                        ),
                        shinydashboard::box(
                            width = NULL,
                            uiOutput("groupColChooser"),
                            conditionalPanel(
                                condition = "input.groupCol != 'noGroupSelected'",
                                uiOutput("groupChooser")
                            )
                        ),
                        shinydashboard::box(
                            width = NULL,
                            actionButton("subsetSelectButton", "Select", width = "100%")
                        )
                    ),
                    column(
                        width = 4,
                        shinydashboard::box(
                            width = NULL,
                            title = "Observations:",
                            htmlOutput("obsTable")
                        ),
                        shinydashboard::box(
                            width = NULL,
                            title = "Observations per group:",
                            htmlOutput("obsPerGroupTable")
                        )
                    ),
                    column(
                        width = 4,
                        shinydashboard::box(
                            width = NULL,
                            title = "Missing values per column:",
                            htmlOutput("naTable")
                        )
                    )
                )
            ),
            shinydashboard::tabItem(
                tabName = "statisticsTab",
                fluidRow(
                    htmlOutput("descrBox"),
                    htmlOutput("histBox")
                ),
                fluidRow(
                    htmlOutput("covMatBox")
                )
            ),
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
                                selected = "ML"
                            ),
                            numericInput(
                                "corrIndSL",
                                "Enter the significance level for this test:",
                                value = 0.05,
                                min = 0,
                                max = 1,
                                step = 0.001
                            ),
                            htmlOutput("corrInd")
                        ),
                        shinydashboard::box(
                            width = NULL,
                            title = "Correlation Table with Confidence Intervals:",
                            radioButtons(
                                "corrTabNA",
                                "Choose how to handle missing values:",
                                choices = c("Use pairwise complete observations" = "pairwise.complete.obs",
                                            "Use only complete observations" = "complete.obs"),
                                selected = "pairwise.complete.obs"
                            ),
                            numericInput(
                                "corrTabSL",
                                "Enter the significance level for the correlation tests:",
                                value = 0.05,
                                min = 0,
                                max = 1,
                                step = 0.001
                            )
                        )
                    ),
                    column(
                        width = 8,
                        htmlOutput("scatterPlotBox")
                    )
                ),
                fluidRow(
                    htmlOutput("corrTableBox")
                )
            ),
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
                                step = 0.001
                            )
                        ),
                        shinydashboard::box(
                            width = NULL,
                            title = "Test on Multivariate Normality:",
                            htmlOutput("mvnComment")
                        ),
                        shinydashboard::box(
                            width = NULL,
                            title = "Tests on Univariate Normality:",
                            htmlOutput("mvnTable")
                        )
                    ),
                    column(
                        width = 8,
                        htmlOutput("mvnPlotBox")
                    )
                )
            ),
            shinydashboard::tabItem(
                tabName = "testParamTab",
                fluidRow(
                    column(
                        width = 5,
                        shinydashboard::box(
                            width = NULL,
                            fluidRow(
                                column(
                                    width = 6,
                                    radioButtons(
                                        "estimator",
                                        "Choose estimator:",
                                        choices = c("Maximum Likelihood" = "ML",
                                                    "Robust Maximum Likelihood" = "MLR"),
                                        selected = "ML"
                                    )
                                ),
                                column(
                                    width = 6,
                                    numericInput(
                                        "sigLvl",
                                        "Enter the significance level:",
                                        value = 0.05,
                                        min = 0,
                                        max = 1,
                                        step = 0.001
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 6,
                                    shinyjs::disabled(
                                        radioButtons(
                                            "para",
                                            "Choose parameterization:",
                                            choices = c("Std. Eta", "Std. Alpha")
                                        )
                                    )
                                ),
                                column(
                                    width = 6,
                                    shinyjs::disabled(
                                        checkboxInput(
                                            "doMg",
                                            "Perform Multigroup Tests",
                                            value = FALSE
                                        )
                                    )
                                )
                            )
                        ),
                        shinydashboard::box(
                            width = NULL,
                            actionButton("goModels", "Test the models", width = "100%")
                        )
                    ),
                    column(
                        width = 7,
                        shinydashboard::box(
                            width = NULL,
                            title = "Choose models to test and compare:",
                            # A table with all the models to test in checkboxes ------------------------------------------------
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
                            # End of table -------------------------------------------------------------------------------------
                        )
                    )
                )
            ),
            shinydashboard::tabItem(
                tabName = "panelModelTests",
                tabsetPanel(id = "compTabsets", type = "pills")
            ),
            shinydashboard::tabItem(
                tabName = "panelParTables",
                tabsetPanel(id = "parTabsets", type = "pills")
            )
        )
    )
)
