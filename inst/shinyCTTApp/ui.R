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
                            title = "Observations per group:",
                            htmlOutput("obsTable")
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

                )
            )
        )
    )
)
