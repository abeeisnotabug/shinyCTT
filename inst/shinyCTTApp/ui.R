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
                               actionButton("goDescrStats",
                                            "Calculate descriptive statistics!")
                           ),
                           mainPanel(
                               h3("Raw data:"),
                               dataTableOutput("dataOverview"),
                               textOutput("workspace")
                           )
                       )
)

fluidPage(

    # To be updated later: nice alignment of item checkbox group! ---------------------
    tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
    # ---------------------------------------------------------------------------------

    navbarPage("shinyCTT",
               id = "navbar",
               inputPanel
               #position = "fixed-top",
               #header = tagList(p(),
              #                  actionButton("goDescrStats",
              #                               "Calculate descriptive statistics!"),
              #                  hr()
              #          )
    )
)
