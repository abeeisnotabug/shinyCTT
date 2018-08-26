fluidPage(

    # Application title
    titlePanel("shinyCTTApp"),

    fluidRow(

        column(3,

            wellPanel(
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
                                 # Input: Checkbox if file has header ----
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
                                              selected = '"'))
                #conditionalPanel(condition = "input.source == 'SPSS'"),
            ),

            wellPanel(
                textInput("itemColsString", "Specify item columns:", value = "2:7"),
                div("Type in item names in double quotes OR the column numbers without quotes
                    (they are given next to the item names in the table in brackets).
                    Separate by commas, don't mix quoted item names and column numbers."),
                hr(),
                textInput("groupColsString", "Specify group column:", value = "1")
            ),

            wellPanel(
                actionButton("descrStats", "Calculate descriptive statistics!")
            )
        ),
        column(9,
               h3("Overview of the data:"),
               tableOutput("dataOverview"),
               h3(textOutput("caption1", container = span)),
               tableOutput("descrTable"))
    )
)
