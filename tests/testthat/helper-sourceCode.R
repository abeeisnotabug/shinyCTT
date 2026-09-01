## Reading the package's own source as code, for the namespacing checks.
##
## The checks in test-moduleNamespacing.R have to look at calls that are written over
## several lines, so they read the files with parse() and walk the result rather than
## searching the text.

## Where the package's R/ directory is, or NA if the sources are not there.
##
## Under devtools::test() the tests run in <package>/tests/testthat, so R/ is two levels up.
## Under R CMD check they run in a copy that only has the *installed* package, where there
## are no .R files to look at - modelFamily.R is the marker for telling the two apart.
packageSourceDir <- function() {
  candidate <- test_path("..", "..", "R")

  if (file.exists(file.path(candidate, "modelFamily.R"))) candidate else NA_character_
}

## Every call to `functionName` anywhere inside `code`, as a list of calls.
##
## `code` is whatever parse() gives back, or any part of it. The search goes into every
## part of every call, so a conditionalPanel() nested five levels down inside a fluidRow()
## is still found.
findCalls <- function(code, functionName) {
  found <- list()

  if (is.call(code)) {

    # A call can be written bare or with a package in front of it, as DT::dataTableOutput
    # is. Drop anything up to the last "::" before comparing.
    calledName <- sub("^.*::", "", deparse(code[[1]]))

    if (identical(calledName, functionName)) found <- c(found, list(code))
  }

  # Only these have parts worth going into. as.list() of a name gives back a list holding
  # that same name, so recursing into one would never end.
  if (!(is.call(code) || is.expression(code) || is.pairlist(code) || is.list(code)))
    return(found)

  parts <- as.list(code)

  for (position in seq_along(parts)) {

    # An argument can be left empty - the gap in x[, 1] - and looking at such an argument
    # directly is an error. parts[position] keeps it wrapped in a list, which is safe.
    if (identical(parts[position], list(quote(expr = )))) next

    found <- c(found, findCalls(parts[[position]], functionName))
  }

  found
}

## The id argument of a call that creates an input or an output.
##
## Every one of these functions takes the id first, and some also accept it by name, so
## look for the name first and fall back to the first unnamed argument.
idArgument <- function(call) {
  arguments <- as.list(call)[-1]
  argumentNames <- names(arguments)

  if (is.null(argumentNames)) argumentNames <- rep("", length(arguments))

  for (idName in c("inputId", "outputId", "id")) {
    if (idName %in% argumentNames) return(arguments[[idName]])
  }

  unnamed <- which(argumentNames == "")

  if (length(unnamed) == 0) return(NULL)

  arguments[[unnamed[1]]]
}

## The shinyjs functions that take an id. Inside a module shinyjs puts the module's name in
## front of the id itself, so these must be given the *plain* id: an ns() there names the
## control twice and the call silently does nothing.
shinyjsIdFunctions <- function() {
  c("show", "hide", "toggle", "enable", "disable", "toggleState", "reset",
    "addClass", "removeClass", "toggleClass", "delay", "html")
}

## The functions that make an input or an output, and so need a namespaced id inside a
## module. Kept as one list so both the check and its own test read the same names.
idMakingFunctions <- function() {
  c("actionButton", "actionLink", "checkboxInput", "checkboxGroupInput", "dateInput",
    "downloadButton", "fileInput", "numericInput", "radioButtons", "selectInput",
    "sliderInput", "textInput", "textAreaInput", "varSelectInput",
    "htmlOutput", "imageOutput", "plotOutput", "tableOutput", "textOutput", "uiOutput",
    "verbatimTextOutput", "dataTableOutput")
}
