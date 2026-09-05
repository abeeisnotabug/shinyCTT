## Namespacing inside the module files ---------------------------------------------------
##
## Inside a Shiny module every id has to be put through ns(). conditionalPanel() is the
## dangerous one: it needs `ns = ns` for its condition *and* the inputs it shows need ns()
## on their ids, and if either is missing the panel simply never appears. There is no
## error, in the browser or in R. These tests are the guard against that.
##
## They read R/mod-*.R as code, not as text, because the calls run over several lines.

## The walker itself ---------------------------------------------------------------------
# If these two go wrong, everything below passes for the wrong reason.

test_that("findCalls finds a call that is nested and split over several lines", {
  code <- parse(text = "
    fluidRow(
      column(
        2,
        conditionalPanel(
          paste0('input.itemCols && ', enoughItems),
          checkboxInput(ns(rowModel), 'Include', value = TRUE),
          ns = ns)))")

  panels <- findCalls(code, "conditionalPanel")

  expect_length(panels, 1)
  expect_true("ns" %in% names(as.list(panels[[1]])))
})

test_that("findCalls finds a call written with its package in front of it", {
  code <- parse(text = "tabPanel('x', DT::dataTableOutput(ns('scores')))")

  expect_length(findCalls(code, "dataTableOutput"), 1)
})

test_that("findCalls survives an empty argument, as in x[, 1]", {
  code <- parse(text = "f <- function(m) conditionalPanel(cond, plotOutput(ns('p')))")

  expect_length(findCalls(code, "conditionalPanel"), 1)
  expect_length(findCalls(code, "plotOutput"), 1)

  expect_no_error(findCalls(parse(text = "y <- m[, 1]"), "conditionalPanel"))
})

test_that("the walker sees the real conditionalPanels in comparisonGrid()", {
  # comparisonGrid() writes its panels in a loop, so this also checks the walker copes
  # with calls built inside for() blocks. Every one of them is already namespaced.
  skip_if(is.na(packageSourceDir()), "R/ sources are not available here")

  panels <- findCalls(parse(file.path(packageSourceDir(), "fun-comparisonGrid.R")),
                      "conditionalPanel")

  expect_gt(length(panels), 0)
  for (panel in panels) expect_true("ns" %in% names(as.list(panel)))
})

test_that("idArgument reads the id whether it is named or not", {
  expect_equal(idArgument(quote(checkboxInput("tko", "Include"))), "tko")
  expect_equal(idArgument(quote(checkboxInput(inputId = "tko", label = "Include"))), "tko")
  expect_equal(idArgument(quote(plotOutput(ns("hierPlot")))), quote(ns("hierPlot")))
})

## The guard ------------------------------------------------------------------------------

test_that("every conditionalPanel in a module file passes ns", {
  skip_if(is.na(packageSourceDir()), "R/ sources are not available here")

  moduleFiles <- list.files(packageSourceDir(), pattern = "^mod-.*\\.R$", full.names = TRUE)
  skip_if(length(moduleFiles) == 0, "there are no module files yet")

  for (moduleFile in moduleFiles) {
    for (panel in findCalls(parse(moduleFile), "conditionalPanel")) {

      expect_true(
        "ns" %in% names(as.list(panel)),
        label = sprintf("conditionalPanel in %s is missing ns = ns:\n%s",
                        basename(moduleFile),
                        paste(deparse(panel), collapse = "\n")))
    }
  }

  # A module file with no conditionalPanel in it is fine; say so, so that testthat does not
  # report a test that made no checks at all.
  succeed()
})

test_that("no shinyjs call in a module file puts its id through ns()", {
  skip_if(is.na(packageSourceDir()), "R/ sources are not available here")

  moduleFiles <- list.files(packageSourceDir(), pattern = "^mod-.*\\.R$", full.names = TRUE)
  skip_if(length(moduleFiles) == 0, "there are no module files yet")

  for (moduleFile in moduleFiles) {
    code <- parse(moduleFile)

    for (functionName in shinyjsIdFunctions()) {
      for (call in findCalls(code, functionName)) {

        # Only shinyjs::show() and friends are meant here, not a same-named function of
        # something else, so look at the calls that were written with the package in front.
        if (!grepl("^shinyjs::", deparse(call[[1]]))) next

        id <- idArgument(call)
        idIsNamespaced <- is.call(id) && identical(deparse(id[[1]]), "ns")

        expect_false(
          idIsNamespaced,
          label = sprintf("shinyjs::%s in %s names its id twice - drop the ns():\n%s",
                          functionName,
                          basename(moduleFile),
                          paste(deparse(call), collapse = "\n")))
      }
    }
  }

  succeed()
})

test_that("every input and output in a module file has its id put through ns()", {
  skip_if(is.na(packageSourceDir()), "R/ sources are not available here")

  moduleFiles <- list.files(packageSourceDir(), pattern = "^mod-.*\\.R$", full.names = TRUE)
  skip_if(length(moduleFiles) == 0, "there are no module files yet")

  for (moduleFile in moduleFiles) {
    code <- parse(moduleFile)

    for (functionName in idMakingFunctions()) {
      for (call in findCalls(code, functionName)) {

        id <- idArgument(call)

        # An id built by ns() - ns("x"), or ns(paste0(...)) - is what we want to see. A
        # bare string or a plain variable means the id will not carry the module's name.
        idIsNamespaced <- is.call(id) && identical(deparse(id[[1]]), "ns")

        expect_true(
          idIsNamespaced,
          label = sprintf("%s in %s does not put its id through ns():\n%s",
                          functionName,
                          basename(moduleFile),
                          paste(deparse(call), collapse = "\n")))
      }
    }
  }

  succeed()
})
