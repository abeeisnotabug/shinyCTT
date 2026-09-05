## The Testing Parameters box (R/mod-testing-params.R).
##
## What it reports back, and the one thing on it that cannot be reached from the page at
## all: the message from a run that failed, which needs the fitting in server.R to throw.
##
## Two things about the grid are deliberately not here, both because testServer() cannot
## see them: the unticking of models the item count cannot support goes out as an
## updateCheckboxInput() message rather than as an input, and output$nItemsChosen is a
## reactive assigned to an output, which the mock session refuses to read. Both are checked
## by looking at the grid in a browser.

## The arguments the box needs, with every reactive stood still. Each test overrides the
## one or two it is about.
paramsArgs <- function(nItems = 6, hasGroups = FALSE, useFIML = FALSE,
                       recommended = NULL, modelFits = NULL, fitError = NULL,
                       frozen = FALSE) {
  list(
    nItems = shiny::reactiveVal(nItems),
    subsetChosen = shiny::reactiveVal(NULL),
    hasGroups = shiny::reactiveVal(hasGroups),
    useFIML = shiny::reactiveVal(useFIML),
    recommended = shiny::reactiveVal(recommended),
    modelFits = shiny::reactiveVal(modelFits),
    fitError = shiny::reactiveVal(fitError),
    notifications = shiny::reactiveValues(notList = list()),
    frozen = shiny::reactiveVal(frozen))
}

test_that("it hands back the models and comparisons that are ticked", {
  family <- cttModelFamily()

  shiny::testServer(testingParamsServer, args = paramsArgs(), {

    # What the browser sends back with every box ticked, which is how the grid is drawn.
    ticked <- as.list(rep(TRUE, length(family$names) + length(family$comparable)))
    names(ticked) <- c(family$names, family$comparable)
    do.call(session$setInputs, ticked)

    # The box hands its answers back in a list, which testServer() puts in session$returned.
    answers <- session$returned

    expect_equal(unname(answers$modelsToTest()), unname(family$names))
    expect_equal(unname(answers$comps()), unname(family$comparable))

    # Untick tau-congeneric and the one comparison that uses it against ess. tau-equiv.
    session$setInputs(tko = FALSE, etetko = FALSE)

    expect_false("tko" %in% answers$modelsToTest())
    expect_false("etetko" %in% answers$comps())
  })
})

test_that("a run that failed is drawn under the button, and nothing is drawn otherwise", {
  shiny::testServer(testingParamsServer, args = paramsArgs(fitError = NULL), {
    session$flushReact()
    expect_null(output$goModelsError)
  })

  shiny::testServer(
    testingParamsServer,
    args = paramsArgs(fitError = "some variables have no values"),
    {
      session$flushReact()
      expect_true(grepl("some variables have no values", output$goModelsError$html,
                        fixed = TRUE))
    })
})

test_that("the button asks for a refit only while the estimator disagrees with the fits", {
  fittedWithML <- list(single = list(estimator = "ML"))

  shiny::testServer(
    testingParamsServer,
    args = paramsArgs(modelFits = fittedWithML),
    {
      session$setInputs(estimator = "ML")
      expect_null(output$refitPendingNote)

      session$setInputs(estimator = "MLR")
      expect_false(is.null(output$refitPendingNote))

      # Changing it back clears the note by itself; nothing is written down.
      session$setInputs(estimator = "ML")
      expect_null(output$refitPendingNote)
    })
})

test_that("the normality test's recommendation is reported next to the buttons", {
  notifications <- shiny::reactiveValues(notList = list())
  args <- paramsArgs(recommended = "MLR")
  args$notifications <- notifications

  shiny::testServer(testingParamsServer, args = args, {
    session$flushReact()

    expect_false(is.null(output$estimatorNote))
    expect_equal(notifications$notList$estUpdate$status, "warning")
  })
})
