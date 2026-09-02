## Step 2's box must stay quiet until step 1 has actually handed it a data set.
##
## Both observers in mod-data-subset.R watch input$groupCol / input$groups / input$itemCols,
## none of which is the data. Without a guard they run once when the app starts, with no
## items ticked because there are no tick boxes yet, and report "No item selected. No
## analysis possible." over a data set the user has not chosen.

test_that("no notification is posted before a data set has been selected", {
  notifications <- shiny::reactiveValues(notList = list())

  shiny::testServer(
    dataSubsetServer,
    args = list(
      chosenData = shiny::reactiveVal(NULL),   # step 1's Select has not been pressed
      notifications = notifications,
      frozen = shiny::reactiveVal(FALSE)),
    {
      session$flushReact()
      expect_null(notifications$notList$numItems)
    })
})

test_that("the item count is still reported once the user empties the tick boxes", {
  notifications <- shiny::reactiveValues(notList = list())

  shiny::testServer(
    dataSubsetServer,
    args = list(
      chosenData = shiny::reactiveVal(rtdata),
      notifications = notifications,
      frozen = shiny::reactiveVal(FALSE)),
    {
      # What the browser sends back once the two choosers exist: every item ticked, no
      # group column.
      session$setInputs(groupCol = "noGroupSelected",
                        groups = character(0),
                        itemCols = paste0("item_", 1:6))
      expect_null(notifications$notList$numItems)

      # The user unticks everything. input$itemCols is empty for a real reason this time.
      session$setInputs(itemCols = NULL)
      expect_false(is.null(notifications$notList$numItems))
    })
})

## Step 1 loads whatever data-like object is first in the workspace as soon as the app
## starts, to fill the preview table. Anything that gets past its filter but cannot be made
## into a data frame used to raise its error inside an observer, which ends the session -
## the user lost the app on launch instead of being told the data set is unusable.

test_that("a workspace object that cannot become a data frame is reported, not fatal", {
  skip_if_not_installed("Matrix")

  # A sparse matrix has a dim and is not character, so step 1's chooser offers it; it is
  # also one of the few things data.frame() refuses outright.
  assign("unreadableObject", Matrix::Matrix(0, 3, 3, sparse = TRUE), envir = globalenv())
  on.exit(rm("unreadableObject", envir = globalenv()))

  notifications <- shiny::reactiveValues(notList = list())

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = notifications, frozen = shiny::reactiveVal(FALSE)),
    {
      session$setInputs(source = "Workspace", objectFromWorkspace = "unreadableObject")

      expect_null(raw())
      expect_false(is.null(notifications$notList$unreadable))
    })
})

test_that("a readable workspace object clears the unreadable notification again", {
  assign("readableObject", rtdata, envir = globalenv())
  on.exit(rm("readableObject", envir = globalenv()))

  notifications <- shiny::reactiveValues(notList = list())

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = notifications, frozen = shiny::reactiveVal(FALSE)),
    {
      session$setInputs(source = "Workspace", objectFromWorkspace = "readableObject")

      expect_null(notifications$notList$unreadable)
      expect_equal(nrow(raw()), nrow(rtdata))
      expect_equal(colnames(raw()), colnames(rtdata))
    })
})
