## What step 1 reads, and which columns step 2 offers as items.
##
## Both were rewritten to say the same thing once instead of three times - the reader is a
## switch() on input$source, the factor columns are found once into factorColumns, and the
## numeric columns are found once into the possibleItemColumns() reactive. Nothing here
## needs a browser: every module is *Server(id, <plain reactives>), which testServer drives.

## Step 1 reads from all three sources ----
## The three names in the switch() are the three the source dropdown offers. A name that
## reached the switch() without being one of them would give NULL, and data.frame(NULL)
## succeeds - so raw() would fill with an empty table and nothing would say the read had
## failed. The req() above the switch() is what stops that, and these three pin that each
## name really does reach a reader.

test_that("a data set is read from the workspace", {
  assign("workspaceData", rtdata, envir = globalenv())
  on.exit(rm("workspaceData", envir = globalenv()))

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      session$setInputs(source = "Workspace", objectFromWorkspace = "workspaceData")

      expect_equal(nrow(raw()), nrow(rtdata))
      expect_equal(colnames(raw()), colnames(rtdata))
    })
})

test_that("a data set is read from a CSV file", {
  csvPath <- tempfile(fileext = ".csv")
  on.exit(unlink(csvPath))
  utils::write.csv(rtdata, csvPath, row.names = FALSE)

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      # What the file chooser and the three CSV controls send: read.csv is given all four.
      session$setInputs(source = "CSV",
                        CSVFile = list(datapath = csvPath),
                        header = TRUE,
                        sep = ",",
                        quote = '"')

      expect_equal(nrow(raw()), nrow(rtdata))
      expect_equal(colnames(raw()), colnames(rtdata))
    })
})

test_that("a data set is read from an SPSS file", {
  savPath <- tempfile(fileext = ".sav")
  on.exit(unlink(savPath))
  haven::write_sav(rtdata, savPath)

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      session$setInputs(source = "SPSS", SPSSFile = list(datapath = savPath))

      expect_equal(nrow(raw()), nrow(rtdata))
      expect_equal(colnames(raw()), colnames(rtdata))
    })
})

## Factor columns become text ----

test_that("a factor column arrives as text, and the numbers beside it are untouched", {
  withFactor <- data.frame(
    grp = factor(c("A", "B", "A", "B")),
    item_1 = c(1.5, 2.5, 3.5, 4.5),
    item_2 = c(2, 3, 4, 5))

  assign("withFactor", withFactor, envir = globalenv())
  on.exit(rm("withFactor", envir = globalenv()))

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      session$setInputs(source = "Workspace", objectFromWorkspace = "withFactor")

      expect_type(raw()$grp, "character")
      expect_equal(raw()$grp, c("A", "B", "A", "B"))
      expect_equal(raw()$item_1, withFactor$item_1)
    })
})

test_that("a data set with no factor column comes through unchanged", {
  assign("noFactors", rtdata, envir = globalenv())
  on.exit(rm("noFactors", envir = globalenv()))

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      session$setInputs(source = "Workspace", objectFromWorkspace = "noFactors")

      expect_equal(raw()$item_1, rtdata$item_1)
    })
})

## Which columns step 2 offers as items ----
## possibleItemColumns() is read by the item tick boxes and by both Select all /
## Unselect all observers, so all three offer the same list by construction. These pin what
## that list holds.

test_that("only the numeric columns are offered as items", {
  mixedColumns <- data.frame(
    label = c("a", "b", "c", "d"),          # text: not an item
    item_1 = c(1.5, 2.5, 3.5, 4.5),
    item_2 = c(2, 3, 4, 5),
    stringsAsFactors = FALSE)

  shiny::testServer(
    dataSubsetServer,
    args = list(chosenData = shiny::reactiveVal(mixedColumns),
                notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      expect_equal(possibleItemColumns(), c("item_1", "item_2"))
    })
})

test_that("Select all and Unselect all count the same columns the tick boxes were built from", {
  shiny::testServer(
    dataSubsetServer,
    args = list(chosenData = shiny::reactiveVal(rtdata),
                notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      session$setInputs(groupCol = "noGroupSelected",
                        groups = character(0),
                        itemCols = itemNames())

      # Drawing the tick boxes writes the count into the blue box at the top.
      output$itemColsChooser
      expect_equal(itemColsRV(), length(possibleItemColumns()))

      # Both links write it again, and must not arrive at a different number.
      session$setInputs(selectall = 1)
      expect_equal(itemColsRV(), length(possibleItemColumns()))

      session$setInputs(deselectall = 1)
      expect_equal(itemColsRV(), length(possibleItemColumns()))
    })
})
