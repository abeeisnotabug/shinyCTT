## What step 1 reads, and which columns step 2 offers as items.
##
## Both were rewritten to say the same thing once instead of three times - the reader is a
## switch() on input$source, the factor columns are found once into factorColumns, and the
## numeric columns are found once into the possibleItemColumns() reactive. Nothing here
## needs a browser: every module is *Server(id, <plain reactives>), which testServer drives.

## Step 1 reads from all four sources ----
## The four names in the switch() are the four the source dropdown offers. A name that
## reached the switch() without being one of them would give NULL, and data.frame(NULL)
## succeeds - so raw() would fill with an empty table and nothing would say the read had
## failed. The req() above the switch() is what stops that, and these pin that each name
## really does reach a reader.

test_that("a data set is read from the workspace", {
  assign("workspaceData", rtdata, envir = globalenv())
  on.exit(rm("workspaceData", envir = globalenv()))

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      session$setInputs(source = "Workspace", chosenObject = "workspaceData")

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

## An uploaded R data file ----
## The two kinds are read differently: .RData holds any number of objects and gives their
## names, so one has to be picked; .rds holds one and has no name for it.

test_that("an object is picked out of an uploaded .RData", {
  rdataPath <- tempfile(fileext = ".RData")
  on.exit(unlink(rdataPath))

  first <- rtdata
  second <- rtdata[1:10, ]
  save(first, second, file = rdataPath)

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      session$setInputs(
        source = "RData",
        RDataFile = list(name = "twoObjects.RData", datapath = rdataPath),
        chosenObject = "second")

      expect_equal(nrow(raw()), 10)
      expect_equal(colnames(raw()), colnames(rtdata))

      # The name the factor score files are called after, and what makeRCode() writes.
      expect_equal(session$returned$name(), "second")
      expect_equal(session$returned$descriptor()$type, "RData")
      expect_equal(session$returned$descriptor()$object, "second")

      # Picking the other one must not read the file again from scratch and lose the pick.
      session$setInputs(chosenObject = "first")
      expect_equal(nrow(raw()), nrow(rtdata))
    })
})

test_that("an uploaded .rds is the data itself, with no object to pick", {
  rdsPath <- tempfile(fileext = ".rds")
  on.exit(unlink(rdsPath))

  saveRDS(rtdata, rdsPath)

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      session$setInputs(
        source = "RData",
        RDataFile = list(name = "myScores.rds", datapath = rdsPath))

      expect_equal(nrow(raw()), nrow(rtdata))
      expect_equal(colnames(raw()), colnames(rtdata))

      # No name for what an .rds holds, so the file's own name stands in.
      expect_equal(session$returned$name(), "myScores")
      expect_equal(session$returned$descriptor()$type, "RDS")
      expect_equal(session$returned$descriptor()$name, "myScores.rds")
    })
})

test_that("an R data file holding no data set says so", {
  nothingUsable <- tempfile(fileext = ".RData")
  aTable <- tempfile(fileext = ".RData")
  on.exit(unlink(c(nothingUsable, aTable)))

  notes <- "hello"
  lookup <- list(a = 1, b = 2)
  save(notes, lookup, file = nothingUsable)

  alpha <- data.frame(item_1 = c(1, 2, 3), item_2 = c(4, 5, 6))
  save(alpha, file = aTable)

  notifications <- shiny::reactiveValues(notList = list())

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = notifications, frozen = shiny::reactiveVal(FALSE)),
    {
      # The file reads fine - there is simply nothing in it the chooser could offer, which
      # used to leave the user with a file they had just picked and nothing on the screen.
      session$setInputs(
        source = "RData",
        RDataFile = list(name = "nothing.RData", datapath = nothingUsable))

      expect_null(raw())
      expect_false(is.null(notifications$notList$noDataset))

      # A file that does hold one clears the message again.
      session$setInputs(
        RDataFile = list(name = "alpha.RData", datapath = aTable),
        chosenObject = "alpha")

      expect_null(notifications$notList$noDataset)
      expect_equal(nrow(raw()), 3)
    })
})

test_that("an R data file that cannot be read is reported, not fatal", {
  notARDataFile <- tempfile(fileext = ".RData")
  on.exit(unlink(notARDataFile))
  writeLines("this is not an R data file", notARDataFile)

  notifications <- shiny::reactiveValues(notList = list())

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = notifications, frozen = shiny::reactiveVal(FALSE)),
    {
      # load() warns about the magic number before it gives up, and that warning reaches
      # the console rather than the user.
      suppressWarnings(session$setInputs(
        source = "RData",
        RDataFile = list(name = "broken.RData", datapath = notARDataFile),
        chosenObject = "anything"))

      expect_null(raw())
      expect_false(is.null(notifications$notList$unreadable))
    })
})

## The data sets the app was started with ----
## shinyCTTApp(data = ) puts a named list of data frames into options(shinyCTT.data), and step
## 1 offers them as a source of their own, first on the list so a visitor opens the app on
## them. The launcher checks the shape of the list; these check what step 1 does with it.

test_that("a data set the app came with is read like any other", {
  previous <- options(shinyCTT.data = list(scores = rtdata, half = rtdata[1:20, ]))
  on.exit(options(previous))

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      session$setInputs(source = "Supplied", chosenObject = "half")

      expect_equal(nrow(raw()), 20)
      expect_equal(colnames(raw()), colnames(rtdata))
      expect_equal(session$returned$name(), "half")
      expect_equal(session$returned$descriptor()$type, "Supplied")
      expect_equal(session$returned$descriptor()$object, "half")

      # Both of them are on offer, and the other one can be picked.
      expect_equal(pickableObjects(), c("half", "scores"))

      session$setInputs(chosenObject = "scores")
      expect_equal(nrow(raw()), nrow(rtdata))
    })
})

test_that("the source list offers the supplied data first, and only when there is any", {
  previous <- options(shinyCTT.data = NULL, shinyCTT.workspace = TRUE)
  on.exit(options(previous))

  without <- as.character(dataSourceUI("dataSource"))
  expect_false(grepl("Supplied data", without, fixed = TRUE))

  options(shinyCTT.data = list(scores = rtdata))
  with <- as.character(dataSourceUI("dataSource"))

  expect_true(grepl("Supplied data", with, fixed = TRUE))

  # First on the list is what a visitor opens the app on.
  expect_lt(regexpr("Supplied data", with, fixed = TRUE),
            regexpr("Workspace", with, fixed = TRUE))
})

## Hosting: the workspace is only offered when there is a console behind the app ----
## globalenv() is the visitor's own only when the app was started from their console. On a
## server it holds whatever the person who put the app up left there, so both halves of
## step 1 have to stop offering it - the source list, and the reader behind it.

test_that("the source list drops the workspace when it is not on offer", {
  previous <- options(shinyCTT.workspace = FALSE)
  on.exit(options(previous))

  hosted <- as.character(dataSourceUI("dataSource"))

  options(shinyCTT.workspace = TRUE)
  local <- as.character(dataSourceUI("dataSource"))

  expect_false(grepl("Workspace", hosted, fixed = TRUE))
  expect_true(grepl("Workspace", local, fixed = TRUE))

  # The R data file upload is there either way - it is the way in when the workspace is not.
  expect_true(grepl("dataSource-RDataFile", hosted, fixed = TRUE))
  expect_true(grepl("dataSource-RDataFile", local, fixed = TRUE))
})

test_that("nothing is read from the workspace when it is not on offer", {
  previous <- options(shinyCTT.workspace = FALSE)
  assign("workspaceData", rtdata, envir = globalenv())
  on.exit({
    options(previous)
    rm("workspaceData", envir = globalenv())
  })

  shiny::testServer(
    dataSourceServer,
    args = list(notifications = shiny::reactiveValues(notList = list()),
                frozen = shiny::reactiveVal(FALSE)),
    {
      # What a hand-made request could send: the name is not in the dropdown any more, but
      # the input still reaches the server.
      session$setInputs(source = "Workspace", chosenObject = "workspaceData")

      expect_null(raw())

      chooser <- paste(as.character(output$objectChooser), collapse = "")
      expect_false(grepl("workspaceData", chooser, fixed = TRUE))
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
      session$setInputs(source = "Workspace", chosenObject = "withFactor")

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
      session$setInputs(source = "Workspace", chosenObject = "noFactors")

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
