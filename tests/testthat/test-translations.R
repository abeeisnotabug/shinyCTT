## tr() and germanText() are the whole translation mechanism (R/translations.R): tr(text)
## looks text up as its own key in germanText(), and falls back to English whenever the key
## is missing or its German is still "". These tests pin that fallback behaviour and check
## the table itself for the two mistakes that would break a lookup silently: a duplicated
## key, and an empty one.

test_that("an unknown string comes back unchanged in both languages", {
  oldOption <- getOption("shinyCTT.language")
  on.exit(options(shinyCTT.language = oldOption))

  unknownString <- "This string is not in germanText()."

  options(shinyCTT.language = "en")
  expect_equal(tr(unknownString), unknownString)

  options(shinyCTT.language = "de")
  expect_equal(tr(unknownString), unknownString)
})

test_that("a string whose German is still \"\" comes back in English", {
  oldOption <- getOption("shinyCTT.language")
  on.exit(options(shinyCTT.language = oldOption))

  # "Raw data:" is in the table with "" as its German, same as every other entry until the
  # package owner fills one in.
  untranslated <- "Raw data:"
  expect_equal(unname(germanText()[untranslated]), "")

  options(shinyCTT.language = "de")
  expect_equal(tr(untranslated), untranslated)
})

test_that("the keys of germanText() have no duplicates and none is empty", {
  keys <- names(germanText())

  expect_false(any(duplicated(keys)))
  expect_false(any(keys == ""))
  expect_false(any(is.na(keys)))
})
