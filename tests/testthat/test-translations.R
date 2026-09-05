## The text system: inst/translations.csv holds every piece of text the app shows, one row
## per piece, named by a short key. R/helpers-translations.R turns a key into text with tr().
##
## Two kinds of test here.
##
## The first kind runs tr() and checks what comes back - the fallbacks, mainly, because a
## half-translated file has to keep working.
##
## The second kind never runs the app. It reads the package's own source with parse(),
## collects every tr("...") in it, and checks the names against the file in both directions.
## This is the safety net named keys need: with the English written into the code a typo was
## visible on screen, but tr("subset.itemz.label") looks fine until someone opens that page.

## the file itself ----

translationFile <- function() {
  inRepo <- test_path("..", "..", "inst", "translations.csv")

  if (file.exists(inRepo)) inRepo else system.file("translations.csv", package = "shinyCTT")
}

## na.strings = character(0) for the same reason loadTranslations() uses it: one row's text
## is the word "NA", and read.csv would otherwise turn it into a missing value.
readTranslationFile <- function() {
  utils::read.csv(translationFile(), colClasses = "character", encoding = "UTF-8",
                  na.strings = character(0))
}

test_that("the file has the three expected columns and no broken rows", {
  table <- readTranslationFile()

  expect_named(table, c("key", appLanguages))
  expect_gt(nrow(table), 0)

  expect_false(any(duplicated(table$key)))
  expect_false(any(is.na(table$key)))
  expect_false(any(!nzchar(table$key)))

  # Every row must have English. German may be empty - nobody has translated it yet.
  expect_false(any(is.na(table$en)))
  expect_false(any(!nzchar(table$en)))
})

test_that("every key is a name, not a sentence", {
  keys <- readTranslationFile()$key

  # lowercase words joined by dots, at least two of them: "common.select", "subset.na.info".
  wrongShape <- keys[!grepl("^[a-z][a-z0-9]*(\\.[a-z0-9]+)+$", keys)]

  expect_equal(wrongShape, character(0))
})

test_that("no translatable text contains markup", {
  table <- readTranslationFile()

  # sym.* rows are labels, not sentences - they are the same in every language and they do
  # use <sub> for subscripts. Everything else is text a translator is handed, and markup in
  # it is what produced two rows holding the same sentence, one of them with a <br/>.
  sentences <- table[!startsWith(table$key, "sym."), ]

  withMarkup <- sentences$key[grepl("<[a-zA-Z/]", sentences$en)]

  expect_equal(withMarkup, character(0))
})

test_that("a translation keeps every placeholder the English has", {
  table <- readTranslationFile()

  # sprintf() fills %s and %i in order, and DataTables fills _START_ and its friends. A
  # translation that drops one either loses a value or stops the app at the moment it is
  # rendered, and neither shows up until that screen is opened.
  placeholders <- function(text) {
    paste(sort(unlist(regmatches(
      text, gregexpr("%[-0-9.]*[sdifg%]|_[A-Z]+_", text)))), collapse = " ")
  }

  for (language in setdiff(appLanguages, appLanguages[1])) {
    translated <- table[nzchar(table[[language]]), ]

    mismatched <- translated$key[
      vapply(translated$en, placeholders, character(1)) !=
        vapply(translated[[language]], placeholders, character(1))]

    expect_equal(paste(sort(mismatched), collapse = ", "), "",
                 info = paste("language:", language))
  }
})

test_that("the row whose text is the word NA survives being read", {
  # The hierarchical table prints "NA" in a cell when lavaan drops the RMSEA column under
  # FIML. read.csv turns that text into a missing value unless na.strings is emptied.
  expect_equal(tr("results.na"), "NA")
})

## what tr() gives back ----

## Run `code` with the app set to `language`, and put the option back afterwards.
withLanguage <- function(language, code) {
  previous <- options(shinyCTT.language = language)
  on.exit(options(previous))

  force(code)
}

test_that("an untranslated row comes back in English", {
  table <- readTranslationFile()
  untranslated <- table$key[!nzchar(table$de)]

  skip_if(length(untranslated) == 0, "every row has been translated")

  withLanguage("de",
    expect_equal(tr(untranslated[1]), table$en[table$key == untranslated[1]]))
})

test_that("a translated row comes back in German", {
  savedText <- getOption("shinyCTT.text")
  on.exit(options(shinyCTT.text = savedText))

  someKey <- readTranslationFile()$key[1]

  withGerman <- savedText
  withGerman$de[someKey] <- "Ein deutscher Text"
  options(shinyCTT.text = withGerman)

  withLanguage("de", expect_equal(tr(someKey), "Ein deutscher Text"))
  withLanguage("en", expect_equal(tr(someKey), unname(savedText$en[someKey])))
})

test_that("an unknown key comes back as itself, in both languages", {
  unknown <- "no.such.key"

  withLanguage("en", expect_equal(tr(unknown), unknown))
  withLanguage("de", expect_equal(tr(unknown), unknown))
})

test_that("an unknown language falls back to English", {
  someKey <- readTranslationFile()$key[1]

  # Not "fr", which is a language the app has: this test reads the first row, whose French
  # happens to be the same word as its English, so it passed either way.
  withLanguage("xx",
    expect_equal(tr(someKey), unname(getOption("shinyCTT.text")$en[someKey])))
})

## which language wins ----

test_that("the language R is running in is read out of the locale", {
  # "de_DE" -> "de". LANGUAGE can hold a list like "de:en", so only the part before the
  # colon counts; a language the app does not have falls back to the first one.
  previous <- Sys.getenv("LANGUAGE")
  on.exit(Sys.setenv(LANGUAGE = previous))

  Sys.setenv(LANGUAGE = "de_DE.UTF-8"); expect_equal(systemLanguage(), "de")
  Sys.setenv(LANGUAGE = "fr:en");       expect_equal(systemLanguage(), "fr")
  Sys.setenv(LANGUAGE = "pt_BR.UTF-8"); expect_equal(systemLanguage(), "en")
})

test_that("the address beats shinyCTTApp(language =), which beats R's own locale", {
  previous <- Sys.getenv("LANGUAGE")
  on.exit(Sys.setenv(LANGUAGE = previous))
  Sys.setenv(LANGUAGE = "fr_FR.UTF-8")

  # Nothing set anywhere: R's locale decides, which is what puts the startup message in
  # the reader's language before there is an app at all.
  withLanguage(NULL, expect_equal(resolveLanguage(NULL), "fr"))

  # shinyCTTApp(language = "de") was given: that beats the locale...
  withLanguage("de", expect_equal(resolveLanguage(NULL), "de"))

  # ...and ?lang=en in the address beats both.
  withLanguage("de", expect_equal(resolveLanguage("en"), "en"))
})

test_that("tr() says so when no text has been read", {
  savedText <- getOption("shinyCTT.text")
  on.exit(options(shinyCTT.text = savedText))

  options(shinyCTT.text = NULL)

  expect_error(tr("common.select"), "translations.csv")
})

## the source against the file ----

## Every key the app asks for, found by reading its source rather than running it. Each
## tr() call takes one plain string - checked by the test below - so the argument can be
## read straight out of the parsed call.
keysUsedInSource <- function() {
  # helpers-translations.R is scanned too. Defining tr() there is an assignment rather than a call
  # to it, so the definition is not picked up, and languageLabels() does ask for two keys.
  files <- list.files(packageSourceDir(), pattern = "[.]R$", full.names = TRUE)

  calls <- unlist(lapply(files, function(file) findCalls(parse(file), "tr")), recursive = FALSE)

  vapply(calls, function(call) {
    argument <- as.list(call)[[2]]
    if (is.character(argument)) argument else NA_character_
  }, character(1))
}

test_that("every tr() in the app is given a plain string", {
  skip_if(is.na(packageSourceDir()), "app sources are not here")

  # A key built at run time cannot be checked against the file, so the two tests below
  # would silently stop covering it.
  expect_false(any(is.na(keysUsedInSource())))
})

## Both checks below compare one joined string rather than two vectors, so that a failure
## prints the offending keys. Comparing the vectors only reports "Lengths differ: 1 is not
## 0", which does not say which key is wrong.

test_that("every key the app asks for is in the file", {
  skip_if(is.na(packageSourceDir()), "app sources are not here")

  missing <- setdiff(keysUsedInSource(), readTranslationFile()$key)

  expect_equal(paste(sort(missing), collapse = ", "), "")
})

test_that("every key in the file is asked for somewhere", {
  skip_if(is.na(packageSourceDir()), "app sources are not here")

  unused <- setdiff(readTranslationFile()$key, keysUsedInSource())

  expect_equal(paste(sort(unused), collapse = ", "), "")
})

test_that("no English sentence is left written into the code", {
  skip_if(is.na(packageSourceDir()), "app sources are not here")

  # A key is lowercase words joined by dots. Anything else handed to tr() is the text
  # itself, which is what this whole change was for.
  notAKey <- unique(keysUsedInSource()[
    !grepl("^[a-z][a-z0-9]*(\\.[a-z0-9]+)+$", keysUsedInSource())])

  expect_equal(paste(sort(notAKey), collapse = " | "), "")
})
