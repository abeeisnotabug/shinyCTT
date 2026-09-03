## Every piece of text the user reads goes through tr(). The text itself is not in this
## file any more - it lives in inst/translations.csv, one row per piece, named by a short
## key, with one column per language:
##
##     key,en,de
##     common.select,Select,Auswaehlen
##     subset.items.label,2a. Select the item columns:,
##
## So the code says tr("common.select") and never holds the English sentence itself. Three
## reasons that beat writing the English into the code:
##
##   1. R CMD check warns about any non-ASCII character in R/, so the sigma-squared of an
##      error variance cannot be written in an R file - only as the HTML entity for it. A
##      .csv is not R code, so the real characters are fine there.
##   2. The long warning sentences made lines of 250 characters.
##   3. Two rows of the old table held the same sentence, one of them with a <br/> in it.
##      Markup does not belong in text a translator is handed.
##
## Symbols live in the same file under sym.*. They are not translated - a sigma is a sigma
## in German - but they are text, so they belong outside R/ for reason 1.

## The languages on offer, in the order the chooser lists them. The first is the fallback:
## an entry with no translation yet comes back in it. Adding one means a column of the same
## name in inst/translations.csv and a sym.lang.<code> row for the chooser's label.
appLanguages <- c("en", "de")

## What the chooser in the header calls each language: its flag, and its own name for
## itself. Written out one by one rather than built with paste0(), because
## test-translations.R reads the source for tr("...") and cannot see a key that is
## assembled at run time.
languageLabels <- function() {
  c(en = tr("sym.lang.en"),
    de = tr("sym.lang.de"))
}

## Read the text into an option. Called once, by .onLoad() in zzz.R.
##
## An option rather than a variable of this file, for the same reason the app's other
## settings are options: it is one place, and nothing that reloads the package's code can
## quietly leave an empty copy behind.
loadTranslations <- function(path) {
  # na.strings = character(0) because one row's text is the word "NA" - the hierarchical
  # table prints it in a cell when lavaan drops the RMSEA column under FIML. read.csv would
  # otherwise read that text as a missing value and the key would come back empty on screen.
  table <- utils::read.csv(path, colClasses = "character", encoding = "UTF-8",
                           na.strings = character(0))

  # One named vector per language: the names are the keys, the values the text. Built from
  # appLanguages rather than named one by one, so a new language is a new column and
  # nothing here changes.
  options(shinyCTT.text = stats::setNames(
    lapply(appLanguages, function(language) {
      stats::setNames(table[[language]], table$key)
    }),
    appLanguages))
}

## Which language to show, given whatever the address asked for. Anything unknown - or
## nothing at all - falls back to what shinyCTTApp(language = ) was given, and that to the
## first language on the list.
resolveLanguage <- function(asked) {
  if (isTRUE(asked %in% appLanguages)) return(asked)

  fallback <- getOption("shinyCTT.language", default = appLanguages[1])

  if (isTRUE(fallback %in% appLanguages)) fallback else appLanguages[1]
}

## Remember the language the next page is being built in. Called at the top of ui(), from
## the ?lang= part of the address the browser asked for.
##
## One value for the whole app rather than one per visitor, which is safe only because R
## runs one thing at a time: ui() is called, builds the page and returns before the next
## visitor's ui() starts. The language of a visitor whose page is already open is a
## different value and lives in their own session - see tr() below.
setUiLanguage <- function(asked) {
  options(shinyCTT.uiLanguage = resolveLanguage(asked))
}

## The language this piece of text is being asked for in.
##
## While the server is running there is a session, and the language chosen for THAT session
## is kept in it - server() puts it there, so two people using the app at the same time can
## be reading it in different languages. While the page is being built there is no session
## yet, so the language ui() just set is used. Both halves are needed: the page is built
## once per visit, and the server goes on rendering text afterwards.
currentLanguage <- function() {
  session <- shiny::getDefaultReactiveDomain()

  if (is.null(session)) {
    resolveLanguage(getOption("shinyCTT.uiLanguage"))
  } else {
    resolveLanguage(session$userData$lang)
  }
}

## The text to put on the screen, looked up by its short name.
##
## An empty German entry gives back the English, so a half-translated file still shows a
## working app. A name that is not in the file at all comes back as the name itself, which
## is visible on screen - test-translations.R fails on it long before it gets that far.
tr <- function(key) {
  text <- getOption("shinyCTT.text")

  # Without this the failure would be "argument is of length zero" from the if() below.
  if (is.null(text))
    stop("No text has been read yet - .onLoad() reads inst/translations.csv.")

  english <- text$en[key]

  if (is.na(english)) return(key)

  language <- currentLanguage()

  if (identical(language, appLanguages[1])) return(unname(english))

  translated <- text[[language]][key]

  if (is.na(translated) || !nzchar(translated)) unname(english) else unname(translated)
}
