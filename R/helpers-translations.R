## Every piece of text the user reads goes through tr(). The text is in
## inst/translations.csv: one row per piece, named by a short key, one column per language.
## So the code says tr("common.select") and never holds the sentence itself.
##
## Symbols are rows too, under sym.*, and are the same in every language.

## The languages on offer, in the order the chooser lists them. The first is the fallback:
## an entry with no translation yet comes back in it. Adding one means a column of the same
## name in inst/translations.csv and a sym.lang.<code> row for the chooser's label.
appLanguages <- c("en", "de", "fr")

## What the chooser in the header calls each language: its flag, and its own name for
## itself. Every key is written out in full - test-translations.R reads this file as text
## and cannot see one built at run time.
languageLabels <- function() {
  c(en = tr("sym.lang.en"),
    de = tr("sym.lang.de"),
    fr = tr("sym.lang.fr"))
}

## Read the text into an option. Called once, by .onLoad() in zzz.R.
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

## The language R itself is running in, when the app has been translated into it, and the
## first language on the list otherwise. "de_DE" gives "de".
##
## LANGUAGE is what R checks first for its own messages and can hold a list like "de:en", so
## only the part before the colon is read; LC_MESSAGES is the locale proper.
systemLanguage <- function() {
  asked <- Sys.getenv("LANGUAGE")

  if (!nzchar(asked)) asked <- Sys.getlocale("LC_MESSAGES")

  code <- substr(sub(":.*", "", asked), 1, 2)

  if (isTRUE(code %in% appLanguages)) code else appLanguages[1]
}

## Which language to show, given whatever the address asked for. Anything unknown - or
## nothing at all - falls back to what shinyCTTApp(language = ) was given, and that to the
## language R itself is running in.
##
## That last step is what puts the startup message in German on a German machine: .onAttach()
## runs before there is an app, let alone a visitor, so neither of the two above it is set.
resolveLanguage <- function(asked) {
  if (isTRUE(asked %in% appLanguages)) return(asked)

  fallback <- getOption("shinyCTT.language", default = systemLanguage())

  if (isTRUE(fallback %in% appLanguages)) fallback else appLanguages[1]
}

## Remember the language the next page is being built in. Called at the top of ui(), from
## the ?lang= part of the address the browser asked for.
##
## One value for the whole app, which is safe because R builds one page at a time. A
## visitor whose page is already open has their own, in their session - see tr() below.
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
