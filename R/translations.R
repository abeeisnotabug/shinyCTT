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

appLanguages <- c("en", "de")   # the languages on offer; the first one is the fallback

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

  # One named vector per language: the names are the keys, the values the text.
  options(shinyCTT.text = list(
    en = stats::setNames(table$en, table$key),
    de = stats::setNames(table$de, table$key)))
}

## Which language the app is showing. Set by shinyCTTApp(language = "de"); an unknown one
## falls back to English.
appLanguage <- function() {
  language <- getOption("shinyCTT.language", default = appLanguages[1])

  if (isTRUE(language %in% appLanguages)) language else appLanguages[1]
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

  if (identical(appLanguage(), "en")) return(unname(english))

  translated <- text[[appLanguage()]][key]

  if (is.na(translated) || !nzchar(translated)) unname(english) else unname(translated)
}
