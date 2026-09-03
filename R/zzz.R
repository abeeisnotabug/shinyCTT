.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the shinyCTT package! Start the shiny app by calling the shinyCTTApp() function.")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  opShinyCTT <- list(knitr.kable.NA = "",
                     knitr.table.format = "html")

  toSet <- !(names(opShinyCTT) %in% names(op))
  if (any(toSet)) options(opShinyCTT[toSet])

  # Every piece of text the app shows, read once into the store tr() looks in. Not guarded
  # like the two above: this one is the package's own text, not a setting a caller might
  # have opinions about.
  loadTranslations(system.file("translations.csv", package = pkgname))

  invisible()
}
