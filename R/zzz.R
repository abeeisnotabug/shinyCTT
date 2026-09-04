.onAttach <- function(libname, pkgname) {
  # .onLoad() has already read the text file, so this can be looked up like any other piece
  # of text. There is no app and no visitor yet, so it comes out in the language R itself is
  # running in.
  packageStartupMessage(tr("startup.message"))
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
