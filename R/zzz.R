.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the shinyCTT package! Start the shiny app by calling the shinyCTTApp() function.")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  opShinyCTT <- list(knitr.kable.NA = "",
                     knitr.table.format = "html")

  toSet <- !(names(opShinyCTT) %in% names(op))
  if (any(toSet)) options(opShinyCTT[toSet])

  invisible()
}
