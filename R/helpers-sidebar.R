## The left-hand menu, and the four stages of the workflow the app steps through.
##
##   "data" -> "subset" -> "statistics" -> "results"
##
## Nothing here touches the data or any input; each function is given what it needs and
## returns menu entries, so it can be read and tested on its own.

# The four stages, in the order the user passes through them.
stages <- c("data", "subset", "statistics", "results")

# Every tab the menu can switch to, plus the reload link, which switches to none.
# server.R watches one link per name and calls bslib::nav_select() with it.
tabNames <- c("dataSelectionTab", "subsetSelectionTab",
              "statisticsTab", "corrTab", "mvnTab",
              "testParamTab",
              "modelTests", "modelTestsMg",
              "parTables", "parTablesMg",
              "facScores", "facScoresMg",
              "modelCode", "modelCodeMg",
              "reloadTab")

# Which tab each stage opens on. The block a stage reveals is also the one it moves the
# user onto, so server.R selects this the moment the stage changes.
stageTabs <- c(data = "dataSelectionTab",
               subset = "subsetSelectionTab",
               statistics = "statisticsTab",
               results = "modelTests")

# TRUE if the app has reached `atLeast`, or gone past it. Both arguments are stage names;
# match() turns each into its position in `stages`, so "results" (4) is at least
# "subset" (2), but "data" (1) is not.
atLeastStage <- function(stage, atLeast) {
  match(stage, stages) >= match(atLeast, stages)
}

# One entry: a link that asks server.R to switch to `tabName`. The id is what server.R
# watches, and `selected` only paints it - the switch itself is nav_select().
#
# shinydashboard::menuItem() wrote AdminLTE's own markup, which bslib knows nothing about
# and would leave unstyled; the .cttMenu rules in ui.R give this its look.
navEntry <- function(tabName, label, icon, selected = FALSE) {
  tags$li(actionLink(
    paste0("nav_", tabName),
    label,
    icon = icon,
    class = if (selected) "cttSelected"))
}

# A block of entries that folds open and shut, the way a menu item with sub-items did.
# <details> does the folding by itself, so nothing here needs a script.
navGroup <- function(label, icon, entries, open = FALSE) {
  tags$li(tags$details(
    open = if (open) NA,
    tags$summary(icon, label),
    tags$ul(class = "cttSubMenu", entries)))
}

# Builds the whole menu for one stage. Later stages add more blocks; the newest block's
# first entry is marked selected, which is the tab server.R moves the user onto.
#
#   stage : "data", "subset", "statistics" or "results"
#   doMg  : TRUE -> each results block also gets a "Multigroup" entry
sidebarGroups <- function(stage, doMg) {

  # Collected block by block. Each element is itself a list of menu entries; horizontal
  # rules get put between the blocks at the very end.
  blocks <- list()

  ## ---- Always shown: the first two steps. ----
  dataAndSubsetEntries <- list(
    navEntry("dataSelectionTab", tr("data.nav"), icon("database"),
             selected = stage == "data"))

  if (atLeastStage(stage, "subset")) {
    dataAndSubsetEntries[[2]] <- navEntry(
      "subsetSelectionTab", tr("subset.nav"), icon("table"),
      selected = stage == "subset")
  }

  blocks[[1]] <- dataAndSubsetEntries

  ## ---- Unlocked once a subset has been chosen. ----
  if (atLeastStage(stage, "statistics")) {

    blocks[[length(blocks) + 1]] <- list(navGroup(
      tr("stats.nav"),
      icon("chart-bar"),
      list(
        navEntry("statisticsTab", tr("stats.desc.title"), icon("angles-right"),
                 selected = stage == "statistics"),
        navEntry("corrTab", tr("stats.nav.correlation"), icon("angles-right")),
        navEntry("mvnTab", tr("stats.mvn.title"), icon("angles-right"))),
      open = TRUE))

    blocks[[length(blocks) + 1]] <- list(navEntry(
      "testParamTab", tr("params.nav"), icon("cog")))
  }

  ## ---- Unlocked once the models have been run. ----
  if (atLeastStage(stage, "results")) {

    # The four results sections all have the same shape: a heading, then one entry per
    # tab. Listed here as heading / single-group tab / multigroup tab so the loop below
    # can build them all the same way.
    resultsSections <- list(
      c(tr("results.nav.comparison"), "modelTests", "modelTestsMg"),
      c(tr("results.nav.tables"),       "parTables",  "parTablesMg"),
      c(tr("results.nav.scores"),          "facScores",  "facScoresMg"),
      c(tr("results.nav.code"),             "modelCode",  "modelCodeMg"))

    resultsBlock <- list()

    for (sectionNumber in seq_along(resultsSections)) {

      section <- resultsSections[[sectionNumber]]
      heading <- section[1]
      singleGroupTab <- section[2]
      multigroupTab <- section[3]

      # Only the very first section is opened and selected, so the user lands on the
      # model comparison tests rather than somewhere further down.
      isFirstSection <- sectionNumber == 1

      subEntries <- list(
        navEntry(singleGroupTab, tr("results.nav.singlegroup"), icon("angles-right"),
                 selected = isFirstSection))

      if (doMg) {
        subEntries[[2]] <- navEntry(multigroupTab, tr("results.nav.multigroup"),
                                    icon("angles-right"))
      }

      resultsBlock[[sectionNumber]] <- navGroup(
        heading,
        icon("chart-bar"),
        subEntries,
        open = isFirstSection)
    }

    blocks[[length(blocks) + 1]] <- resultsBlock
  }

  ## ---- Always last. ----
  blocks[[length(blocks) + 1]] <- list(navEntry(
    "reloadTab", tr("common.reload"), icon("sync")))

  ## ---- Flatten the blocks into one list, with a rule drawn between each pair. ----
  # Reduce() walks the list of blocks left to right, gluing the next one onto what it has
  # built so far and slipping an hr() in between.
  tags$ul(
    class = "cttMenu",
    Reduce(function(soFar, nextBlock) c(soFar, list(hr()), nextBlock), blocks))
}
