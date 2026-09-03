## The left-hand menu, and the four stages of the workflow the app steps through.
##
##   "data" -> "subset" -> "statistics" -> "results"
##
## Nothing here touches the data or any input; each function is given what it needs and
## returns menu items, so it can be read and tested on its own.

# The four stages, in the order the user passes through them.
stages <- c("data", "subset", "statistics", "results")

# TRUE if the app has reached `atLeast`, or gone past it. Both arguments are stage names;
# match() turns each into its position in `stages`, so "results" (4) is at least
# "subset" (2), but "data" (1) is not.
atLeastStage <- function(stage, atLeast) {
  match(stage, stages) >= match(atLeast, stages)
}

# Builds the whole menu for one stage. Later stages add more blocks; the newest block is
# marked selected = TRUE, which moves the user onto the tab they just unlocked.
#
#   stage : "data", "subset", "statistics" or "results"
#   doMg  : TRUE -> each results entry also gets a "Multigroup" sub-item
sidebarGroups <- function(stage, doMg) {

  # Collected block by block. Each element is itself a list of menu entries; horizontal
  # rules get put between the blocks at the very end.
  blocks <- list()

  ## ---- Always shown: the first two steps. ----
  dataAndSubsetEntries <- list(
    shinydashboard::menuItem(
      tr("data.nav"),
      tabName = "dataSelectionTab",
      icon = icon("database")))

  if (atLeastStage(stage, "subset")) {
    dataAndSubsetEntries[[2]] <- shinydashboard::menuItem(
      tr("subset.nav"),
      tabName = "subsetSelectionTab",
      icon = icon("table"),
      selected = stage == "subset")
  }

  blocks[[1]] <- dataAndSubsetEntries

  ## ---- Unlocked once a subset has been chosen. ----
  if (atLeastStage(stage, "statistics")) {

    blocks[[length(blocks) + 1]] <- list(shinydashboard::menuItem(
      tr("stats.nav"),
      shinydashboard::menuSubItem(tr("stats.desc.title"), tabName = "statisticsTab",
                                  selected = stage == "statistics"),
      shinydashboard::menuSubItem(tr("stats.nav.correlation"), tabName = "corrTab"),
      shinydashboard::menuSubItem(tr("stats.mvn.title"), tabName = "mvnTab"),
      icon = icon("chart-bar"),
      startExpanded = TRUE))

    blocks[[length(blocks) + 1]] <- list(shinydashboard::menuItem(
      tr("params.nav"),
      tabName = "testParamTab",
      icon = icon("cog")))
  }

  ## ---- Unlocked once the models have been run. ----
  if (atLeastStage(stage, "results")) {

    # The four results sections all have the same shape: a heading, then one sub-item per
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

      subItems <- list(
        shinydashboard::menuSubItem(tr("results.nav.singlegroup"),
                                    tabName = singleGroupTab,
                                    selected = isFirstSection))

      if (doMg) {
        subItems[[2]] <- shinydashboard::menuSubItem(tr("results.nav.multigroup"),
                                                     tabName = multigroupTab)
      }

      resultsBlock[[sectionNumber]] <- shinydashboard::menuItem(
        heading,
        subItems,
        icon = icon("chart-bar"),
        startExpanded = isFirstSection)
    }

    blocks[[length(blocks) + 1]] <- resultsBlock
  }

  ## ---- Always last. ----
  blocks[[length(blocks) + 1]] <- list(shinydashboard::menuItem(
    tr("common.reload"),
    tabName = "reloadTab",
    icon = icon("sync"),
    selected = FALSE))

  ## ---- Flatten the blocks into one list, with a rule drawn between each pair. ----
  # Reduce() walks the list of blocks left to right, gluing the next one onto what it has
  # built so far and slipping an hr() in between.
  Reduce(function(soFar, nextBlock) c(soFar, list(hr()), nextBlock), blocks)
}
