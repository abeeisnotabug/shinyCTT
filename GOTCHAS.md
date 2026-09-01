# Gotchas

Why a few things in this package are written the way they are, and what bit us when they
weren't. This lives here rather than in the code so the source files stay readable: comments
in `R/` say what a line does, and anything that needs a paragraph of reasoning goes here.

`WALKTHROUGH.md` is the guide to how the app works. This is the list of traps.

---

## Shiny

### A rebuilt control comes back enabled

`updateCheckboxGroupInput(choices = ...)` and friends build the control again from scratch,
and the new one has no memory of `shinyjs::disable()`.

This caused a real bug. The "Select all" / "Unselect all" links sit in the same `renderUI` as
the item checkboxes. Clicking one after the analysis had been run rebuilt the checkbox group,
handed the user an editable item list, and let them change the items that every already-rendered
table had been computed from.

Greying the links out is not enough on its own: `shinyjs` marks an `actionLink` disabled, but
an `<a>` element has no disabled state, so the click still reaches R and the counter still goes
up. The guard that actually works is the `identical(appStage(), "subset")` test inside each
observer.

*Where:* `server.R`, the `selectall` / `deselectall` observers.

### Hiding a checkbox does not change its value

`conditionalPanel` only hides things. A model whose cell in the comparison grid says "Too few
items." still has its checkbox set to `TRUE`, and the run reads the checkboxes, not the grid.

With two items selected this sent an unidentified tau-congeneric model to lavaan. Its RMSEA
confidence interval called `qchisq()` with negative degrees of freedom, the resulting `NaN`
reached an `if`, and the whole "Test the models" observer died — leaving every grid cell
reading "Tested." with no results and no error message anywhere on the page.

The fix is to untick the model itself when the item count drops, not to filter it out later.
Correcting the input also corrects the grid, because all 33 conditions in the grid read those
same checkboxes.

*Where:* `server.R`, "keep the model selection in step with the item count".

### `do.call()` on a named list makes children into attributes

`fluidRow()`, `tagList()` and `shinydashboard::menuItem()` all accept a plain list of children
and unpack it themselves, so `do.call()` is never needed for them.

It is also actively dangerous. `family$names` is a *named* vector, so `lapply()` over it returns
a named list, and `do.call(fluidRow, cells)` turns those names into HTML attributes:

```html
<div class="row" tko="&lt;div class=&quot;col-sm-2&quot;..." ete="..."></div>
```

The cells vanish with no error. Pass the list directly instead.

*Where:* `comparisonGrid.R`, `sidebar.R`.

### `conditionalPanel` conditions are JavaScript

They run in the browser, use `.` rather than `$`, and cannot call R functions. A malformed
condition does not error — the panel simply never appears.

`input.itemCols` is *undefined* until data has been chosen, which is why every condition in the
comparison grid starts with `input.itemCols &&`. Without that guard the browser throws roughly
eighteen errors per walkthrough.

Inside a Shiny module a `conditionalPanel` needs `ns = ns` passed to it, **and** the input ids
inside it need `ns()` applied directly. Missing either half fails silently the same way.

### Writing a number box back rewrites it under the user's fingers

A `numericInput` sends its value on every keystroke, so an observer that "corrects" an
out-of-range value fights whoever is typing. Typing `0.01` into an empty significance level
box used to give **`0.0501`**: the leading `0` is out of range, the app put `0.05` in the box,
and the remaining keystrokes landed on the end of that.

```
keystroke  box afterwards
0          0
.          (emptied by the correction)
0          0.050
1          0.0501
```

So neither level is written back any more. An unusable entry is simply not taken: the tables
go on showing the last usable value and a red note appears under the box saying what they are
still using. The bounds live in `sigLvlUsable()` / `rmseaCiLvlUsable()` and nowhere else.

This was invisible while the boxes were frozen after a run. Step 4b made them live, which is
the whole point of step 4b, and that is what exposed it.

### shinyjs puts the module's name in front of the id itself

Inside a module, `shinyjs::show("corrTabNA")` is right and `shinyjs::show(ns("corrTabNA"))` is
wrong. shinyjs 2.1.1 checks whether it is being called with a module's session and, if so,
does the naming itself:

```r
if (inherits(session, "session_proxy") && isShinyjsFunction) {
  if ("id" %in% names(params) && !is.null(params[["id"]])) {
    if (!"asis" %in% names(params) || !params[["asis"]]) {
      params[["id"]] <- session$ns(params[["id"]])
```

Write `ns()` as well and the id is named twice — `corrTable-corrTable-corrTabNA` — which
matches no element on the page, so the call does nothing at all and says nothing about it.
This happened for real: the "how to handle missing values" control on the correlation tab
stayed hidden under FIML until the `ns()` came off. `shiny`'s own `update*Input()` functions
behave the same way, so they too take the plain id.

The `----` markers in `R/mod-*.R` and the test in `tests/testthat/test-moduleNamespacing.R`
guard both directions: a control's id must go *through* `ns()` where the control is created,
and must *not* where shinyjs is told to show or hide it.

### `useShinyjs()` registers its JavaScript at runtime

`shinyjs` 2.1.1 calls `shiny::addResourcePath()` from inside `useShinyjs()` and nowhere else.
While `ui` was a top-level object rather than a function, that call ran in the *installer's* R
session and was thrown away, so the script 404'd at runtime and every `disable()`, `hide()` and
`runjs()` in the app was a silent no-op for anyone running an installed build.

`devtools::load_all()` hides this completely, because it sources `ui.R` in the live session.

**So: `ui` must stay `function(request) { ... }`, and anything touching shinyjs must be tested
against an installed build.** Check it with `window.shinyjs` in the browser console — it should
be an object, not `undefined`.

### Outputs are suspended while hidden

Shiny does not compute an output that is not currently on screen. Anything that must be
available before the user visits its tab needs
`outputOptions(output, "id", suspendWhenHidden = FALSE)` — as `incompleteCasesBoolRV` does,
because the FIML checkbox's `conditionalPanel` reads it.

---

## lavaan

### A warning is not a failure

lavaan frequently warns and still returns a perfectly usable fit — the essentially
tau-parallel and tau-parallel models do it routinely. `tryCatch(warning = ...)` exits at the
first warning and throws the fit away, so those models used to be silently dropped from every
comparison table with no explanation.

`withCallingHandlers()` plus `invokeRestart("muffleWarning")` records the warning and lets the
call finish, so one pass gets both. A model that errors is still caught by the surrounding
`tryCatch()`.

*Where:* `server.R`, the fitting loop.

### `lavTestLRT()` drops its RMSEA column under FIML

If *any* compared model was fitted with `missing = "fiml"`, the output has no RMSEA-of-the-
difference column at all — whether or not the data actually has missing values. `makeHierTable()`
fills it with `NA` before selecting columns, and renders it as an explicit "NA" in a grey cell.

Verified present under `"listwise"` and absent under `"fiml"`, on both complete and incomplete
data, with lavaan 0.6-21.

### The reliability confidence interval is undefined at a Heywood boundary

`extractParameters()` computes reliability intervals on the logit scale and transforms back, to
keep them inside [0, 1]. When an estimate sits exactly at 0 or 1 — or past it, which happens in
a genuine Heywood case — the logit is infinite and its standard error blows up too, giving
`NaN` on one bound.

Both bounds are clamped to the boundary instead. `log()` of a negative number is computed and
then discarded on that path, so it is wrapped in `suppressWarnings()`.

`rtdataWarn.RData` reproduces this: `item_1`'s error variance comes out at −0.000113 and its
reliability at 1.000146.

### `minItems` means *positive* degrees of freedom

Not "identified". The tau-congeneric model at 3 items is *just* identified — df = 0, fits
perfectly, tests nothing. Only at 2 items is it under-identified, with df = −1. The thresholds
in `cttModelFamily()` are the first item count at which each model has something to test.

---

## This package

### Helpers must never take `input`

Inside a Shiny module, `input` is a namespaced proxy that only sees that module's own
namespace. A helper reading `input$source` from another namespace gets `NULL` **with no error**,
so the failure is silent and shows up arbitrarily far downstream.

`makeRCode()` used to take `input = input` and read eight fields off it. It now takes them
explicitly, with the data source passed in as a small descriptor list. Do not reintroduce the
pattern. Dynamic `input[[id]]` reads *within one namespace* are fine.

### String suffixes are not a namespace

The single-group and multigroup passes are the same code run twice, distinguished by pasting
`"Mg"` onto every output id. This broke twice:

- the multigroup factor-score download read `input$tkoFilenameMg`, but the text box was created
  as `tkoFilename` — so `filename` was `NULL`, `sprintf('...filename="%s"', NULL)` collapsed to
  `character(0)`, and the response had no `Content-Disposition` header at all;
- `tkoSep` and `tkoDec` were created with the *same* id in both passes, so the single-group and
  multigroup separator controls were one control and changing either changed both.

Both were fixed by deriving all five ids from `groupAppend`-suffixed variables, and the
suffixing is now gone altogether: `mod-ctt-results.R` is started once per pass, so `NS()` does
the telling apart and the ids inside it are plain. Only the `tabName`s in `ui.R` still end in
`Mg`, and those belong to the app rather than to a module.

### A tab that is appended is appended again next time

The results tabs used to be added one per model with `appendTab()`. That only looked right
because the button disabled itself after one run: a second run would have given two
"τ-kongeneric" tabs, then three. The three strips are now built whole in a `renderUI()` from
the models that fitted, so a rerun replaces them. Never go back to `appendTab()` here.

`shinydashboard::tabBox()` takes its panels one at a time and does *not* accept a list of
them — `tabsetPanel(listOfPanels)` errors with "Navigation containers expect a collection of
...". Hence the `do.call()` at those three call sites, on an unnamed list. (An unnamed one:
see the `do.call()` gotcha above.)

### Anything the results tabs read must be frozen at fit time

The estimator can now be changed after a run without refitting, so `input$estimator` and the
fitted models can disagree. Every setting the results pages need — the estimator, the FIML
choice, the item and group columns, where the data came from — is stored in `modelFitsRV()`
next to the fits, and the tables read it from there.

Read a live input instead and the failure is silent: the fit index table would head its
column `MLR-χ²` over numbers that came out of ML, and the exported R script on the Model Code
tab would not reproduce the results next to it. Two things are deliberately live and only
two: the significance level and the confidence level of the RMSEA interval, because neither
changes how anything was fitted.

### The comparison set is derived, not listed

`cttModelFamily()$comparable` used to be
`outer(models, models, paste0)[lower.tri(diag(5))][-8]`. The `[-8]` dropped the
tau-equivalent / essentially-tau-parallel pair — the one pair with equal degrees of freedom,
where neither model is nested in the other, so no likelihood-ratio test exists.

It picked that pair out *by position*. Reordering `models`, or adding a sixth, would have
silently dropped the wrong one and filled the comparison tables with meaningless tests.
`nestedPairs()` now works it out from the nesting graph, and a test pins that it still
reproduces the old vector exactly.

### Freezing controls only goes one way

Each of the first two steps freezes its own controls, from a `frozen` reactive the app hands
it. What is left of the stage lockout in `server.R` disables the controls of every stage
already passed, and never enables anything. Some controls start disabled for their own reasons — the data Select button
until the chosen data validates, the multigroup checkbox until the group column yields usable
groups — so a blanket "enable everything for the current stage" would switch those on wrongly.

The one backwards move, the failed-run handler, re-enables its two controls by name.

The significance level, the confidence level of the RMSEA interval, the estimator and the
"Test the models" button are in no stage entry at all, so nothing ever disables them — they
are meant to stay usable after a run.

### Group colours are pinned by name

A discrete ggplot2 scale hands its palette to whichever levels are still present in the data,
so de-selecting a group in a plot tab used to recolour the remaining ones. `groupColors()`
builds the palette itself and names it by group, and every group-wise plot uses
`scale_*_manual()`. Do not put `scale_*_discrete()` back.
