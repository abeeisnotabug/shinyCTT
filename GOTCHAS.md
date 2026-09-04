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

The flip side: an **observer is never suspended**. An `observeEvent()` watching only inputs
runs the moment the app starts, on a tab the user has not reached, over data that does not
exist yet — which is how step 2 came to report "No item selected. No analysis possible."
before any data set had been chosen. An observer that works from the data must say so with
`req()`; being on a hidden tab protects nothing.

Two more observers fire at startup and get away with it: `mod-corr-independence.R` and
`mod-corr-table.R` both watch `useFIML()`, which is a `reactiveVal(FALSE)` and so has a value
from the first moment. They are harmless only because each does nothing unless `useFIML()` is
TRUE. Invert either condition and the same bug is back in a second place.

---

### An empty input can mean two different things

`input$itemCols` is `NULL` both when the tick boxes have not been drawn yet and when the
user has unticked all of them. The first must stay silent, the second must warn — and the
input itself cannot tell them apart.

The signal that separates them is `input$groupCol`. It is drawn from the same data in the
same pass and always holds a value once it exists (`"noGroupSelected"` at the least), and
the browser reports every newly drawn control back in one message. So `req(chosenData(),
input$groupCol)` means "the choosers are on screen and have told us what they are set to",
after which an empty `input$itemCols` is the user's doing. Verified in a browser: nothing is
posted on the way into step 2, and unticking every item posts the warning at once.

Both observers in `mod-data-subset.R` open with that line, and
`tests/testthat/test-startupGuards.R` pins both halves.

---

### An output written from inside an observer can never clear itself

`observeEvent(raw(), { output$dataOverview <- ... })` does not run when `raw()` becomes
`NULL` — `ignoreNULL = TRUE` is the default — so the last table it wrote stays on screen after
the data behind it is gone. Step 1's preview went on showing the previous data set under a
message saying the new one could not be read. The render belongs at the top level, where it
re-runs on every change of `raw()`, empty ones included.

---

### `req()` does not blank a table that has already been drawn

Verified in a browser: `req(raw())` at the top of a `DT::renderDataTable()` leaves the rows
from the previous run on screen. `if (is.null(raw())) return(NULL)` clears it completely, back
to the same blank the tab starts out with. Use `req()` to keep a render from computing;
use an explicit `NULL` when the point is to take something off the screen.

---

### An error inside an observer ends the session

An error in a `render*()` shows up in the box that output draws into and the app carries on.
An error in an `observe()` or `observeEvent()` has nowhere to be shown, so shiny stops the
session — the user's app goes dead, mid-click, with no message. Anything an observer does
with data it did not create needs a `tryCatch()`; step 1's loader is the example, and the
`observeEvent(input$goModels, tryCatch(...))` around the fitting run is the other.

---

### `req()` raises an error, so it cannot sit inside a `tryCatch()`

`req()` stops an observer by raising a condition that `inherits(e, "error")` is TRUE for.
A `tryCatch(error = )` wrapped around it therefore catches "the user has not chosen a file
yet" and reports it as a failure. Keep the `req()`s above the `tryCatch()`, not in it.

---

### A control must be born in the state it belongs in

Step 2's Select button used to arrive switched on and was switched off a moment later by the
observer above — so guarding that observer handed the user a live button with nothing behind
it. A control that should start off is wrapped in `shinyjs::disabled()` where it is built,
and the observer only ever changes it from there.

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

## reactable

### A reactable fills its box unless told not to, and then overflows it

`kableExtra(full_width = FALSE)` drew an ordinary HTML table, which is as wide as its contents
need. A reactable is a flexbox and fills its container, so every converted table was stretched
across its whole box.

`width: fit-content` in the theme fixes that - but on its own it lets a wide table grow *past*
the box instead of stopping at it. With 20 items the covariance matrix came out 2100px wide
inside an 1180px box and hung over what was beside it. `max-width: 100%` alongside it stops the
growth at the box, and `.rt-table`'s own `overflow-x: auto` then scrolls it sideways. Both are
needed; either one alone is wrong.

Columns do not size themselves to their contents either - each is `colDef(minWidth = )` wide,
100px by default. A table of N columns is therefore N x 100px, which is what "as wide as it
needs" means here. A column holding `+5` or `.194` under a two-letter symbol is then two to
three times wider than anything in it, and the table as a whole outgrows the box it was going
to fit in: the hierarchical table's eight columns came to 800px inside a 550px half-width box,
so it scrolled sideways for numbers that need 500px in total.

A column narrower than its widest content wraps that content onto a second line rather than
growing back, so one number for the whole table does not work: 60px everywhere fits the
numbers but breaks FIMLR-Δχ² and RMSEA_D over two lines, and the 85px those need would put
every other column back where it started.

**A table whose columns hold different things names each one; a table whose columns all hold
the same thing names one number.** The first kind gets a `minWidths` vector keyed the same way
as its `headers`, the second a `defaultColDef(minWidth = )` - which is also the only one that
survives a change in the number of items or groups, so a covariance matrix or a correlation
table must never be given a per-column vector.

The numbers were measured, not guessed: the text of every header and every cell drawn on a
canvas at the table's own 14px Arial, plus the 12px of padding a compact cell adds. Where a
header is translated the widest of the three languages sets the width - "Mittelwert" in the
descriptives table, "Std. Schätzer" in the parameter tables. Anything longer than what was
measured wraps, which is the right way round for a value nobody expects.

What each table came to, in an 1180px page: hierarchical 800 → 523, fit index 1000 → 789,
χ²-comparison 1100 → 725, parameter tables 2000 → 1540, covariance 700 → 462, descriptives
500 → 420, the two subset tables 200 → 158 and 140. The correlation table was already at its
content width, because an interval like `[.123, .456]` needs the whole 100px.

**The two AIC/BIC comparison tables cannot be made to fit.** Six columns headed by model names,
in half of an 1180px page: 95px is what "ess. τ-equiv." needs, so the table is 575px in a 550px
box and scrolls sideways at a 1440px window, though not at 1500 or wider. Narrowing them
further only moves the wrap into the header.

*Where:* `shinyCTTApp.R`, the `reactable.theme` in `onStart`; `helperFunsTables.R`,
`makeHierTable()`, `makeFitsTable()` and `makeParTableWithCIs()`; `mod-ctt-results.R`,
`drawCompTable()` and its three call sites; `mod-covmatrix.R`, `mod-descriptives.R`,
`mod-data-subset.R`, `mod-mvn.R`.

### `colFormat()` rounds in the *reader's* language

`reactable::colFormat(digits = 3)` does its formatting in the browser, with
`Number.toLocaleString()`, so it uses whatever decimal separator that browser is set to. On a
German-locale browser Mardia's skewness came out as `148,484` in the same row as a p-value of
`0.039`, which is formatted in R with `sprintf()` and so always has a dot. One table, two
separators.

Every `colFormat()` in this package therefore passes `locales = "en-US"`, which pins the dot.
Change that only if the whole app's numbers are meant to change with the language, which is a
decision about all of them and not about one table.

*Where:* `mod-mvn.R`, and every numeric column converted after it.

### `reactable()` does not round anything by default

`makeKable(digits = 3)` rounded every numeric column, so a converted table with no `format =`
shows full floating-point precision instead. There is no warning; the numbers are simply
long. Give any column of real numbers a `colFormat(digits = 3, locales = "en-US")`, and leave
counts alone - `digits = 3` would turn 238 into `238.000`.

### `renderReactable()` can only give back a table

Several boxes used to return *either* a table *or* a `helpText()` note from one `renderUI()`.
That has to become two outputs with a guard on each, so only one of them ever fills:
`obsPerGroupTable` / `obsPerGroupNote` in `mod-data-subset.R`, and the univariate `table` /
`tableNote` in `mod-mvn.R`. The multivariate box in `mod-mvn.R` is a three-way split, because
its table has a sentence above it and a sentence below.

Unlike DT, `req()` **does** blank a reactable that has already been drawn - checked in a
browser - so it does not need the explicit `NULL` that `mod-data-source.R`'s preview does.

### `as.data.frame()` also makes duplicate row names unique

The correlation table has two rows per item - the correlation, then its interval - and every
second row name was the word `CI`. As a matrix that is fine; `as.data.frame()` renames them
`CI`, `CI.1`, `CI.2`, and that is what the screen showed.

Row labels that repeat therefore have to be a column of their own, with
`colDef(name = "")` for a blank header, rather than row names.

*Where:* `helperFunsTables.R`, `makeCorrTableWithCIs()`'s `rowLabel` column.

### A row name carrying an HTML entity needs `html = TRUE` on `.rownames`

The model abbreviations are entities (`&#964;-kong.`). Without `html = TRUE` on the
`.rownames` column they print as that text rather than as a Greek tau. The same applies to
`colGroup(name = )`, which is why the comparison table's group bands pass it too.

### `as.data.frame()` melts a one-row `table` object

`reactable()` runs `as.data.frame()` on whatever it is given, and on a `table` that turns a
one-row count into three long columns. `as.data.frame.matrix()` first keeps the counts on one
row. A `useNA = "ifany"` column also arrives named `NA` and needs a printable header.

*Where:* `mod-data-subset.R`, `obsPerGroupTable`.

---

## The language

### Store the language you resolved, not the one the address asked for

The chooser in the header reports its value the moment the page loads. The observer that acts
on it compares that value against `session$userData$lang` and reloads when they differ — so
whatever is in the session has to be the *same shape* as what the chooser reports, or every
first visit reloads itself once.

An address with no `?lang=` gives `NULL`. Storing that `NULL` and then having the chooser
report `"en"` is exactly the mismatch: the page would reload, arrive with `?lang=en`, and only
then settle. So `server()` stores `resolveLanguage(...)`, never the raw query value.

*Where:* `server.R`, the "Language" block at the top.

### `tr()` needs both halves, because the page and the server are different moments

`ui()` builds the page once per visit, and there is no session while it runs — so it puts the
language it resolved into an option, and `tr()` reads that when
`shiny::getDefaultReactiveDomain()` gives `NULL`. Everything rendered afterwards *does* have a
session, and takes the language out of `session$userData`.

An option is safe for the first half only because R runs one thing at a time: `ui()` is
called, builds the page and returns before the next visitor's `ui()` starts. It would not be
safe for the second, which is the whole reason the language moved out of
`options(shinyCTT.language)` — on a server, one visitor switching used to switch everyone.

### A test that calls `tr()` depends on the machine unless it says otherwise

The app starts in whatever language R is running in, so on a German machine `tr()` gives back
German. Tests are written against the English words — `test-modelVocabulary.R` asserts
`"kongeneric"`, which German spells `kongenerisch`. `tests/testthat/helper-language.R` pins
English for the run. Check any change to the language code under `LANGUAGE=fr_FR.UTF-8` as
well as unset.

### selectize hides the options you are looking for

Debugging the chooser in a browser console, `document.querySelector("#language").options`
shows one entry, not three: selectize.js takes the options out of the original `<select>` and
builds its own list. Click the control and read the page instead of trusting that query — the
same mistake as reading state instead of looking at the screen.

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
