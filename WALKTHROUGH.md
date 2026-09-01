# How this app works

A guide to reading and changing `shinyCTT`, written for the person maintaining it rather than
for a Shiny expert. This explains how the app fits together; `GOTCHAS.md` lists the traps and
why a few things are written the way they are; `CLAUDE.md` is the terse reference.

---

## 1. The ten-second version

The app walks the user through **four steps**, one at a time: pick data, pick a subset of it,
look at descriptive statistics, run the models. Each step unlocks the next one and freezes
its own controls so nothing can be changed underneath a result that has already been
computed.

```
  data  ────────►  subset  ────────►  statistics  ────────►  results
   ▲                                                            │
   └──────────── only backwards move: a failed model run ───────┘
                 (goes back to "statistics", not further)
```

Everything else in the app hangs off that.

---

## 2. Shiny in five sentences

You only need five ideas to read this code.

| Thing | What it means |
| --- | --- |
| `input$foo` | Whatever the user has currently typed or ticked in the control with id `"foo"`. |
| `output$bar <- render*(...)` | "Whenever anything inside this changes, redraw the thing on the page called `bar`." |
| `reactive({...})` | A value that recomputes itself when its ingredients change. Call it like a function: `myThing()`. |
| `reactiveVal(x)` | A single value you set by hand. `myVal()` reads it, `myVal(5)` writes it. |
| `observeEvent(x, {...})` | "When `x` happens, run this block." Used for side effects — a button press, disabling a control. |

The one rule that trips people up: **anything reading `input$foo` or a `reactive()` re-runs
automatically when that changes.** You never write "update the screen"; you write what the
screen should contain, and Shiny works out when to redraw it.

---

## 3. What happens when the user presses each button

### "Select" on the data tab (`dataSelectButton`)

1. An observer has already been watching the source dropdown and file inputs. Each time
   they change it loads the data into `userDataRaw()` and checks it has more than one column
   and at least one numeric one. If not, it disables the Select button and posts a
   notification.
2. Pressing Select runs `observeEvent(input$dataSelectButton, ...)`, which does two things:
   copies the data into `userDataChosen()`, and calls `appStage("subset")`.
3. `appStage("subset")` is what makes everything else happen — see section 4.

### "Select" on the subset tab (`subsetSelectButton`)

Sets `appStage("statistics")`, works out `userDataGroup()` (the data cut down to the chosen
items and groups), decides whether the group column is usable (`validGroupsRV()`), and
records whether FIML is in play (`fimlRV()`).

### "Test the models" (`goModels`)

It:

1. freezes the multigroup choice into `doMgRV()` and calls `appStage("results")`;
2. builds the five lavaan model syntax strings with `makeModelCodes()`;
3. fits each chosen model, keeping the fit even if lavaan only *warns*;
4. puts the fits, and the settings they were fitted with, into `modelFitsRV()`.

**It draws nothing.** Every table, plot and tab on the results pages is an output of its own,
written once when the app starts, that reads `modelFitsRV()` — see section 4b.

The whole body is wrapped in `tryCatch()`. If anything in that chain fails, the handler puts
the message in a red box under the button and steps `appStage()` back to `"statistics"` so
the user can change something and try again.

---

## 4. The one idea worth understanding: `appStage`

There is a single `reactiveVal` holding where the user is:

```r
appStage <- reactiveVal("data")          # then "subset", "statistics", "results"
```

**Two things read it, and nothing else records workflow position.**

**The sidebar.** `sidebarGroups(stage, doMg)` in `R/sidebar.R` builds the entire menu from
scratch for a given stage. It collects blocks of menu entries into a list, then joins them with `hr()` lines:

```r
blocks[[1]] <- dataAndSubsetEntries               # always
if (atLeastStage(stage, "statistics")) { ... }    # adds two more blocks
if (atLeastStage(stage, "results"))    { ... }    # adds the four results sections
blocks[[length(blocks) + 1]] <- reloadBlock       # always last
```

**The control lockout.** `stageControls` lists which inputs belong to which stage:

```r
stageControls <- list(
  data       = c("source", "CSVFile", ... , "dataSelectButton"),
  subset     = c("itemCols", "groupCol", ... , "useFIML"),
  statistics = c("doMg", "etaIntFree"))
```

and one observer disables everything belonging to a stage the app has already left:

```r
observeEvent(appStage(), {
  currentPosition <- match(appStage(), stages)
  for (position in seq_len(currentPosition - 1)) { ...disable them... }
})
```

**Locks only ever go one way.** The loop never *enables* anything, because some controls
start disabled for their own reasons (the Select button until the data validates, the
multigroup checkbox until there is a usable group column). A blanket "enable everything from
the current stage" would switch those on wrongly. The only backwards move — the failed-run
handler — re-enables its four controls by name.

> **If you add a control**, put its id in the right `stageControls` entry. That is the whole
> job; you do not need to write a `disable()` call.

Four controls on the Testing Parameters tab are deliberately in no entry at all, so they stay
usable after a run: the significance level, the confidence level of the RMSEA interval, the
estimator, and the "Test the models" button itself.

---

## 4b. Fitting and drawing are separate

The button fits. Everything on the results pages draws itself from the fits, and redraws when
a display setting changes. Nothing is refitted for a display setting.

```
"Test the models"  ->  modelFitsRV()  ->  the tables, plots and tabs
                                     ^
                       the significance level and the RMSEA confidence level
                       feed in here, not into the fitting
```

So:

- **Changing the significance level** recolours every table, re-stars every comparison and
  recomputes the parameter confidence intervals, with no refit. It is instant.
- **Changing the confidence level of the RMSEA interval** widens or narrows that interval in
  the fit index table, and relabels its header. Also no refit — the interval is worked out
  from the fitted model by `fitMeasures(fm.args = list(rmsea.ci.level = ...))`.
- **Changing the estimator** *does* need a refit, because it changes how the models are
  fitted. The app does not do it silently: an orange note appears under the button and the
  button's label gains a `*`, until the user presses it.

`modelFitsRV()` holds the settings each pass was fitted with — the estimator, whether FIML
was used, the item and group columns, where the data came from — alongside the fits. The
tables read those, not the live controls, so after an estimator change the results go on
saying which estimator actually produced them, and the exported R script goes on reproducing
them.

**The three tab strips are rebuilt whole**, from `renderUI()`, rather than a tab being added
per model. A tab that is *added* would be added again on the next run, giving two
"τ-kongeneric" tabs, then three. Each strip depends only on the fits, so changing a display
setting redraws the table *inside* the open tab and leaves the user where they were.

---

## 5. Where things live

| File | What's in it |
| --- | --- |
| `R/modelFamily.R` | The five models: names, labels, how many items each needs, how they nest. **Start here** if you want to change anything about the models themselves. |
| `R/makeModelCodes.R` | Turns the chosen item columns into five lavaan syntax strings. The models differ only in which parameter labels they re-use. |
| `R/comparisonGrid.R` | Draws the 5×5 table of checkboxes on the Testing Parameters tab. |
| `R/helperFunsExtract.R` | Pulls fit indices and parameter estimates out of a fitted lavaan object. Contains the reliability confidence intervals. |
| `R/helperFunsTables.R` | Every HTML table the app shows. |
| `R/helperFunsAdvanced.R` | Factor scores, and the copy-pasteable R script shown on the Model Code tab. |
| `R/ui.R` | The page layout: which boxes sit on which tab. Also the FU Berlin green theme. |
| `R/sidebar.R` | The left-hand menu, and the four stages the app steps through. |
| `R/server.R` | Everything that computes or reacts. Long, but sectioned — see below. |

`server.R` is navigated by its `----` markers. In RStudio the outline pane (top right of the
editor, or <kbd>Ctrl/Cmd</kbd>+<kbd>Shift</kbd>+<kbd>O</kbd>) turns them into a clickable
table of contents. The number of `#` marks the nesting depth:

```r
  # dataSelectionTab ----                              <- a whole tab
  ## dataSelectionTab objectsInWorkspace ----          <- one output within it
```

**Keep adding these when you add code.** They are the only navigation aid in a 2300-line file.

---

## 6. How to make the four most likely changes

### "I want to change a label the user sees"

Model names live in `cttModelFamily()` in `R/modelFamily.R`, in three places:

- `long` — full name, for tab titles: `"essentially &tau;-equivalent"`
- `abbrev` — short name, for table headers: `"ess. &#964;-equiv."`
- `plot$name` — the same again as an R plotmath expression, for the hierarchical plot

Change all three or the app will disagree with itself. Everything else is prose in `ui.R` or
in the box titles in `server.R`.

Greek letters are HTML entities (`&tau;`, `&sigma;`) because the tables are HTML strings, not
Shiny tables. The plot cannot use HTML, so it needs the plotmath form instead.

### "I want to add or remove a model"

1. `cttModelFamily()` — add its short name to `models`, add one entry to each of `long`,
   `abbrev` and `minItems`, add its nesting edges to `hierarchy`, and add a row to `plot`
   giving its label and where the plot should draw it.
2. `makeModelCodes.R` — say which parameters it constrains.

That is all. The comparison grid, the list of valid comparisons, the item-count limits and
the checkbox ids all follow from step 1.

The reason it follows: `comparable` is not a list anyone maintains. `nestedPairs()` walks the
`hierarchy` edges and works out which models sit inside which. If two models are not
connected by any chain of edges — as with tau-equivalent and essentially tau-parallel, which
have the same degrees of freedom — no comparison is offered and the grid prints
"Not testable." in that cell.

### "I want to change how many items a model needs"

`minItems` in `cttModelFamily()`, and nowhere else. The grid reads it to decide when to show
"Too few items.", and `server.R` reads it to untick models the user cannot test.

There is one remaining copy of these numbers written out in prose — the notification text
that says "Only three items selected. Unable to test the τ-kongeneric model." That is a
sentence, not logic, so it does not follow automatically. Update it by hand.

### "I want to add a tab"

1. Add a `tabItem(tabName = "myTab", ...)` to `ui.R`.
2. Add a `menuItem(..., tabName = "myTab")` to the right block in `sidebarGroups()`.

The two `tabName`s must match exactly — that string is how the browser connects the menu
entry to the panel. There is no error if they disagree; the tab simply never opens.

---

## 7. Things that will bite you

**A `conditionalPanel` condition is JavaScript, not R.** `"input.itemCols.length > 3"` runs
in the browser. It uses `.` not `$`, and it cannot call R functions. If a condition is
malformed, the panel silently never appears — there is no error.

**`input.itemCols` is undefined before data is chosen.** Every condition in the comparison
grid starts with `input.itemCols &&` for that reason. Without the guard the browser throws
about eighteen errors per walkthrough.

**Rebuilding a control throws away `disable()`.** `updateCheckboxGroupInput(choices = ...)`
builds the control fresh, and the fresh one is enabled. This caused a real bug: the
"Select all" link handed back an editable item selection after the analysis had already been
computed from the old one. Both links are now disabled *and* their observers refuse to act
once the subset stage has passed — the second half is the one that actually works, because an
`<a>` has no disabled state of its own and the click still reaches R.

**Test against an installed build, not `load_all()`.** `shinyjs` registers its JavaScript at
runtime from inside `useShinyjs()`. Under `devtools::load_all()` that always happens; in an
installed copy it only happens because `ui` is a *function*. If someone ever changes `ui` back
to a plain object, every `disable()` in the app silently stops working — and the dev loop will
not show it. Check with:

```r
devtools::install()
# then, in the running app's browser console:
window.shinyjs        # should be an object, not undefined
```

**An output that only ever draws must not fit anything.** The split in section 4b is what
lets the significance level be changed after a run. If you add something to the results pages,
work out which side of the line it is on: does it need a new fit, or only the fits that are
already there? Put anything that needs a fit in the `goModels` observer, and anything that
does not in an output of its own.

**Every `output$x <- render*()` sits at the top level.** None is nested inside another. A
nested one is torn down and rebuilt every time the outer one changes, which makes it very hard
to work out what depends on what. The five plots are defined next to the `renderUI` that draws
their controls, and each opens with a `req()` naming the inputs it needs, because those
controls do not exist until that `renderUI` has run once.

---

## 8. Checking you have not broken anything

```r
devtools::test()     # 46 tests over the calculation helpers - fast, run it constantly
devtools::check()    # full R CMD check; the package is kept at 0 errors/warnings/notes
```

The tests cover the **calculations**: model syntax, fit indices, parameter tables,
reliabilities, the exported R script. They deliberately check numbers and structure with a
tolerance rather than storing a copy of the rendered HTML, so they do not break when a table
package changes its markup.

**They do not test the user interface at all.** For that, run the app and click through it:

```r
devtools::load_all(".")
shinyCTTApp()
```

Useful data to click through with:

| Dataset | What it exercises |
| --- | --- |
| `rtdata` (ships with the package) | the normal path; 238 rows, 6 items, a `gender` column |
| `KTT App/rtdataNA.RData` | missing values, so the FIML path |
| `KTT App/rtdataWarn.RData` | a genuine Heywood case, so the "lavaan warned but we kept the fit" path |

A full manual pass is: pick the data → pick a group column → look at all three statistics
tabs → run the models → check the results tabs, single-group **and** multigroup.

---

## 9. The house style

Written down because it is a deliberate choice, not an accident:

- **Long and obvious beats short and clever.** Explicit `for` loops rather than `Reduce()`,
  `do.call()` or `apply()` chains. The app is not slow — fitting all five models takes about
  a third of a second — so there is no reason to trade clarity for speed.
- **No functions defined inside other functions.** If you have to follow a chain of four calls
  to find out what a number is, that is a bug in the writing.
- **But no copy-paste either.** A number that appears in three places will eventually
  disagree with itself. Write it once and read it from there — that is what `cttModelFamily()`
  is for.
- **Comments say what the line does and why, next to the line.** Long jargon-heavy blocks at
  the top of a file are not useful. A note about history is only worth writing if it names the
  actual old code and the actual problem with it.
- **camelCase throughout**, and reactive values end in `RV`.
