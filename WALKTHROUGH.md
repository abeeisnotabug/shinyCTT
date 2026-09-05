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
   they change it loads the data into the box's `raw()` and checks it has more than one
   column and at least one numeric one. If not, it disables the Select button and posts a
   notification.

   There are five sources, and the first two only sometimes. **Supplied data** is whatever
   `shinyCTTApp(data = list(scores = myData))` was given; when there is any it comes first, so
   a visitor opens the app on it. **Workspace** lists the objects lying around in R and is
   offered only when the app was started from somebody's own console —
   `shinyCTTApp(workspace = )`, which defaults to `interactive()`, and is FALSE on a server,
   where `globalenv()` holds whatever was left there by whoever put the app up. **CSV** and
   **SPSS** are uploads. **R data file** is an upload too, and takes both kinds: an `.RData`
   (or `.rda`) is loaded into an environment of its own and the user picks one of its objects,
   an `.rds` holds one object and is the data straight away. An upload may be 50 MB.

   The object chooser under the file inputs serves the supplied data, the workspace and the
   `.RData`, and draws nothing for the rest — and an uploaded file with no table in it at all
   says "No data set found in this file" rather than drawing an empty chooser and leaving it
   at that.
2. Pressing Select runs `observeEvent(input$dataSelectButton, ...)` *inside*
   `mod-data-source.R`, which copies the data into that box's `chosen()`. The box hands
   `chosen` back, and `server.R` watches it — `observeEvent(dataSource$chosen(),
   appStage("subset"))` at `server.R:143`. The Select button is the only thing that fills
   `chosen()`, so watching the value is the same as watching the button, and `server.R`
   never has to reach into the box.
3. `appStage("subset")` is what makes everything else happen — see section 4.

### "Select" on the subset tab (`subsetSelectButton`)

Same shape. Inside `mod-data-subset.R` the button fills `data()` — the data cut down to the
chosen items and groups — and `server.R` watches that (`observeEvent(subset$data(),
appStage("statistics"))` at `server.R:157`). The box hands back seven answers in all:
`data`, `itemCols`, `groupCol`, `groups`, `hasGroups` (whether the group column is usable),
`useFIML` (whether FIML is in play) and `incompleteCases`.

### "Fit and compare models" (`goModels`)

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

**The sidebar.** `sidebarGroups(stage, doMg)` in `R/helpers-sidebar.R` builds the entire menu from
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
> job; you do not need to write a `disable()` call. Steps 1 and 2 are the exception — those
> two boxes freeze their own controls, from a `frozen` argument the app hands them, so a new
> control there goes in that box's own list.

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
  What you type is never written over — an empty box, or a number out of range, simply is not
  taken, and the tables go on showing the last usable value with a red note under the box
  saying so. (Correcting the box instead would rewrite it mid-word; see `GOTCHAS.md`.)
- **Changing the confidence level of the RMSEA interval** widens or narrows that interval in
  the fit index table, and relabels its header. Also no refit — the interval is worked out
  from the fitted model by `fitMeasures(fm.args = list(rmsea.ci.level = ...))`.
- **Changing the estimator** *does* need a refit, because it changes how the models are
  fitted. The app does not do it silently: an orange note appears under the button and the
  button's label gains a `*`, until the user presses it.

**Every fit lands you on the model comparison tab**, the first one and every one after it.
The first happens by itself, because the sidebar block being revealed comes up selected; the
later ones are `bslib::nav_select("dataMenu", selected = "modelTests")` at the end of the
button's observer (`server.R:402`).

**The button is switched off while there is nothing to fit.** It is live before the first run,
and afterwards only while the chosen estimator differs from the one the models on screen were
fitted with — `refitPending()` compares the two. Choose a different estimator and change your
mind, and it goes off again, because pressing it would give back the same results.

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

**Three prefixes, and the prefix says what the file is.** `mod-*` is a Shiny module (a box:
its controls and everything that reacts to them). `fun-*` is one function, and the file is
named after it. `helpers-*` is several functions on one subject. `ui.R`, `server.R`,
`shinyCTTApp.R`, `zzz.R` and `data.R` keep their own names — they are the app itself, or
names R expects.

| File | What's in it |
| --- | --- |
| `R/helpers-model-family.R` | The five models: names, labels, how many items each needs, how they nest. **Start here** if you want to change anything about the models themselves. |
| `R/fun-makeModelCodes.R` | Turns the chosen item columns into five lavaan syntax strings. The models differ only in which parameter labels they re-use. |
| `R/fun-comparisonGrid.R` | Draws the 5×5 table of checkboxes on the Testing Parameters tab. |
| `R/mod-*.R` | One box each, moved out of `server.R` and `ui.R` — see section 5b. |
| `R/mod-testing-params.R` | The whole Testing Parameters tab: the estimator, the mean structure, the multigroup box, the two display settings, the model grid and the button. It fits nothing — it hands the settings back and `server.R` does the fitting. |
| `R/mod-mvn.R` | The whole normality tab. It *reports* which estimator its test points to; `server.R` decides what to do about that. |
| `R/mod-data-source.R` | Step 1: where the data comes from. |
| `R/mod-data-subset.R` | Step 2: which items, which groups, missing values. Hands back the seven answers the rest of the app works from. |
| `R/mod-ctt-results.R` | Everything a model run produces: the comparison page, the parameter tables, the factor scores, the model code. Started twice — once for the whole sample, once for the groups. |
| `R/helpers-extract.R` | Pulls fit indices and parameter estimates out of a fitted lavaan object. Contains the reliability confidence intervals. |
| `R/helpers-tables.R` | Every table the app shows. They are `reactable`s: a table works out whether each cell is good, bad or neutral, and the colour is looked up from that. |
| `R/helpers-colors.R` | **Every colour the app draws with, and nothing else names one.** The plots' green, the three the tables rate a cell with, and the palette for group-wise plots. |
| `R/helpers-translations.R` | `tr()`, which turns a short name like `common.select` into the words on screen, in whichever language the visitor is reading. |
| `inst/translations.csv` | **All the text.** One row per piece, one column per language. This is the file to edit to change a word. |
| `inst/styles.css` | **How everything looks** that bslib has no setting for: the green bar, the menu, the boxes. This is the file to edit to change a size or a colour — see section 6b. |
| `R/helpers-cards.R` | The two shapes of box the app draws with. Change how a box behaves here and it changes everywhere. |
| `R/helpers-advanced.R` | Factor scores, and the copy-pasteable R script shown on the Model Code tab. |
| `R/ui.R` | The page layout: which boxes sit on which tab. |
| `R/helpers-look.R` | `fuTheme()`, the settings bslib has names for, and `fuStyle()`, the two lines that read `inst/styles.css` into the page. |
| `R/helpers-sidebar.R` | The left-hand menu, and the four stages the app steps through. |
| `R/server.R` | Everything that computes or reacts. Long, but sectioned — see below. |

### 5b. The `mod-*.R` files

Seven boxes now live in a file of their own rather than in `server.R` and `ui.R`: the
descriptive statistics, the histogram, the covariance matrix, the scatter plot, the test on
correlative independence, the correlation table, and the normality tab. So does everything a
model run produces, in `R/mod-ctt-results.R`.

**That last one is started twice** — `cttResultsServer("single", ...)` and
`cttResultsServer("multigroup", ...)` — which is why nothing in the results half has `Mg`
pasted on the end of its name any more. The names inside the file are plain (`hierPlot`,
`fitsTable`), and `NS()` keeps the two runs apart on the page (`single-hierPlot`,
`multigroup-hierPlot`). Its four UI functions share one id because the four pages they fill
are four tabs of one result. Each file holds the box's *whole* self —
what it looks like, the controls in it, and what it computes.

Each has two functions. `histogramUI("histogram")` is what `ui.R` puts on the page, and
`histogramServer("histogram", ...)` is what `server.R` starts up, with the same name string in
both. The server function is given what the box needs as arguments, so it never reaches into
the rest of the app:

```r
  histogramServer(
    "histogram",
    data = subset$data,
    itemCols = subset$itemCols,
    groupCol = subset$groupCol,
    hasGroups = subset$hasGroups,
    groupColors = groupColors)
```

Note the arguments are handed over *unread* — `subset$data`, not `subset$data()`. The box
reads them itself, as `data()` and `itemCols()`, and re-draws when they change. Reading them
here instead would freeze them at the value they had when the app started.

`subset` is what `dataSubsetServer("subset", ...)` handed back, a few lines above. That is
how every box gets what it needs: one box's answers passed into the next one's arguments,
never a control read across a box boundary.

**Inside such a file, every id the box creates goes through `ns()`** — `plotOutput(ns("x"))`,
`selectInput(ns("y"), ...)`. That is what keeps two boxes from fighting over the same name.
Two things do *not* take `ns()`: reading a control back (`input$histItem`, plain), and telling
shinyjs to show or hide one (`shinyjs::show("corrTabNA")`, plain — shinyjs adds the box's name
itself). Both mistakes are silent, so `devtools::test()` checks for them.

`server.R` is navigated by its `----` markers. In RStudio the outline pane (top right of the
editor, or <kbd>Ctrl/Cmd</kbd>+<kbd>Shift</kbd>+<kbd>O</kbd>) turns them into a clickable
table of contents. The number of `#` marks the nesting depth:

```r
  # dataSelectionTab ----                              <- a whole tab
  ## dataSelectionTab objectChooser ----               <- one output within it
```

**Keep adding these when you add code.** They are the only navigation aid in a 432-line file,
and the same is true of every `mod-*.R` — `mod-ctt-results.R` is 674 lines.

---

## 6. How to make the four most likely changes

### "I want to change a label the user sees"

**Open `inst/translations.csv` and edit that row. Nothing in `R/` holds the words.**

Each row is one piece of text: a short name, then one column per language. The code says
`tr("subset.items.label")`; the row says what that is in English, German and French. To find
the row, search the file for the English words.

Three things to know while you are in there:

- **Leave the `%s`, `%i` and `_TOTAL_` alone.** The code fills those in, by position. A
  translation that drops one either loses a number or stops the app on that screen —
  `devtools::test()` checks for it, so you will be told.
- **Do not put `<b>` or `<i>` in.** Where a sentence needs them, the code puts them around a
  `%s`, so that German and French can put the emphasised words in a different place.
- **The `sym.*` rows are symbols**, not sentences — a sigma is a sigma in every language, so
  they are left untranslated and may carry `<sub>`. They are also where the real Greek
  characters live: `R CMD check` refuses non-ASCII characters in `R/`, which is half of why
  the text moved out here in the first place.

The five model names are in that file too, three rows each — `model.tko.long` for tab
titles, `model.tko.abbrev` for table headers, and `model.tko.plot` for the hierarchical
plot, which cannot use HTML and needs R's plotmath instead (a `~` there means a space).

### "I want to add a language"

1. A new column in `inst/translations.csv`, headed with its two-letter code.
2. That code added to `appLanguages` in `R/helpers-translations.R`.
3. A `sym.lang.<code>` row holding its flag and its own name for itself.
4. One line in `languageLabels()`, next to `appLanguages`.

Written out one by one rather than built with `paste0()`, because the test reads the source
looking for `tr("...")` and cannot see a name that is assembled while the app runs.

### "I want to add or remove a model"

1. `inst/translations.csv` — three rows for its name: `model.<x>.long`, `model.<x>.abbrev`
   and `model.<x>.plot`.
2. `cttModelFamily()` — add its short name to `models`, add one `tr()` line to each of
   `long`, `abbrev` and `plot$name`, one number to `minItems`, its nesting edges to
   `hierarchy`, and a row to `plot` saying where to draw it.
3. `fun-makeModelCodes.R` — say which parameters it constrains.

That is all. The comparison grid, the list of valid comparisons, the item-count limits and
the checkbox ids all follow from step 1.

The reason it follows: `comparable` is not a list anyone maintains. `nestedPairs()` walks the
`hierarchy` edges and works out which models sit inside which. If two models are not
connected by any chain of edges — as with tau-equivalent and essentially tau-parallel, which
have the same degrees of freedom — no comparison is offered and the grid prints
"Not testable." in that cell.

### "I want to change how many items a model needs"

`minItems` in `cttModelFamily()`, and nowhere else. `comparisonGrid()` reads it to decide
when to show "Too few items." (`fun-comparisonGrid.R:62` and `:128`), and
`mod-testing-params.R:209` reads the same vector to untick models the user cannot test.

There is one remaining copy of these numbers written out in prose — the notification text
that says "Only three items selected. Unable to test the τ-kongeneric model." That is a
sentence, not logic, so it does not follow automatically. Update it by hand.

### "I want to add a tab"

1. Write the tab as a module, `R/mod-my-thing.R` — every tab is one. Copy the smallest,
   `mod-covmatrix.R`, and start from that.
2. Add the panel to the `navset_hidden()` in `ui.R`, and a `myThingServer("myThing", ...)`
   in `server.R`. Put the box in a column, even when it is the only one on the tab and takes
   the whole width:

   ```r
   bslib::nav_panel_hidden(
     "myTab",
     fluidRow(
       column(width = 12, myThingUI("myThing"))))
   ```

   `width` is in twelfths, so two boxes side by side are `column(width = 6, ...)` twice.
   Without the column the box hangs 12px past the edge of the page — see GOTCHAS.md,
   "Bootstrap 5 gives every child of a row the full width of it".
3. Add `"myTab"` to `tabNames` in `helpers-sidebar.R`. That is what makes `server.R` listen
   for the link.
4. Add a `navEntry("myTab", ...)` to the right block in `sidebarGroups()`.

The three `"myTab"`s must match exactly — that string is how the click on the menu entry
finds the panel. There is no error if they disagree: with step 3 missing the entry is drawn
and does nothing, and with step 2 missing the click switches to a panel that is not there.

---

## 6b. Changing how it looks

Written for someone who does not speak CSS. Nothing here is hard; it is one idea repeated.

### The two files, and which one you want

| where | what belongs there | how to spot it |
| --- | --- | --- |
| `fuTheme()` in `R/helpers-look.R` | sizes and colours **Bootstrap already has a name for** | `"font-size-base" = "0.8125rem"` |
| `inst/styles.css` | everything else | `.cttBrand { font-size: 20px; }` |

Bootstrap is the set of ready-made looks bslib builds on. It keeps a few dozen settings —
the base text size, the main colour, the corner radius — and works nearly everything else out
from them. When what you want to change is one of those, `fuTheme()` is the place: one line,
and every label, button and table cell moves together. When it is not, you write a rule
yourself, and that goes in the style sheet.

There is no third place, and putting a rule in the wrong one fails **silently** — see
"Where a rule can lose" below.

### What a rule looks like

Three parts. From `inst/styles.css`:

```css
.cttBrand {
  font-size: 20px;
  color: #FFFFFF;
}
```

- `.cttBrand` is the **selector**: which things on the page this applies to. The dot means
  "anything carrying the name `cttBrand`". `R/ui.R` puts that name on the app's title with
  `span(class = "cttBrand", "shinyCTT")`.
- `font-size` and `color` are **properties**: what about them to change.
- `20px` and `#FFFFFF` are the **values**.

Everything in the file is that shape. The `/* ... */` blocks are comments.

### Finding the name of the thing you want to change

You do not have to guess, and you should not.

1. Run the app, right-click the thing, choose **Inspect** (Chrome, Firefox and Safari all
   have it; Safari needs Develop mode switched on first).
2. The panel that opens shows the page's own markup with your thing highlighted. Read its
   `class="..."` — those are the names it answers to.
3. Beside it, a **Styles** panel lists every rule that reached it, in the order they won,
   with the losing ones struck through. That tells you what you are up against.
4. You can type a change straight into that panel and watch it happen. Nothing is saved —
   it is a sketch pad. When it looks right, copy the rule into `inst/styles.css`.

Step 3 is the one that saves the most time. A change that "does nothing" is almost always
a rule that lost, and the panel says so.

### The four selectors this file uses

- `.cttMenu` — anything carrying that name.
- `.card .nav-tabs` (a space) — a `nav-tabs` **anywhere inside** a `card`, however deep.
- `.card > .card-header` (a `>`) — a `card-header` that is a **direct child** of a `card`,
  one level down and no further.
- `.cttMenu a, .cttMenu summary` (a comma) — two selectors sharing one set of properties.
  Nothing more than shorthand.

There is one more you will see: `.card.bslib-card`, two names with no space, meaning
something carrying **both** names at once. That is not the same as `.card .bslib-card`.

### Where a rule can lose

When two rules set the same property on the same thing, one wins. The ordering, simplified
to what this file actually uses:

1. Count the names in each selector. **More names win.** `.card.bslib-card` (two) beats
   `.bslib-card` (one). `.card > .card-header .nav-tabs > li > a` (four) beats almost
   anything.
2. On a tie, **whichever the browser read last wins.**

Both halves matter here, because bslib serves two style sheets and **its own comes second**:
the theme it compiles from `fuTheme()`, and then its `components.css`. So a rule you put in
the theme loses every tie against bslib's own. `inst/styles.css` is written into the page
after both, which is why it lives there and not in `bslib::bs_add_rules()`. Three things
that looked broken during the migration were all this one cause — GOTCHAS.md, "bslib", has
them.

The practical rule: **if your change does nothing, add one more name to the front of your
selector** and look again. There is a blunt instrument, `!important`, which wins outright;
the file uses it once, on `.checkbox-inline`, and it should stay that rare, because a page
where several rules all shout has no ordering left to reason about.

### px and rem, and what follows the text size

`16px` is sixteen dots. `1rem` is "whatever the page's base text size is", so `0.8125rem`
is 13/16 of it. The difference decides what moves when you change `font-size-base`:

- Anything written in `rem`, and anything with no size of its own, **follows**. That is most
  of the app: form labels, buttons, table cells, the DT furniture.
- Anything written in `px` in `inst/styles.css` **stays put**. Today that is the title in the
  green bar (20px), the bell, the menu entries (13px), the box headings (16px), the three
  boxes at the top of step 2, and the green hint box.

That is why dropping the base size to `0.8125rem` left the box headings where they were and
made them stand out more — the headings never followed it in the first place.

### The app's own names

Everything the package names itself starts `ctt`, so a search for `ctt` in
`inst/styles.css` finds all of them. Where each gets put on:

| name | what it marks | set in |
| --- | --- | --- |
| `cttHeader`, `cttBrand`, `cttHeaderRight` | the green bar, its title, the right-hand end | `R/ui.R` |
| `cttBell`, `cttNotifications` | the bell and the list it opens | `R/server.R` |
| `cttMenu`, `cttSubMenu`, `cttSelected` | the menu, a folding block, the current entry | `R/helpers-sidebar.R` |
| `cttTitleRight` | a tab strip with its title on the right | `R/helpers-cards.R` |
| `cttValueBox` | the three boxes at the top of step 2 | `R/mod-data-subset.R` |
| `cttHintBox` | the green hint under the normality plot | `R/mod-mvn.R` |

Everything else in the file — `.card`, `.navbar`, `.form-control`, `.nav-tabs` — is
Bootstrap's or bslib's, not ours, so changing those rules changes every one of them at once.

### A worked example

Say the box headings should be bigger and in a serif face. They are `.card > .card-header`
in `inst/styles.css`:

```css
.card > .card-header {
  background-color: #FFFFFF;
  border-bottom: 0;
  padding: 10px;
  font-size: 16px;             /* <- raise this */
  font-weight: 500;            /* 400 plain, 500 semi-bold, 700 bold */
  color: #000000;
  font-family: Georgia, serif; /* <- and add this */
}
```

One catch, and the inspector would have shown it: the tab strips sit in that same header, so
the tab labels inherit whatever you set and grow with the headings. If you want them left
alone, give them their own size in the rule below it,
`.card > .card-header .nav-tabs > li > a`.

### Seeing the change

Editing the style sheet is not enough on its own. Stop the app, then:

```r
devtools::load_all(".")
shinyCTTApp()
```

`load_all()` is what re-reads the file, and the app has to be started again because the one
you stopped had already built its page. If a change still is not there, open the inspector
and look at the Styles panel — either your rule is not in the list at all (the file was not
re-read) or it is struck through (it lost).

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
devtools::test()     # the calculation helpers, the module id checks, and the text file
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

**Two things only a browser shows.** `devtools::load_all()` hides a whole class of shinyjs
bug, so anything touching enable/disable has to be checked against an installed build
(`R CMD INSTALL`); and a wrong colour on a right number is invisible to every test there is.
When the table code changes, open the old build and the new one and compare what is painted.

---

## 9. The house style

Written down because it is a deliberate choice, not an accident:

- **Long and obvious beats short and clever.** Explicit `for` loops rather than `Reduce()`,
  `do.call()` or `apply()` chains. The app is not slow — fitting all five models takes about
  a third of a second — so there is no reason to trade clarity for speed.
- **No chains of calls you have to follow to find out what a value is.** If you have to follow
  four calls to find out what a number is, that is a bug in the writing. A function written
  inside another one is fine when it is used more than once and reads where it stands — what is
  not fine is one that exists only to move code somewhere else.
- **But no copy-paste either.** A number that appears in three places will eventually
  disagree with itself. Write it once and read it from there — that is what `cttModelFamily()`
  is for.
- **Comments say what the line does and why, next to the line.** Long jargon-heavy blocks at
  the top of a file are not useful. A note about history is only worth writing if it names the
  actual old code and the actual problem with it.
- **camelCase throughout**, and reactive values end in `RV`.
