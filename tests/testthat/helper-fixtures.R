## Fixtures for the characterization tests.
##
## These tests pin down the behaviour of the pure helpers before the module migration
## restructures the app around them. They deliberately assert structure and numbers with
## tolerance rather than snapshotting rendered HTML: the point is to catch a helper that
## silently changes what it computes, not to freeze kableExtra's markup.
##
## rtdata ships with the package. rtdataNA (same data with missings, for the FIML path) and
## rtdataWarn (60 x 7, item_2 a near-duplicate of item_1, which drives tko/ete/teq into a
## genuine Heywood case) are copied in from the debugging fixtures in "KTT App/".

itemNames <- function() paste0("item_", 1:6)

loadFixture <- function(name) {
  env <- new.env(parent = emptyenv())
  load(test_path("fixtures", paste0(name, ".RData")), envir = env)
  get(name, envir = env)
}

## Fit one CTT model the way server.R does, so the tests exercise the same lavaan options
## the app uses. Kept in step with the lavaan() call in server.R by hand.
fitCTT <- function(data,
                   model = "tko",
                   items = itemNames(),
                   group = FALSE,
                   etaIntFree = FALSE,
                   estimator = "ML",
                   missing = "listwise") {
  codes <- makeModelCodes(data, items, group = group, etaIntFree = etaIntFree)

  suppressWarnings(lavaan::lavaan(
    model = codes[[model]],
    data = data,
    meanstructure = TRUE,
    group = if (isFALSE(group)) NULL else group,
    group.equal = if (isFALSE(group)) NULL else c("loadings", "intercepts"),
    estimator = estimator,
    missing = missing,
    int.ov.free = TRUE,
    int.lv.free = etaIntFree,
    auto.fix.first = TRUE,
    auto.fix.single = TRUE,
    auto.var = TRUE,
    auto.cov.lv.x = TRUE,
    auto.efa = TRUE,
    auto.th = TRUE,
    auto.delta = TRUE,
    auto.cov.y = TRUE))
}
