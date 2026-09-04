## Step 1 offers the objects lying around in R only when the app is running in somebody's
## own console - .onLoad() sets the option from interactive(), and a test run is not. The
## tests below read from the workspace because it is the shortest way to hand step 1 a data
## set, so pin it here; the ones that check the hosted behaviour set it to FALSE themselves.
options(shinyCTT.workspace = TRUE)
