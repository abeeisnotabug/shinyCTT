fluidPage(

    # To be updated later: nice alignment of item checkbox group! ---------------------
    tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
    # ---------------------------------------------------------------------------------

    navbarPage("shinyCTT",
               id = "navbar",
               inputPanel
               #position = "fixed-top",
               #header = tagList(p(),
              #                  actionButton("goDescrStats",
              #                               "Calculate descriptive statistics!"),
              #                  hr()
              #          )
    )
)
