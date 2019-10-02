
ui <- dashboardPage(
  dashboardHeader(title = "SWOT Error Diving"),
  dashboardSidebar(
    radioButtons("varselect", "Variable", 
                 choices = c("area_total", "width", "wse"), 
                 selected = "width"),
    radioButtons("refdemselect", "Reference DEM", choices = c("GDEM", "SRTM"),
                 selected = "GDEM"),
    radioButtons("aggselect", "Pixel Aggregation", 
                 choices = c("simple", "frac", "composite"), selected = "frac"),
    checkboxInput("flagnodes", "Remove Ambiguous Nodes", value = FALSE)
  ),
  dashboardBody(
    fluidRow(
      column(width = 6,
        box(title = "Reach Errors", width = NULL,
            p("1. Click a point to bring up node errors"),
            plotlyOutput("reach_errplot")),
        tabBox(title = "Map", width = NULL,
               tabPanel("Lat/Lon",
                        p("Circle sizes reflect (pixel area) * (water fraction)"),
                        checkboxInput("map_gdem", "Show GDEM Truth"), 
                        leafletOutput("rtmap")),
               tabPanel("Slantplane (PIXC)", width = NULL,
                        plotOutput("slantmap")),
               tabPanel("Slantplane (GDEM)", width = NULL,
                        plotOutput("slantmap_gdem")))
        ),
      column(width = 6,
        box(title = "Node Error Accumulation", width = NULL,
            p('2. Drag to select nodes and click "Plot" to bring up pixel maps.',
              " (You may need to click a second time.)"),
            actionButton("nodePlot", "Plot"),
            # actionButton("nodePurge", "Purge"),
            # actionButton("nodeRestore", "Restore All"),
            checkboxInput("err_rel", "Standardize", value = TRUE),
            plotlyOutput("node_accum")),
        box(title = "Pixel Accumulation", width = NULL,
            plotOutput("pix_accum"))
      )
    )
  )
)

