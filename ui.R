
ui <- dashboardPage(
  dashboardHeader(title = "SWOT Error Diving"),
  dashboardSidebar(
    radioButtons("varselect", "Variable", 
                 choices = c("area_total", "width", "wse"), 
                 selected = "width"),
    radioButtons("refdemselect", "Reference DEM", choices = c("GDEM", "SRTM"),
                 selected = "GDEM"),
    radioButtons("aggselect", "Pixel Aggregation", 
                 choiceNames = c("Simple", "Water Fraction", "Composite"),
                 choiceValues = c("simple", "frac", "composite"), 
                 selected = "frac"),
    checkboxInput("flagnodes", "Remove Ambiguous Nodes", value = FALSE)
  ),
  dashboardBody(
    fluidRow(
      column(width = 6,
        box(title = "", width = NULL,
            strong("Reach Errors"), 
            actionLink("topleft_modal", label = "", icon = icon("info-circle")),
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
        box(title = "", width = NULL,
            strong("Node Error Accumulation"), 
            actionLink("topright_modal", label = "", icon = icon("info-circle")),
            p('2. Drag to select nodes and click "Plot" to bring up pixel maps.',
              " (You may need to click a second time.)"),
            actionButton("nodePlot", "Plot"),
            checkboxInput("err_rel", "Standardize", value = TRUE),
            plotlyOutput("node_accum")),
        box(title = NULL, width = NULL,
            strong("Pixel Accumulation"), 
            actionLink("bottomright_modal", label = "", icon = icon("info-circle")),
            plotOutput("pix_accum"))
      ) 
    )
  )
)

