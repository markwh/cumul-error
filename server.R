
# A stripped-down app for showing only how error propagates along with uncertainty 
# across scales: pixel to node, node to reach. 
#

load("cache/rtnodes_master.RData")
load("cache/gdnodes_master.RData")
load("cache/reachvaldf_raw_master.RData")
load("cache/reachvaldf_master.RData")
load("cache/pixdf.RData")
# load("cache/gdempixdf.RData")

# Pre-compute node pixel bbox.
pixbboxdf <- pixdf %>% 
  group_by(node_index) %>% 
  summarize(minlat = min(latitude, na.rm = TRUE),
            maxlat = max(latitude, na.rm = TRUE),
            minlon = min(longitude, na.rm = TRUE),
            maxlon = max(longitude, na.rm = TRUE))

# TODO: implement file upload
# TODO: Add documentation, guidance text
# TODO: fix crash on Reference DEM toggle
# TODO: don't rezoom leaflet except on change of selNode. 

####------------------------------------
#### START OF SERVER FUNCTION ----------
####------------------------------------
function(input, output, session) {
  
  #### DATA INPUT ####
  
  # purgedNodes <- numeric(0) # Keep track of which nodes get manually purged
  # purgeCounter <- 0
  # restoreCounter <- 0 # Track whether node restoration has been triggered.
  
  # subset of data given by side panel selections

  nodedata_in <- reactive({ 
    rt_nodes <- rtnodes_master %>% 
      filter(refdem == input$refdemselect,
             agg == input$aggselect)
    gdem_nodes <- gdnodes_master
    
    if (input$flagnodes) gdem_nodes <- gdem_nodes %>% 
      filter(!ambiguous)
    
    
    return(list(rt_nodes = rt_nodes, gdem_nodes = gdem_nodes))    
  })

  
  # Current dataset (subset of data_in to run and reach)
  rtdata <- reactive({

    # TODO: add purging of nodes
    selreach <- reachvaldf_shared()$data(withSelection = TRUE) %>% 
      filter(selected_)
    if (!nrow(selreach)) return(NULL)
    
    rtnodes <- nodedata_in()$rt_nodes %>% 
      dplyr::filter(run == selreach$run,
                    reach_id == selreach$reach_id)
    gdnodes <- nodedata_in()$gdem_nodes %>% 
      dplyr::filter(run == selreach$run,
                    reach_id == selreach$reach_id)
    
    out <- list(rt_nodes = rtnodes, gdem_nodes = gdnodes)
    out
  })
  
  selected_day <- reactive({
    if (is.null(reachvaldf_shared()$data)) return(NULL)
    reachvaldf_shared()$data(withSelection = TRUE) %>% 
      filter(selected_) %>% 
      pull(day)
  })
  selected_reach <- reactive({
    if (is.null(reachvaldf_shared()$data)) return(NULL)
    reachvaldf_shared()$data(withSelection = TRUE) %>% 
      filter(selected_) %>% 
      pull(reach_id)
  })
  
  
  # Color palette for reaches, days
  reachpal <- reactive({
    reachids <- sort(unique(reachvaldf_master$reach_id))
    nreaches <- length(reachids)
    # viridisLite::viridis(n = nreaches) 
    dkcols <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
    pal <- leaflet::colorNumeric(palette = rep(dkcols, length.out = nreaches), 
                                 domain = reachids)
    pal
  })
  reachcolvec <- reactive({
    reachids <- sort(unique(reachvaldf_master$reach_id))
    setNames(reachpal()(reachids), reachids)
  })
  
  daypal <- reactive({
    days <- sort(unique(reachvaldf_master$day))
    ndays <- length(days)
    # viridisLite::viridis(n = nreaches) 
    dkcols <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
    pal <- leaflet::colorNumeric(palette = rep(dkcols, length.out = ndays), 
                                 domain = days)
    pal
  })
  daycolvec <- reactive({
    days <- sort(unique(reachvaldf_master$day))
    setNames(daypal()(days), days)
  })
  
  
  # subset of pixdf for maps
  sel_bbox <- reactive({
    input$nodePlot
    gddf <- pcv_gdem_selday()
    if (is.null(gddf)) gddf <- pixdf
    pcbbox <- pixdf %>% 
      filter(day == selected_day(), refdem == input$refdemselect) %>% 
      rt_nodebbox(selNode, dilate = 0.03) 
    gdbbox <- gddf %>% 
      rt_nodebbox(selNode, dilate = 0.03)
    out <- list(
      minlat = min(pcbbox$minlat, gdbbox$minlat),
      minlon = min(pcbbox$minlon, gdbbox$minlon),
      maxlat = max(pcbbox$maxlat, gdbbox$maxlat),
      maxlon = max(pcbbox$maxlon, gdbbox$maxlon)
    )
    out
  })
  pcv_selected <- reactive({
    if (!length(selected_day())) return(NULL)
    input$nodePlot
    out <- pixdf %>% 
      filter(day == selected_day(), refdem == input$refdemselect) %>% 
      grab_bbox(sel_bbox()) %>% 
      mutate(longitude = ifelse(is.na(longitude_vectorproc),
                                longitude, longitude_vectorproc),
             latitude = ifelse(is.na(latitude_vectorproc),
                                latitude, latitude_vectorproc),
             alpha = ifelse(node_index %in% selNode, 0.8, 0.3),
             classcolor = classpal(classification))
    if (input$aggselect == "simple") {
      out <- out %>% 
        filter(classification %in% c(3, 4, 23, 24)) %>% 
        mutate(water_frac = 1)
    } else if (input$aggselect == "composite") {
      out <- out %>% 
        mutate(water_frac = ifelse(classification %in% c(4, 23, 24),
                                   1, water_frac))
    }
    out
  })
  
  # Data frame with selected day's gdem pixc(vec)
  pcv_gdem_selday <- reactive({
    input$nodePlot
    if (!length(selected_day())) return(NULL)
    load(sprintf("cache/gdpix%s.RData", selected_day()))
    gdempixdf <- get(sprintf("gdpix%s", selected_day()))
    rm(list = sprintf("gdpix%s", selected_day()))
    gdempixdf
  })
  
  
  # Data frame with locations of selected nodes' gdem pixc(vec)
  pcv_gdem_selected <- reactive({
    if (!length(selNode) || !length(selected_day())) return(NULL)
    
    plotdf <- pcv_gdem_selday() %>%
      grab_bbox(sel_bbox()) %>% 
      mutate(alpha = ifelse(node_index %in% selNode, 0.8, 0.3))
    plotdf
  })
  
  
  #### VALIDATION ####
  
  # Data objects 
  valdata_node <- reactive({
    if (is.null(rtdata())) return(NULL)
    rt_valdata_df(obs = rtdata()$rt_nodes, truth = rtdata()$gdem_nodes)
  })
  
  valdata_reach_orig <- reactive({
    masterdf <- if (input$flagnodes) reachvaldf_master else reachvaldf_raw_master
    
    
    masterdf %>% 
      filter(refdem == input$refdemselect,
             variable == input$varselect,
             agg == input$aggselect) %>% 
      mutate(relerr = pixc_err / sigma_est,
             day = as.factor(day))
  })
  reachvaldf_shared <- reactive({
    input$refdemselect
    SharedData$new(valdata_reach_orig)
  })
    
  
  #### REACH SCATTERPLOT ####
  
  # plotly object for scatterplot
  output$reach_errplot <- renderPlotly({
    # event.data <- event_data("plotly_click", source = "select")
    gg <- ggplot(reachvaldf_shared(), aes(x = reach_id)) +
      geom_ribbon(aes(ymin = -1.96, ymax = 1.96), color = "#dddddd") +
      geom_ribbon(aes(ymin = -1, ymax = 1), color = "#bbbbbb") +
      geom_point(aes(y = relerr), color = "#ddbbbb", 
                 data = valdata_reach_orig()) +
      geom_point(aes(y = relerr, color = day)) +
      scale_color_manual(values = daycolvec()) +
      ylab("Standardized Error") +
      scale_x_continuous(breaks = 1:10)
    
    ggplotly(gg, tooltip = "text")
    
  })
  
  
  #### ERROR ACCUMULATION PLOT ####
  nodeaccumdf <- reactive({
    input$err_rel
    if (is.null(valdata_node()) || nrow(valdata_node()) == 0) return(NULL)
    scalearg <- ifelse(input$err_rel, "unc", "none")
    out <- try(rt_cumulplot(valdata_node(), var = input$varselect, 
                            plot = FALSE, scale = scalearg))
    if (inherits(out, "try-error")) return(NULL)
    out$nodeid_chr <- as.character(out$node_id)
    out$errtype = factor(out$errtype, levels = c("error", "cumul. error"))
    out
  })
  # nodeaccum_shared <- SharedData$new(nodeaccumdf, key = ~nodeid_chr)
  nodeaccum_shared <- reactive({
    nodeaccumdf()
    out <- SharedData$new(nodeaccumdf(), key = ~nodeid_chr)
    selrows <- nodeaccumdf()$node_id %in% selNode
    if (sum(selrows) > 0) out$selection(selrows)
    out
  })
  output$node_accum <- renderPlotly({
    if (is.null(nodeaccum_shared()$data())) return(NULL)
    gg <- ggplot(nodeaccum_shared(), 
                 aes(x = node_id, y = y, color = reach_id)) + 
      # geom_line(size = 0.1) + 
      geom_point(aes(text = node_id), size = 0.5) + 
      facet_grid(rows = vars(errtype), scales = "free_y")
    
    ggplotly(gg, tooltip = "text") %>% 
      layout(dragmode = "select") %>% 
      hide_legend() %>% 
      layout(selectdirection = "h", dragmode = "select") %>% 
      highlight(on = "plotly_selected")
  })
  
  # selected nodes
  selNode <- numeric(0)
  selReach <- 0
  observe({ # update selNode
    input$nodePlot
    reachvaldf_shared()$data(withSelection = TRUE)
    if (is.null(nodeaccum_shared()$data())) return()
    if (selReach != selected_reach()) {
      print("NEW REACH!")
      # # nodeaccum_shared()$clearSelection()
      # nodeaccum_shared() <<- SharedData$new(nodeaccumdf())
      selReach <<- selected_reach()
      selNode <<- numeric(0)
    } else {
      print("SAME REACH!")
      seldf <- try(nodeaccum_shared()$data(withSelection = TRUE))
      if (inherits(seldf, "try-error")) browser()
      selNodes_cur <- filter(seldf, selected_) %>%
        pull(node_id)
      print(sprintf("SELNODES: %s", selNodes_cur))
      if (length(selNodes_cur)) selNode <<- selNodes_cur
    }
  })
  

  #### Pixel accumulation plot ####
  
  output$pix_accum <- renderPlot({
    if (is.null(pcv_selected()) || nrow(pcv_selected()) == 0) return(ggplot())
    if (input$varselect == "wse") {
      plotdf <- pcv_selected() %>% 
        filter(alpha > 0.5, # hacky check on whether in node selection or not
               classification == 4) %>% 
        left_join(rtdata()$gdem_nodes[c("node_id", "wse")], 
                  by = c(node_index = "node_id"))
      
      out <- ggplot(plotdf, aes(x = along_reach, y = height)) + 
        geom_point(aes(color = node_index, y = height)) +
        geom_line(aes(y = wse), color = "red", linetype = 1, size = 1) +
        ylab("wse") + xlab("Along-reach distance (m)") + 
        theme(legend.position = "bottom")
    } else {
      plotdf <- pcv_selected() %>% 
        filter(alpha > 0.5) %>% 
        arrange(desc(water_frac), classification)

      truthdf <- rtdata()$gdem_nodes %>% 
        filter(node_id %in% selNode) %>% 
        summarize(true_area = sum(area_total, na.rm = TRUE))
      
      out <- plotdf %>% 
        mutate(cum_area = cumsum(pixel_area), 
               area_lag = dplyr::lag(cum_area, default = 0)) %>% 
        ggplot() +
        geom_rect(aes(xmin = area_lag, ymin = 0, xmax = cum_area, 
                      ymax = water_frac, fill = classcolor)) +
        scale_fill_identity() +
        geom_rect(aes(xmin = 0, ymin = 0, xmax = true_area, ymax = 1),
                  fill = NA, color = classpal(1), linetype = 2, 
                  data = truthdf) + 
        xlab("Cumulative pixel area (m^2)") + 
        ylab("Water Fraction")
    }
    out
  })
  
  #### MAPPING ####

  # Unlike the larger shiny app, this one uses crosstalk, and for some unknown
  # reason that doesn't work in a leafletProxy. So the actual leaflet map must
  # contain pixels as well as nodes and tiles.
  # Proxies will contain gdem pixels and rezoom events.

  # Locations of nodes for a particular case
  allNodeLocations <- rtnodes_master %>% 
    dplyr::transmute(reach_id, node_id, lat = lat_prior, lon = lon_prior)
  riverNodeLocations <- reactive({
    if (is.null(rtdata())) return(NULL)
    nodedf <- rtdata()$gdem_nodes[c("reach_id", "node_id", "lat", "lon")]
    nodedf
  })
  
  # Base map with tiles, nodes, selected pixc
  output$rtmap <- renderLeaflet({
    locs <- allNodeLocations
    if (is.null(locs)) locs <- gdnodes_master
    basemap <- leaflet() %>%
      addTiles() %>%
      fitBounds(min(locs$lon), min(locs$lat),
                max(locs$lon), max(locs$lat)) %>% 
      mapOptions(zoomToLimits = "never")
    basemap
  })
  
  # Observer for pcv
  observe({
    input$nodePlot
    pcvdf <- pcv_selected()
    proxy <- leafletProxy("rtmap",
                          data = pcvdf) %>% 
      clearGroup("pcv")
    if (is.null(pcvdf) || nrow(pcvdf) == 0)
      return(proxy)
    proxy %>%
      addCircles(~longitude, ~latitude, stroke = FALSE,
                 radius = ~sqrt(pixel_area / pi * water_frac),
                 fillOpacity = ~alpha,
                 popup = ~paste(sprintf("reach: %s\nnode: %s",
                                        reach_index, node_index)),
                 fillColor = ~classpal(classification),
                 group = "pcv", data = pcvdf)
  })
  
  # Observer for node locations
  observe({
    locations <- riverNodeLocations()
    proxy <- leafletProxy("rtmap",
                          data = locations) %>% 
      clearGroup("nodes")
    if (!length(locations))
      return(leafletProxy("rtmap"))

    selpal <- colorFactor("Set1", c(TRUE, FALSE)) #TODO: change this

    proxy %>%
      addCircleMarkers(
        ~lon,
        ~lat,
        popup = ~paste(sprintf("reach: %s\nnode: %s", reach_id, node_id)),
        opacity = 0.3,
        group = "nodes",
        # color = "blue",
        radius = 2
      )
  })


  # Observer to add gdem pcv points
  observe({
    input$map_gdem
    if (!input$map_gdem ||
        is.null(pcv_gdem_selected()) ||
        (nrow(pcv_gdem_selected()) == 0)) {
      leafletProxy("rtmap") %>%
        clearGroup("pcv_gdem")
    } else {
      leafletProxy("rtmap", data = pcv_gdem_selected()) %>%
        clearGroup("pcv_gdem") %>%
        addCircles(~longitude, ~latitude, radius = ~sqrt(pixel_area / pi),
                   stroke = FALSE,
                   fillOpacity = ~alpha,
                   color = ~classpal(1),
                   group = "pcv_gdem")
    }
  })

  plotBounds <- reactive({
    input$nodePlot
    isolate({
      if (is.null(nodeaccum_shared()$data())) {print("no selection."); return()}
      # browser()
      selected_nodes <- nodeaccum_shared()$data(withSelection = TRUE) %>% 
        filter(selected_) %>% 
        pull(node_id)
    })
    bbox <- pixbboxdf %>% 
      filter(node_index %in% selected_nodes) %>% 
      summarize(minlat = min(minlat),
                maxlat = max(maxlat),
                minlon = min(minlon),
                maxlon = max(maxlon))
    bbox
  })
    
  # Observer for rezooming
  observeEvent(plotBounds(), {
    pcvdf <- pcv_selected()
    proxy <- leafletProxy("rtmap")
    if (is.null(pcvdf)) return(proxy)
    proxy %>%
      leaflet::flyToBounds(lng1 = plotBounds()$minlon,
                           lng2 = plotBounds()$maxlon,
                           lat1 = plotBounds()$minlat,
                           lat2 = plotBounds()$maxlat)
  })

  # Observer for pcv points legend
  observe({

    proxy <- leafletProxy("rtmap", data = pcv_selected())
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (length(selNode)) {
      proxy %>% addLegend(position = "topright",
                          colors = classpal(classes),
                          labels = classlabs)
    }
  })
  
  #### SLANT-PLANE MAP ####
  
  output$slantmap <- renderPlot({
    if (is.null(pcv_selected())) return(NULL)
    minx <- pcv_selected()[which.min(pcv_selected()$longitude), ]
    maxx <- pcv_selected()[which.max(pcv_selected()$longitude), ]
    flipx <- minx$range_index > maxx$range_index
    
    miny <- pcv_selected()[which.min(pcv_selected()$latitude), ]
    maxy <- pcv_selected()[which.max(pcv_selected()$latitude), ]
    flipy <- miny$azimuth_index > maxy$azimuth_index
    
    out <- pcv_selected() %>% 
      ggplot() + 
      geom_raster(aes(x = range_index, y = azimuth_index, 
                      fill = classcolor, alpha = alpha)) + 
      scale_alpha_identity() + 
      scale_fill_identity() + 
      theme(legend.position = "none")
    if (flipx) out <- out + scale_x_reverse()
    if (flipy) out <- out + scale_y_reverse()
    out
  })
  
  output$slantmap_gdem <- renderPlot({
    if (is.null(pcv_gdem_selected())) return(NULL)
    
    minx <- pcv_gdem_selected()[which.min(pcv_gdem_selected()$longitude), ]
    maxx <- pcv_gdem_selected()[which.max(pcv_gdem_selected()$longitude), ]
    flipx <- minx$range_index > maxx$range_index
    
    miny <- pcv_gdem_selected()[which.min(pcv_gdem_selected()$latitude), ]
    maxy <- pcv_gdem_selected()[which.max(pcv_gdem_selected()$latitude), ]
    flipy <- miny$azimuth_index > maxy$azimuth_index
    
    
    out <- pcv_gdem_selected() %>% 
      ggplot() + 
      geom_raster(aes(x = range_index, y = azimuth_index, 
                      alpha = alpha), fill = classpal(1)) + 
      scale_alpha_identity() + 
      theme(legend.position = "none")
    
    if (flipx) out <- out + scale_x_reverse()
    if (flipy) out <- out + scale_y_reverse()
    out
  })
  
  
  ### MODAL DIALOGS -------------------------------------------------------
  
  observeEvent(input$topleft_modal, {
    showModal(modalDialog(
      title = "Reach Error Plot",
      includeMarkdown("markdown/reacherr-modal.md"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$topright_modal, {
    showModal(modalDialog(
      title = "Node Error Accumulation Plot",
      withMathJax(includeMarkdown("markdown/nodeaccum-modal.md")),
      easyClose = TRUE,
      footer = NULL
      
    ))
  })
  
  observeEvent(input$bottomright_modal, {
    if (input$varselect == "wse") {
      showModal(modalDialog(
        title = "Pixel Height Plot",
        includeMarkdown("markdown/pixheight-modal.md"),
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Pixel Area Plot",
        includeMarkdown("markdown/pixarea-modal.md"),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
}


