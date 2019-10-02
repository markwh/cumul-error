# Globals for shiny app

# library(shinyFiles)
library(shinythemes)
library(shinydashboard)
library(rivertile)
# devtools::load_all("~/Documents/rivertile")
library(rtvalidate)
# library(ncdf4)
library(fs)
library(ggplot2)
library(leaflet)
library(purrr)
library(tidyr)
library(plotly)
library(crosstalk)
library(dplyr) 


# na.rm version of cumsum
nacumsum <- function(x) {
  Reduce(function(y0, y1) sum(y0, y1, na.rm = TRUE), x = x, accumulate = TRUE)
}

theme_set(theme_bw())

# Colors
nodecolor_unsel <- "#668cff"
nodecolor_sel <- "#0039e6"

# Pixc(vec) color legend
classes <- c(1, 2, 3, 4, 22, 23, 24)
classlabs <- c("gdem water", "land near water", "water near land", 
               "open water", "land near dark water", 
               "dark water edge", "dark water")
classpal <- colorFactor(palette = "Set1", domain = classes)
classcolvec <- setNames(classpal(classes), classes)



# redo_reach <- function(reachdata, nodedata, weight = TRUE) {
#   # browser()
#   reachdf0 <- nodedata %>% 
#     reach_agg(weight = weight)
#   commonnames <- intersect(names(reachdata), names(reachdf0))
#   reachdata[commonnames] <- reachdf0[commonnames]
#   reachdata
# }

#' Function to remove nodes from an rtdata set--that is, a list of data.frames.
purge_nodes <- function(rtdata, purgenodes = numeric(0), 
                        redo_reaches = TRUE) {
  # browser()
  if (length(purgenodes) == 0) return(rtdata)
  reachinds <- grep("reach", names(rtdata))
  purgefun <- function(x) x[!(x[["node_id"]] %in% purgenodes), ]
  
  out <- rtdata
  out[-reachinds] <- lapply(rtdata[-reachinds], purgefun)
  if (redo_reaches) {
    out$rt_reaches <- redo_reach(rtdata$rt_reaches, out$rt_nodes)
    out$gdem_reaches <- redo_reach(rtdata$gdem_reaches, out$gdem_nodes, weight = FALSE)
  }
  out
}

