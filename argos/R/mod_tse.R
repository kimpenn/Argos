# The module of time series explorer (tse)

# UI function ------------------------------------------------

timeSeriesExplorerUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "time_series",
    h2('Time Series Inspection on Genes'),
    
    tseGeneSelectUI(ns("geneSelect")),
    tabsetPanel(
      tabPanel("Box Plot", tseTab1UI(ns("tab1"))),
      tabPanel("Line chart", tseTab2UI(ns("tab2")))
    )
    
  )
  
}

# Server function ---------------------------------------------

timeSeriesExplorerServer <- function(id, DatasetRVs, GeneListRV) {
  moduleServer(id, function(input, output, session) {
    # Module: Build Gene List ------------------------------------
    
    CornerStoneGeneListRV <-
      tseGeneSelectServer("geneSelect", DatasetRVs, GeneListRV)
    
    # Module: Tab with Box Plot  ------------------------------------
    
    tseTab1Server("tab1", DatasetRVs, CornerStoneGeneListRV)
    
    # Module: Tab with Line Chart  ------------------------------------
    
    tseTab2Server("tab2", DatasetRVs, CornerStoneGeneListRV)
  })
}