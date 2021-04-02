# The module of time series explorer (tse)

# UI function ------------------------------------------------

timeSeriesExplorerUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "time_series",
    h2('Time Series Inspection on Genes'),
    
    tseGeneSelectUI(ns("geneSelectUI")),
    tabsetPanel(
      tabPanel("Box Plot", tseTab1UI(ns("tab1"))),
      tabPanel("Line chart", tseTab2UI(ns("tab2")))
    )
    
  )
  
}

# Server function ---------------------------------------------

timeSeriesExplorerServer <- function(id, dataset, geneList) {
  moduleServer(id, function(input, output, session) {
    theMatrix <- reactiveVal()
    # Observer: Setup Data ------------------------------------
    
    observeEvent(dataset$normData, {
      req(dataset$normData)
      theMatrix(dataset$normData())
    })
    
    # Module: Build Gene List ------------------------------------
    
    SelectedRVs <-
      tseGeneSelectServer("geneSelectUI", dataset, geneList)
    
    # observe({
    #   req(SelectedRVs)
    #   req(SelectedRVs$target)
    #   req(SelectedRVs$geneList)
    #   print("--- In mod_tse ---")
    #   print("selectedRVs$target:")
    #   print(SelectedRVs$target())
    #   print("selectedRVs$geneList:")
    #   print(SelectedRVs$geneList())
    # })
    
    # Module: Tab with Box Plot  ------------------------------------
    tseTab1Server("tab1", theMatrix, dataset, SelectedRVs)
    # Module: Tab with Line Chart  ------------------------------------
    tseTab2Server("tab2", theMatrix, dataset, SelectedRVs)
  })
}