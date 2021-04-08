# Module for the first tab in Time Series Explorer (tse) Component

# UI function ------------------------------------------------

tseTab1UI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Fig 1",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      plotOutput(ns("plot1"), width = "100%", height = "600px")
    ),
    br(),
    cellImageUI(ns("cellImage"))
  )
}

# Server function ---------------------------------------------

tseTab1Server <- function(id, DatasetRVs, SelectedRVs) {
  moduleServer(id, function(input, output, session) {
    theOutliers <- reactiveVal()
    theFig <- reactiveVal()
    
    # Visualization: Box Plot   ------------------------------------
    
    observe({
      req(DatasetRVs$normData)
      req(DatasetRVs$colData)
      req(SelectedRVs$target)
      req(SelectedRVs$geneList)
      
      res <-
        tse_box_plot(
          DatasetRVs$normData,
          DatasetRVs$colData,
          SelectedRVs$target,
          SelectedRVs$geneList
        )
      
      theOutliers(res[[2]])
      theFig(res[[1]])
    })
    
    output$plot1 <- renderPlot({
      req(theFig())
      
      theFig()
    })
    
    # Visualization: Cell Images  ------------------------------------
    
    cellImageServer("cellImage", DatasetRVs, theOutliers)
    
  })
}