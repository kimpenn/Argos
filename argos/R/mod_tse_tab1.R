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
    
    SelectedDatasetRVs <- reactiveValues(
      normData = NULL,
      colData = NULL
    )
    
    observe({
      req(DatasetRVs$normData)
      req(DatasetRVs$colData)
      
      the_dataset <- preprocess_data(DatasetRVs$normData,
                                     DatasetRVs$colData,
                                     SelectedRVs$goodSamplesOnly)
      
      SelectedDatasetRVs$normData <- the_dataset$normData
      SelectedDatasetRVs$colData <- the_dataset$colData
    })
    
    # Visualization: Box Plot   ------------------------------------
    
    observe({
      req(SelectedDatasetRVs$normData)
      req(SelectedDatasetRVs$colData)
      req(SelectedRVs$target)
      req(SelectedRVs$geneList)
      
      res <-
        tse_box_plot(
          SelectedDatasetRVs$normData,
          SelectedDatasetRVs$colData,
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