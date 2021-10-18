# Module for the second tab in Time Series Explorer (tse) Component

# UI function ------------------------------------------------

tseTab2UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(
        title = "Fig 2",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        plotOutput(ns("plot2"), width = "100%", height = "600px")
      )
    ),
    
    fluidRow(
      overviewTableBox(ns("groupC")),
      overviewTableBox(ns("group1")),
      overviewTableBox(ns("group2"))
    ),
    
    fluidRow(overviewTableBox(ns("group3")),
             overviewTableBox(ns("group7")))
  )
}

# Server function ---------------------------------------------

tseTab2Server <- function(id, DatasetRVs, SelectedRVs) {
  moduleServer(id, function(input, output, session) {
    
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
    
    # Visualization: Line Chart ------------------------------------
    output$plot2 <- renderPlot({
      req(SelectedDatasetRVs$normData)
      req(SelectedDatasetRVs$colData)
      req(SelectedRVs$target)
      req(SelectedRVs$geneList)
      
      tse_line_chart(
        SelectedDatasetRVs$normData,
        SelectedDatasetRVs$colData,
        SelectedRVs$target,
        SelectedRVs$geneList
      )
    })
    
    # Render Data Table------------------------------------
    
    tibleListRV <-
      eventReactive(
        c(
          SelectedDatasetRVs$normData,
          SelectedDatasetRVs$colData,
          SelectedRVs$target,
          SelectedRVs$geneList
        ),
        {
          req(SelectedDatasetRVs$normData)
          req(SelectedDatasetRVs$colData)
          req(SelectedRVs$target)
          req(SelectedRVs$geneList)
          
          tse_overview_table(
            SelectedDatasetRVs$normData,
            SelectedDatasetRVs$colData,
            SelectedRVs$target,
            SelectedRVs$geneList
          )
        }
      )
    
    observeEvent(tibleListRV(), {
      output$groupC = renderTableHandler(tibleListRV()[[1]])
      output$group1 = renderTableHandler(tibleListRV()[[2]])
      output$group2 = renderTableHandler(tibleListRV()[[3]])
      output$group3 = renderTableHandler(tibleListRV()[[4]])
      output$group7 = renderTableHandler(tibleListRV()[[5]])
    })
    
    
  })
  
  
}