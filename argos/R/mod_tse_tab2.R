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
    # Visualization: Line Chart ------------------------------------
    output$plot2 <- renderPlot({
      req(DatasetRVs$normData)
      req(DatasetRVs$colData)
      req(SelectedRVs$target)
      req(SelectedRVs$geneList)
      
      tse_line_chart(
        DatasetRVs$normData,
        DatasetRVs$colData,
        SelectedRVs$target,
        SelectedRVs$geneList
      )
    })
    
    # Render Data Table------------------------------------
    
    tibleListRV <-
      eventReactive(
        c(
          DatasetRVs$normData,
          DatasetRVs$colData,
          SelectedRVs$target,
          SelectedRVs$geneList
        ),
        {
          req(DatasetRVs$normData)
          req(DatasetRVs$colData)
          req(SelectedRVs$target)
          req(SelectedRVs$geneList)
          
          tse_overview_table(
            DatasetRVs$normData,
            DatasetRVs$colData,
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