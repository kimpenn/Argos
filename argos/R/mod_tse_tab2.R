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

tseTab2Server <- function(id, theMatrix, dataset, SelectedRVs) {
  moduleServer(id, function(input, output, session) {
    # Visualization: Line Chart ------------------------------------
    output$plot2 <- renderPlot({
      req(dataset)
      req(dataset$colData)
      req(SelectedRVs)
      req(SelectedRVs$target())
      req(SelectedRVs$geneList())
      
      tse_line_chart(
        theMatrix(),
        dataset$colData(),
        SelectedRVs$target(),
        SelectedRVs$geneList()
      )
    })
    
    # Render Data Table------------------------------------
    observe({
      req(dataset)
      req(dataset$colData)
      req(SelectedRVs)
      req(SelectedRVs$target)
      req(SelectedRVs$target())
      req(SelectedRVs$geneList)
      req(SelectedRVs$geneList())
      
      tibleListRV <- tse_overview_table(
        theMatrix(),
        dataset$colData(),
        SelectedRVs$target(),
        SelectedRVs$geneList()
      )
      
      output$groupC = render_table_handler(tibleListRV()[[1]])
      output$group1 = render_table_handler(tibleListRV()[[2]])
      output$group2 = render_table_handler(tibleListRV()[[3]])
      output$group3 = render_table_handler(tibleListRV()[[4]])
      output$group7 = render_table_handler(tibleListRV()[[5]])
    })
  })
  
  
}