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


tseTab1Server <- function(id, theMatrix, dataset, SelectedRVs) {
  moduleServer(id, function(input, output, session) {
    theOutliers <- reactiveVal()
    theFig <- reactiveVal()
    # Visualization: Box Plot   ------------------------------------
    observe({
      req(dataset)
      req(dataset$colData)
      req(theMatrix)
      req(SelectedRVs)
      req(SelectedRVs$target)
      req(SelectedRVs$geneList)
      req(SelectedRVs$target())
      req(SelectedRVs$geneList())
      print("Drawing Box Plot!")
      res <-
        tse_box_plot(
          theMatrix(),
          dataset$colData(),
          SelectedRVs$target(),
          SelectedRVs$geneList()
        )
      theOutliers(res[[2]])
      theFig(res[[1]])
    })
    
    observe({
      req(theFig)
      req(SelectedRVs)
      req(SelectedRVs$target)
      output$plot1 <- renderPlot({
        theFig()
      })
    })
    
    
    cellImageServer("cellImage", dataset, theOutliers)
    
  })
}