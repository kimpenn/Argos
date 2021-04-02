dlDimensionReductionUI <- function(id) {
  ns <- NS(id)
  fluidPage(wellPanel(fluidRow(
    column(
      6,
      selectInput(
        ns("select_dr"),
        "Select the Dimension Reduction Method",
        choices = c("PCA", "UMAP"),
        multiple = FALSE
      )
    ),    
    column(width = 6,
                 h4("Points near click"),
                 verbatimTextOutput(ns("click_info"))
    )
  )),
  fluidRow(
    plotOutput(
      ns("DRplot"),
      width = "100%",
      height = "800px",
      click = ns("dr_click")
    )
  ))
}

dlDimensionReductionServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    thePoltRV <- reactiveVal()
    thePlotDataRV <- reactiveVal()
    
    observe({
      req(dataset)
      req(dataset$colData)
      req(dataset$colData())
      req(dataset$rawData)
      req(dataset$rawData())
      
      if (input$select_dr == "PCA") {
        res <- pca_plot(dataset$rawData(), dataset$colData())
        thePoltRV(res$plot)
        thePlotDataRV(res$data)
        
      } else if (input$select_dr == "UMAP") {
        res <- umap_plot(dataset$rawData(), dataset$colData())
        thePoltRV(res$plot)
        thePlotDataRV(res$data)
      }
    })
    
    observe({
      req(thePoltRV)
      req(thePoltRV())
      output$DRplot <- renderPlot({
        thePoltRV()
      })
    })
    
    observe({
      req(thePlotDataRV)
      req(thePlotDataRV())
      
      output$click_info <- renderPrint({
        nearPoints(
          thePlotDataRV(),
          input$dr_click,
          xvar = "X",
          yvar = "Y",
          addDist = FALSE
        )$sample
      })
    })
  })
}