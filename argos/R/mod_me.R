# Module for  Matrix Explorer (me) Component
# It is named as Data Exploration in the UI
# (=_=) Naming is really hard...

# UI function ----------------

matrixExplorerUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "data_exp",
          
          h2(textOutput(ns('title'))),
          
          meSubsetUI(ns("subset")),
          
          meNormUI(ns("norm")),
          
          fluidRow(
            box(
              width = 12,
              DT::dataTableOutput(ns("contents")),
              style = "overflow-y: scroll; overflow-x: scroll;"
            )
            
          ))
  
}

# Server function ----------------

matrixExplorerServer <- function(id, DatasetRVs, GeneListRV) {
  moduleServer(id, function(input, output, session) {
    CornerStoneGeneListRV <-
      meSubsetServer("subset", DatasetRVs, GeneListRV)
    
    observe({
      req(CornerStoneGeneListRV())
      print("CornerStoneGeneListRV")
      print(CornerStoneGeneListRV())
    })
    
    NormResultRVs <-
      meNormServer("norm", DatasetRVs, CornerStoneGeneListRV)
    
    output$title <- renderText({
      req(NormResultRVs$theTitle)
      
      NormResultRVs$theTitle
    })
    
    # Render Matrix Data Table ------------
    
    output$contents <- DT::renderDataTable({
      req(NormResultRVs$theMatrix)
      
      datatable(NormResultRVs$theMatrix) %>%
        formatRound(columns = colnames(NormResultRVs$theMatrix),
                    digits = 2)
    })
  })
}
