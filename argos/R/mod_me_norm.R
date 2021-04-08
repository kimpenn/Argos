# Module for triggering calculations for Matrix Explore（me) Component

# UI function ----------------

meNormUI <- function(id) {
  ns <- NS(id)
  wellPanel(fluidRow(
    actionButton(ns("btn_norm"),
                 "Raw/Normalized",
                 class = "btn-success"),
    actionButton(ns("btn_relative"),
                 "Relative Amts",
                 class = "btn-primary"),
    actionButton(ns("btn_reset_relative"),
                 "Reset Calculation",
                 class = "btn-warning"),
  ))
}

# Server function ----------------

meNormServer <- function(id, DatasetRVs, CornerStoneGeneListRV) {
  moduleServer(id, function(input, output, session) {
    NormResultRVs <- reactiveValues(theMatrix = NULL,
                                    theTitle = "Argos")
    
    isNormalized <- reactiveVal(FALSE)
    isRelatived <- reactiveVal(FALSE)
    
    # Initialize the matrix ----------
    
    observeEvent(DatasetRVs$rawData, {
      NormResultRVs$theMatrix = DatasetRVs$rawData
      isNormalized(FALSE)
    })
    
    # Subset/reset Matrix -------------
    
    observeEvent(CornerStoneGeneListRV(), {
      req(DatasetRVs$normData)
      
      if (length(CornerStoneGeneListRV()) == 0){
        # Reset
        if (isNormalized()) {
          NormResultRVs$theMatrix <- DatasetRVs$normData
        } else{
          NormResultRVs$theMatrix <- DatasetRVs$rawData
        }
      }else{
        # Subset
        cur_matrix <- NormResultRVs$theMatrix
        NormResultRVs$theMatrix <-
          cur_matrix[CornerStoneGeneListRV(),]
      }
    })
    
    # Trigger between Raw/Normalized Matrix -------
    
    observeEvent(input$btn_norm, {
      req(NormResultRVs$theMatrix)
      
      if (isNormalized()) {
        NormResultRVs$theMatrix <- DatasetRVs$rawData
      } else{
        NormResultRVs$theMatrix <- DatasetRVs$normData
      }
      
      isNormalized(!isNormalized())
    })
    
    # Trigger Calculating Relative Amt -------
    
    observeEvent(input$btn_relative, {
      req(NormResultRVs$theMatrix)
      req(dim(NormResultRVs$theMatrix)[1] < 100)
      
      NormResultRVs$theMatrix <-
        cal_percent(NormResultRVs$theMatrix)
      isRelatived(TRUE)
      
    })
    
    # Reset Calculation of Relative Amt -------
    
    observeEvent(input$btn_reset_relative, {
      req(NormResultRVs$theMatrix)
      req(CornerStoneGeneListRV())
      
      if (!isNormalized()) {
        NormResultRVs$theMatrix <-
          DatasetRVs$rawData[CornerStoneGeneListRV(), ]
      } else{
        NormResultRVs$theMatrix <-
          DatasetRVs$normData[CornerStoneGeneListRV(), ]
      }
      isRelatived(FALSE)
    })
    
    # Visualization: Page Title -------
    
    observeEvent(isNormalized(), {
      NormResultRVs$theTitle <- get_title(isNormalized(), isRelatived())
    })
    
    observeEvent(isRelatived(), {
      NormResultRVs$theTitle <- get_title(isNormalized(), isRelatived())
    })
    
    NormResultRVs
  })
}