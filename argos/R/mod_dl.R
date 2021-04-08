# Module for Data Loader (dl) Component

# UI function ----------------

dataLoaderUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "data_upload",
          
          tabsetPanel(
            tabPanel("Select dataset",
                     dlSelectDatasetUI(ns("selectData"))),
            tabPanel("Dimension Reduction",
                     dlDimensionReductionUI(ns(
                       "dimensionReduction"
                     )))
          ))
}

# Server function ----------------
# Return: list(rawData, normData, colData)
dataLoaderSever <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    dataset <- dlSelectDatasetServer("selectData")
    
    dlDimensionReductionServer("dimensionReduction", dataset)

    # Output ----------------

    dataset
  })
}
