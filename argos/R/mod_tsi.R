# The module of time series Inference (tsi)

# UI function ------------------------------------------------

timeSeriesInferenceUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "time_series_inf",
    h2("Time Series Inference Visualization"),
    wellPanel(
      fluidRow(
        column(6, 
               tsiGeneSelectUI(ns("tsiGeneSelect")),
               verbatimTextOutput(ns("verbNumOfNonZeros"))), 
        column(6, 
               tsiGeneListSelectUI(ns("tsiGeneListSelect")),
               verbatimTextOutput(ns("verbIsVariated"))),
      )
    ),
    fluidRow(
      column(6,
        align = "center",
        tabsetPanel(
          tabPanel("Local regression (LOESS)", tsiTab1UI(ns("tab1"))),
          tabPanel("Generial Additive Model (GAM)", tsiTab2UI(ns("tab2")))
        )
      ),
      column(6,
             tabsetPanel(tabPanel("Box Plot", 
                                  plotOutput(ns("boxPlot"), height = "500px")))
      )
    )
  )
}

# Server function ---------------------------------------------
timeSeriesInferenceServer <- function(id, InferenceRVs, GeneListRV) {
  moduleServer(id, function(input, output, session) {
    # Generate Smoothed GAM Plot ------------------------------------

    GeneListSelectedRV <- tsiGeneListSelectServer(
      "tsiGeneListSelect",
      InferenceRVs, GeneListRV
    )

    GeneSelectedRV <- tsiGeneSelectServer(
      "tsiGeneSelect",
      InferenceRVs, GeneListRV,
      GeneListSelectedRV
    )

    output$verbNumOfNonZeros <- renderText({
      req(InferenceRVs$gamSce)
      req(GeneSelectedRV())
      tsi_count_non_zeros(InferenceRVs$gamSce, GeneSelectedRV())
    })

    output$verbIsVariated <- renderText({
      req(InferenceRVs$variatedGenes)
      req(GeneSelectedRV())
      tsi_is_variated(InferenceRVs$variatedGenes, GeneSelectedRV())
    })
    tsiTab1Server("tab1", InferenceRVs, GeneSelectedRV)
    tsiTab2Server("tab2", InferenceRVs, GeneSelectedRV)
    
    output$boxPlot <- renderPlot({
      req(InferenceRVs$gamSce)
      req(GeneSelectedRV())
      tsi_box_plot(InferenceRVs$gamSce, GeneSelectedRV())
    })
  })
}

# Time Series Inference Server for loading the pre-calculated sce object
timeSeriesInferenceLoaderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    datasetRVs <- reactiveValues(
      gamSce = NULL,
      variatedGenes = NULL
    )

    objPath <- "./dataset/ts-inference/8-tradeseq-GAM-sce.rds"
    datasetRVs$gamSce <- readRDS(objPath)

    geneListPath <- "./dataset/ts-inference/8-yhatSmooth.csv"
    datasetRVs$variatedGenes <- tsi_get_variated_genes(geneListPath)

    datasetRVs
  })
}
