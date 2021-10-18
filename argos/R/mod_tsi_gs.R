# Module for Gene Selection in Time Series Inference (tsi) Component

# Time Series Inference Gene Select UI ----------
tsiGeneSelectUI <- function(id) {
  ns <- NS(id)
  selectizeInput(
    ns("select_symbol"),
    "Set the target gene from the Gene List",
    choices = character(0),
    multiple = FALSE
  )
}

# Time Series Inference Gene List Select UI ----------
tsiGeneListSelectUI <- function(id) {
  ns <- NS(id)
  selectizeInput(
    ns("select_list"),
    "Select Gene List",
    choices = character(0),
    multiple = FALSE
  )
}

# Time Series Inference Gene Select Server ----------
tsiGeneSelectServer <- function(id, InferenceRVs, GeneListRV, SelectedListRV) {
  moduleServer(
    id,
    function(input, output, session) {
      selectedGeneRV <- reactiveVal()

      observe({
        req(InferenceRVs$gamSce)
        req(GeneListRV())
        req(SelectedListRV())
        if (SelectedListRV() == "All Genes"){
          the_choices <- rownames(
            SummarizedExperiment::assays(InferenceRVs$gamSce)$counts
          )
        }
        else{
          the_choices <- GeneListRV()[SelectedListRV()]
        }
        updateSelectizeInput(
          session,
          "select_symbol",
          choices = the_choices,
          selected = "Creb1",
          server = TRUE
        )
      })

      observe({
        selectedGeneRV(input$select_symbol)
      })

      selectedGeneRV
    }
  )
}

# Time Series Inference Gene List Select Server ----------
tsiGeneListSelectServer <- function(id, InferenceRVs, GeneListRV) {
  moduleServer(
    id,
    function(input, output, session) {
      selectedListRV <- reactiveVal()
      
      observe({
        req(InferenceRVs$gamSce)
        updateSelectizeInput(
          session,
          "select_list",
          choices = c("All Genes", names(GeneListRV())),
          selected = "All Genes",
          server = TRUE
        )
      })
      
      observe({
        selectedListRV(input$select_list)
      })
      
      selectedListRV
    }
  )
}