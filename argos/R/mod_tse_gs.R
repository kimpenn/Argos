# Module for Gene Selection in Time Series Explorer (tse) Component 

# UI function ------------------------------------------------

tseGeneSelectUI <- function(id) {
  ns <- NS(id)
  wellPanel(fluidRow(
    column(
      6,
      selectizeInput(
        ns("select_symbols"),
        "Set the target gene list" ,
        choices = character(0),
        multiple = TRUE
      ),
      selectInput(
        ns("select_target"),
        "Select the gene to inspect on",
        choices = character(0),
        multiple = FALSE
      )
    ),
    column(
      6,
      selectInput(
        ns("select_gene_list"),
        "↙️ Add a pre-defined gene list to the selection.",
        choices = character(0),
        multiple = FALSE
      ),
      
      actionButton(ns("btn_add_to_selection"),
                   "Add to Selection",
                   class = "btn-secondary")
      # ,
      # 
      # checkboxInput(ns("cb_good_samples"), "Good Samples Only", 
      #               value = FALSE, width = NULL)
      
    )
  ))
}

# Server function ---------------------------------------------

tseGeneSelectServer <- function(id, DatasetRVs, GeneListRV) {
  moduleServer(
    id,
    function(input, output, session) {
      # Define Output ------------------
      
      SelectedRVs <- reactiveValues(
        geneList = NULL,
        target = NULL,
        goodSamplesOnly = TRUE
      )
      
      # Select CornerStone Genes ------------------
      
      observe({
        req(DatasetRVs$geneUniverse)
        
        updateSelectizeInput(
          session,
          "select_symbols",
          choices = DatasetRVs$geneUniverse,
          server = TRUE
        )
      })
      
      # Select Pre-defined Genes List ------------------
      
      observe({
        req(GeneListRV())
        updateSelectInput(session,
                          "select_gene_list",
                          choices = names(GeneListRV()))
      })
      
      # Select Samples ------------------
      
      # observeEvent(input$cb_good_samples, {
      #   SelectedRVs$goodSamplesOnly <- input$cb_good_samples
      # })
      # 
      # Select the Target Gene ------------------
      
      observeEvent(input$select_symbols, {

        updateSelectInput(session, "select_target",
                          choices = input$select_symbols)
      })
      
      # Load Pre-defined Genes  ------------------
      
      observeEvent(input$btn_add_to_selection, {
        req(GeneListRV())
        req(input$select_gene_list)
        
        shinyjs::disable("btn_add_to_selection")

        new_selected <- c(input$select_symbols,
                          unlist(GeneListRV()[[input$select_gene_list]]))

        updateSelectizeInput(
          session,
          "select_symbols",
          choices = DatasetRVs$geneUniverse,
          selected = new_selected,
          server = TRUE
        )

        shinyjs::enable("btn_add_to_selection")
      })
      
      # Tiger Analysis   ------------------
      
      observe({
        SelectedRVs$geneList <- input$select_symbols
        SelectedRVs$target <- input$select_target
      })
      
      # Output  ------------------
      
      SelectedRVs
    }
  )
}