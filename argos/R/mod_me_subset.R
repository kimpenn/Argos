# Module for subsetting the matrix for Matrix Explore（me) Component

# UI function ----------------

meSubsetUI <- function(id) {
  ns <- NS(id)
  wellPanel(fluidRow(
    column(
      6,
      selectizeInput(
        ns("select_symbols"),
        "Subset the table by the Corner Stone Symbols" ,
        choices = character(0),
        multiple = TRUE
      ),
      
      actionButton(ns("btn_subset"),
                   "Subset Table",
                   class = "btn-primary"),
      
      actionButton(ns("btn_reset_subset"),
                   "Reset Subset",
                   class = "btn-warning")
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
    )
    
    
  ))
}

# Server function ----------------

meSubsetServer <- function(id, DatasetRVs, GeneListRV) {
  moduleServer(id, function(input, output, session) {
    CornerStoneGeneList <- reactiveVal(c())
    
    # Select Genes ------------------
    
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
    
    # Load Pre-defined Genes ------------------
    
    observeEvent(input$btn_add_to_selection, {
      req(GeneListRV)
      req(input$select_gene_list)
      shinyjs::disable("btn_add_to_selection")
      
      new_selected <- c(input$select_symbols,
                        unlist(GeneListRV()[[input$select_gene_list]]))
      
      updateSelectizeInput(
        session,
        "select_symbols",
        selected = new_selected,
        choices = DatasetRVs$geneUniverse,
        server = TRUE
      )
      shinyjs::enable("btn_add_to_selection")
    })
    
    # Reset Subset ------------------
    
    observeEvent(input$btn_reset_subset, {
      updateSelectizeInput(session,
                           "select_symbols",
                           selected = character(0))
      print("I am here")
      CornerStoneGeneList(character(0))
      print(CornerStoneGeneList())
    })
    
    # return ----------------
    
    observeEvent(input$btn_subset,{
      req(input$select_symbols)
      CornerStoneGeneList(input$select_symbols)
    })
    
    CornerStoneGeneList
  })
}