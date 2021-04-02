# Module for Gene Selection in Time Series Explorer (tse) Component 

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
    )
  ))
}


tseGeneSelectServer <- function(id, dataset, geneList) {
  moduleServer(
    id,
    function(input, output, session) {
      # Define Output ------------------
      SelectedRVs <- reactiveValues(
        geneList = NULL,
        target = NULL
      )
      
      # Select Genes ------------------
      observe({
        req(dataset)
        req(dataset$rawData)
        
        updateSelectizeInput(
          session,
          "select_symbols",
          choices = dataset$geneUniverse(),
          server = TRUE
        )
      })
      # Select Pre-defined Genes List ------------------
      observe({
        req(geneList)
        updateSelectInput(session,
                          "select_gene_list",
                          choices = names(geneList()))
      })
      # Select the Target Gene
      observeEvent(input$select_symbols, {
        req(input$select_symbols)
        updateSelectInput(session, "select_target",
                          choices = input$select_symbols)
      })
      
      # Load Pre-defined Genes  ------------------
      observeEvent(input$btn_add_to_selection, {
        req(geneList)
        req(input$select_gene_list)
        shinyjs::disable("btn_add_to_selection")
        
        new_selected <- c(input$select_symbols,
                          unlist(geneList()[[input$select_gene_list]]))
        
        updateSelectizeInput(
          session,
          "select_symbols",
          choices = dataset$geneUniverse(),
          selected = new_selected,
          server = TRUE
        )
        
        shinyjs::enable("btn_add_to_selection")
      })
      
      # Tiger Analysis   ------------------
      observe({
        req(input$select_gene_list)
        req(input$select_target)
        SelectedRVs$geneList <- reactive(input$select_symbols)
        SelectedRVs$target <- reactive(input$select_target)
      })
      
      # Output  ------------------
      SelectedRVs
    }
  )
}