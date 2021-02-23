# UI function ----------------
matrixExplorerUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "data_exp",
    h2(textOutput(ns('title'))),
    
    wellPanel(fluidRow(
      column(
        6,
        selectizeInput(
          ns("select_symbols"),
          "Subset the table by the selected Symbols" ,
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
      
      
    )),
    
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
    )),
    
    fluidRow(
      box(
        width = 12,
        # height = 1024,
        DT::dataTableOutput(ns("contents")),
        style = "overflow-y: scroll; overflow-x: scroll;"
      )
      
    )
  )
  
}

matrixExplorerServer <- function(id, dataset, geneList) {
  moduleServer(id, function(input, output, session) {
    isNormalized <- reactiveVal(FALSE)
    isRelatived <- reactiveVal(FALSE)
    currentGeneList <- reactiveVal()
    theMatrix <- reactiveVal()
    ###################################
    # Build up Current Gene List
    ##################################
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
    
    # Load Pre-defined Genes  ------------------
    observeEvent(input$btn_add_to_selection, {
      # req(input$select_symbols)
      req(geneList)
      req(input$select_gene_list)
      print("geneList()")
      print(unlist(geneList()[[input$select_gene_list]]))
      
      print("input$select_symbols:")
      print(input$select_symbols)
      
      new_selected <- c(input$select_symbols,
                        unlist(geneList()[[input$select_gene_list]]))
      print("new_selected:")
      print(new_selected)
      cat("is.reactive(new_selected):",
          is.reactive(new_selected),
          "\n")
      
      updateSelectizeInput(
        session,
        "select_symbols",
        choices = dataset$geneUniverse(),
        selected = new_selected,
        server = TRUE
      )
    })
    
    ###################################
    # Matrix: Subset
    ##################################
    observeEvent(input$btn_subset, {
      req(theMatrix)
      req(input$select_symbols)
      cur_matrix <- theMatrix()
      theMatrix(cur_matrix[input$select_symbols,])
    })
    
    ###################################
    # Matrix: Raw/Normalized
    ##################################
    observeEvent(input$btn_norm, {
      req(theMatrix)
      req(isNormalized)
      if (isNormalized()) {
        theMatrix(dataset$rawData())
        isNormalized(FALSE)
      } else{
        theMatrix(dataset$normData())
        isNormalized(TRUE)
      }
    })
    
    ###################################
    # Matrix: Visualization
    ##################################
    observeEvent(dataset$rawData, {
      req(dataset$rawData)
      theMatrix(dataset$rawData())
    })
    
    observe({
      req(isRelatived)
      req(isNormalized)
      if (isNormalized())
        str <- "Normalized Counts"
      else
        str <- "Raw Counts"
      
      if (isRelatived())
        str <- paste0("Relative ", str)
      
      output$title <- renderText({
        str
      })
    })
    
    observe({
      req(theMatrix)
      output$contents <- DT::renderDataTable({
        datatable(theMatrix()) %>%
          formatRound(columns = colnames(theMatrix()),
                      digits = 2)
      })
    })
    
    
    ###################################
    # Reset
    ##################################
    # Reset Subset
    observeEvent(input$btn_reset_subset, {
      req(theMatrix)
      req(isNormalized)
      updateSelectizeInput(
        session,
        "select_symbols",
        choices = dataset$geneUniverse(),
        selected = character(0),
        server = TRUE
      )
      
      if (!isNormalized()) {
        theMatrix(dataset$rawData())
      } else{
        theMatrix(dataset$normData())
      }
    })
    
    # Reset Calculation
    observeEvent(input$btn_reset_relative, {
      req(input$select_symbols)
      req(theMatrix)
      req(isNormalized)
      
      if (!isNormalized()) {
        theMatrix(dataset$rawData()[input$select_symbols,])
      } else{
        theMatrix(dataset$normData()[input$select_symbols,])
      }
      isRelatived(FALSE)
    })
    
    ###################################
    # Matrix: Relative Amt
    ##################################
    observeEvent(input$btn_relative, {
      if (dim(theMatrix())[1] < 100) {
        theMatrix(cal_percent(theMatrix()))
        isRelatived(TRUE)
      }
    })
    
    ###################################
    # Debug
    ##################################
    observe({
      req(theMatrix)
      cat("dim(theMatrix()):", dim(theMatrix()), "\n")
      
    })
    
    observe({
      print("-------------------------------")
      print("In matrixExplorerServer:")
      cat("name of the dataset: ", names(dataset), "\n")
      req(dataset$colData)
      req(dataset$geneUniverse)
      req(dataset$normData)
      req(dataset$rawData)
      cat("dim(dataset$colData):", dim(dataset$colData()), "\n")
      cat("dim(dataset$geneUniverse):",
          dim(dataset$geneUniverse()),
          "\n")
      cat("dim(dataset$normData):", dim(dataset$normData()), "\n")
      cat("dim(dataset$rawData):", dim(dataset$rawData()), "\n")
      print("-------------------------------")
      
    })
    
    
  })
}
