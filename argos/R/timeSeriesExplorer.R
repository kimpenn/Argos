# UI function ----------------
timeSeriesExplorerUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "time_series",
    h2('Time Series Inspection on Genes'),
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
    )),
    
    
    fluidRow(
      box(
        title = "Fig 1",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        plotOutput(ns("plot1"), width = "100%", height = "600px")
      )
    ),
    
    fluidRow(
      box(
        title = "Fig 2",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        plotOutput(ns("plot2"), width = "100%", height = "600px")
      )
    ),
    br(),
    cell_image_ui(ns)
    
  )
  
}

timeSeriesExplorerServer <- function(id, dataset, geneList) {
  moduleServer(id, function(input, output, session) {
    currentGeneList <- reactiveVal()
    theMatrix <- reactiveVal()
    thePlotMatrix <- reactiveVal()
    theOutliers <- reactiveVal()
    ###################################
    # Matrix
    ##################################
    observeEvent(dataset$normData, {
      req(dataset$normData)
      theMatrix(dataset$normData())
    })
    
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
      new_selected <- c(input$select_symbols,
                        unlist(geneList()[[input$select_gene_list]]))
      cat("new_selected: ", new_selected, "\n")
      updateSelectizeInput(
        session,
        "select_symbols",
        choices = dataset$geneUniverse(),
        selected = new_selected,
        server = TRUE
      )
    })
    
    ###################################
    # Visualization: Fig 1
    ##################################
    theFig1 <- eventReactive(input$select_target, {
      req(dataset)
      req(dataset$colData)
      req(theMatrix)
      req(input$select_target)
      req(input$select_symbols)
      print("In reactive!")
      res <-
        plot_fig_1(theMatrix(),
                   dataset$colData(),
                   input$select_target,
                   input$select_symbols)
      theOutliers(res[[2]])
      res[[1]]
    })
    
    output$plot1 <- renderPlot({
      theFig1()
    })
    
    ###################################
    # Visualization: Fig 2
    ##################################
    output$plot2 <- renderPlot({
      req(dataset)
      req(dataset$colData)
      req(theMatrix)
      req(input$select_target)
      req(input$select_symbols)
      
      plot_fig_2(theMatrix(),
                 dataset$colData(),
                 input$select_target,
                 input$select_symbols)
    })
    
    # ###################################
    # # Cell Images: Outliers
    # ##################################
    observe({
      req(theOutliers)
      updateSelectInput(session, "select_outliers",
                        choices = theOutliers())
    })
    
    observeEvent(input$select_outliers, {
      req(theOutliers)
      req(input$select_outliers)
      
      file_name <-
        paste0(
          tolower(strsplit(input$select_outliers, ".", fixed = TRUE)[[1]][2]),
          "-",
          strsplit(input$select_outliers, ".", fixed = TRUE)[[1]][3],
          "-",
          c("before", "after"),
          ".jpg"
        )
      cat("outlier file name: \n", file_name, "\n")
      output$outlier_img_before <- renderUI({
        tags$img(src = file_name[1], width = "100%")
      })
      output$outlier_img_after <- renderUI({
        tags$img(src = file_name[2], width = "100%")
      })
    })
    
    ###################################
    # Cell Images: Any Images
    ##################################
    observe({
      req(input$select_cell_image_group)
      req(dataset$colData)
      updateSelectInput(
        session,
        "select_cell_image_id",
        choices = get_sample_list(input$select_cell_image_group,
                                  dataset$colData())
      )
    })
    
    observeEvent(input$select_cell_image_id, {
      req(input$select_cell_image_id)
      
      file_name <-
        paste0(
          tolower(
            strsplit(input$select_cell_image_id, ".", fixed = TRUE)[[1]][2]
          ),
          "-",
          strsplit(input$select_cell_image_id, ".", fixed = TRUE)[[1]][3],
          "-",
          c("before", "after"),
          ".jpg"
        )
      cat("outlier file name: \n", file_name, "\n")
      output$any_img_before <- renderUI({
        tags$img(src = file_name[1], width = "100%")
      })
      output$any_img_after <- renderUI({
        tags$img(src = file_name[2], width = "100%")
      })
    })
    
    
  })
}