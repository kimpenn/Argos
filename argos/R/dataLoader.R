# Helper function ----------------

# UI function ----------------

dataLoaderUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "data_upload",
          
          tabsetPanel(
            tabPanel(
              "Upload Dataset",
              fluidPage(
                fluidRow(wellPanel(
                  shinyFiles::shinyDirButton(
                    ns("folder"),
                    "Select a folder",
                    "Please select a folder",
                    FALSE,
                    class = "btn-success"
                  )
                )),
                
                fluidRow(wellPanel(verbatimTextOutput(
                  ns("dataset_path_log")
                ))),
                
                fluidRow(wellPanel(
                  actionButton(ns("load_data_button"),
                               "Upload Data",
                               class = "btn-primary")
                )),
                
                fluidRow(wellPanel(verbatimTextOutput(ns(
                  "upload_log"
                ))))
              )
            ),
            tabPanel("Design Table", DTOutput(ns('designTable'))),
            tabPanel("Dimension Reduction", uiDimensionReduction(ns))
          ))
}

# Server function ----------------
# Return: list(rawData, normData, colData)
dataLoaderSever <- function(id) {
  moduleServer(id, function(input, output, session) {
    ###############################
    # Define Output
    ###############################
    dataSet <- reactiveValues(
      rawData = NULL,
      normData = NULL,
      colData = NULL,
      geneUniverse = NULL,
      title = NULL
    )
    
    ###############################
    # Select Dataset Folder
    ###############################
    roots <- c(wd = '.')
    
    shinyDirChoose(input,
                   "folder",
                   roots = roots,
                   filetypes = c("", "txt"))
    
    datasetPath <- reactive({
      parseDirPath(roots, input$folder)
    })
    
    observe({
      if (length(input$folder) <= 1) {
        dataSet$title <- reactive({
          "Argos"
        })
      } else{
        dataSet$title <-
          reactive({
            paste0("Argos-", input$folder[["path"]][[3]])
          })
      }
    })
    
    ###############################
    # Generate Reading Log
    ###############################
    # Default
    observe({
      req(length(input$folder) <= 1)
      output$dataset_path_log <- renderText({
        INFO_STR
      })
    })
    # Reading Log
    observe({
      req(length(input$folder) > 1)
      print("Updating file path log...")
      output$dataset_path_log <- renderText({
        paste0("Selected Path of the Dataset:\n",
               datasetPath())
      })
    })
    
    observeEvent(input$load_data_button, {
      req(datasetPath)
      print("Loading rawData...")
      str <- ""
      str <- paste0(str, "Loding count matrix...\n")
      dataSet$rawData <-
        reactive(load_dataset(file.path(datasetPath(), "count-matrix.csv")))
      
      str <- paste0(str, "Loding Normalized count matrix...\n")
      dataSet$normData <-
        reactive(load_dataset(file.path(
          datasetPath(), "count-matrix-norm.csv"
        )))
      
      str <- paste0(str, "Loding Column Data...\n")
      dataSet$colData <-
        reactive(load_coldata(file.path(datasetPath(), "design-table.csv")))
      
      str <-
        paste0(str, "Extracting Gene Symbols from the Data...\n")
      dataSet$geneUniverse <- reactive(rownames(dataSet$rawData()))
      
      str <- paste0(str, "Done!\n")
      output$upload_log <- renderText({
        str
      })
      
    })
    
    ###############################
    # Design Table
    ###############################
    observe({
      req(dataSet$colData)
      req(dataSet$colData())
      
      output$designTable <- renderDT(
        dataSet$colData() %>% dplyr::count(Group)
      )
    })
    
    ###############################
    # Dimension Reduction
    ###############################
    observe({
      req(dataSet$colData)
      req(dataSet$rawData)
      if (input$select_dr == "PCA") {
        output$DRplot <- renderPlot({
          pca_plot(dataSet$rawData(), dataSet$colData())
        })
      } else if (input$select_dr == "UMAP") {
        output$DRplot <- renderPlot({
          umap_plot(dataSet$rawData(), dataSet$colData())
        })
      }
      
      
    })
    
    ###############################
    # Output
    ###############################
    dataSet
  })
}
