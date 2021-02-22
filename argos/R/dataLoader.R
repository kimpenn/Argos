# Helper function ----------------

# UI function ----------------

dataLoaderUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "data_upload",
    
    h2("Upload Dataset"),
    
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
}

# Server function ----------------
# Return: list(rawData, normData, colData)
dataLoaderSever <- function(id) {
  moduleServer(id, function(input, output, session) {
    roots = c(wd = '.')
    shinyDirChoose(input,
                   "folder",
                   roots = roots,
                   filetypes = c("", "txt"))
    
    datasetPath <- reactive({
      parseDirPath(roots, input$folder)
    })
    
    observe({
      cat("datasetPath: ", datasetPath(), "\n")
    })
    
    observe({
      req(length(input$folder) <= 1)
      output$dataset_path_log <- renderText({
        INFO_STR
      })
    })
    
    observe({
      req(length(input$folder) > 1)
      print("Updating file path log...")
      output$dataset_path_log <- renderText({
        paste0("Selected Path of the Dataset:\n",
               datasetPath())
      })
    })
    
    dataSet <- reactiveValues(rawData = NULL, normData = NULL, 
                              colData = NULL, geneUniverse = NULL)
    
    observeEvent(input$load_data_button, {
      req(datasetPath)
      print("Loading rawData...")
      str <- ""
      str <- paste0(str, "Loding count matrix...\n")
      dataSet$rawData <-
        reactive(load_dataset(file.path(datasetPath(), "counts.csv")))
      str <- paste0(str, "Loding Normalized count matrix...\n")
      dataSet$normData <-
        reactive(load_dataset(file.path(datasetPath(), "norm-counts.csv")))
      str <- paste0(str, "Loding Column Data...\n")
      dataSet$colData <-
        reactive(load_coldata(file.path(datasetPath(), "design_tbl.csv")))
      dataSet$geneUniverse <- reactive(dataSet$rawData()$Symbol)
      str <- paste0(str, "Done!\n")
      output$upload_log <- renderText({str})
    })
    
    dataSet
  })
}
