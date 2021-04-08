# Module for Tab 1 Upload Dataset (ud) in Data Loader (dl) Component

# UI function ----------------

dlSelectDatasetUI <- function(id) {
  ns <- NS(id)
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
    )))),
    
    DTOutput(ns('designTable'))
  )
}

# Server function ----------------

dlSelectDatasetServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactiveValues(
      rawData = NULL,
      normData = NULL,
      colData = NULL,
      geneUniverse = NULL,
      title = "Argos"
    )
    
    # Select dataset Folder -----------
    
    roots <- c(wd = '.')
    
    shinyDirChoose(input,
                   "folder",
                   roots = roots,
                   filetypes = c("", "txt"))
    
    datasetPath <- eventReactive(input$folder, {
      parseDirPath(roots, input$folder)
    })
    
    observeEvent(datasetPath(), {
      the_name <- str_split(datasetPath(),
                            pattern = "/", simplify = TRUE)[1, 3]
      dataset$title <- paste0("Argos-", the_name)
    }, ignoreInit = TRUE)
    
    output$dataset_path_log <- renderText({
      paste0("Selected Path of the dataset:\n",
             datasetPath())
    })
    
    # Read Dataset -------------------
    
    uploadLogRV <- reactiveVal(INFO_STR)
    
    observeEvent(input$load_data_button, {
      req(datasetPath())
      
      print(paste0("Click: input$load_data_button"))
      str <- ""
      str <- paste0(str, "Loding count matrix...\n")
      dataset$rawData <-
        load_dataset(file.path(datasetPath(), "count-matrix.csv"))
      
      str <- paste0(str, "Loding Normalized count matrix...\n")
      dataset$normData <-
        load_dataset(file.path(datasetPath(), "count-matrix-norm.csv"))
      
      str <- paste0(str, "Loding Column Data...\n")
      dataset$colData <-
        load_coldata(file.path(datasetPath(), "design-table.csv"))
      
      str <-
        paste0(str, "Extracting Gene Symbols from the Data...\n")
      dataset$geneUniverse <- rownames(dataset$rawData)
      
      str <- paste0(str, "Done!\n")
      
      uploadLogRV(str)
    }, ignoreInit = TRUE)
    
    output$upload_log <- renderText(uploadLogRV())
    
    # Render Design Table -------------------
    
    output$designTable <-
      renderDT({
        req(dataset$colData)
        datatable(dataset$colData %>% dplyr::count(Group),
                  options = list(dom = 't'))
      })
    
    # Output -------------------
    
    dataset
  })
}