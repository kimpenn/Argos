# Helper function ----------------

# UI function ----------------
# Upload CSV file as gene list dataframe
# Customize gene list
# Download current geen list dataframe

geneListManagerUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "gene_list_manager",
    
    h2("Manage Gene List for Data Wrangling"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Upload Gene List"),
        fileInput(
          ns("user_gene_list_csv"),
          "Choose CSV File",
          multiple = FALSE,
          accept = c("text",
                     "json")
        ),
        tags$hr(),
        
        h3("Add New Gene List"),
        textInput(ns("new_gene_list_name"),
                  "The New Gene List Name:"),
        selectizeInput(
          ns("new_gene_list"),
          "Select the genes in the list",
          choices = NULL,
          selected = FALSE,
          multiple = TRUE
        ),
        
        actionButton(ns("btn_add_gene_list"),
                     "Add Gene List",
                     class = "btn-primary"),
        tags$hr(),
        
        h3("Remove Gene List"),
        
        selectizeInput(
          ns("remove_gene_list"),
          "Select the list to remove",
          choices = NULL,
          selected = FALSE,
          multiple = FALSE
        ),
        
        actionButton(ns("btn_remove_gene_list"),
                     "Remove Gene List",
                     class = "btn-warning"),
        tags$hr(),
        
        h3("Download Gene Lists"),
        downloadButton(ns("btn_download_gene_list"),
                       "Download Gene List",
                       class = "btn-success")
      ),
      mainPanel(wellPanel(
        h3("Preview the Gene Lists"),
        reactjsonOutput(ns("rjed"), height="650px")
      ))
    )
  )
}

# Server function ----------------
# Return: geneListDataframe
geneListManagerServer <- function(id, geneUniverse) {
  stopifnot(is.reactive(geneUniverse))
  
  moduleServer(id, function(input, output, session) {
    geneList <- reactiveVal(list())
    #####################################
    # Upload Gene List CSV File
    #####################################
    observeEvent(input$user_gene_list_csv, {
      geneList(fromJSON(
        file = input$user_gene_list_csv$datapath,
        simplify = FALSE
      ))
    })
    
    #####################################
    # Add New Gene List
    #####################################
    observeEvent(input$btn_add_gene_list, {
      the_list <- geneList()
      the_list[[input$new_gene_list_name]] <- as.list(input$new_gene_list)
      geneList(the_list)
    })
    
    observeEvent(geneUniverse(), {
      updateSelectizeInput(session,
                           "new_gene_list",
                           choices = geneUniverse(),
                           server = TRUE)
    })
    
    #####################################
    # Remove New Gene List
    #####################################
    observeEvent(geneList(), {
      updateSelectizeInput(
        session,
        "remove_gene_list",
        # The first list cannot be removed
        choices = names(geneList())[-1],
        server = TRUE
      )
    })
    observeEvent(input$btn_remove_gene_list, {
      req(input$remove_gene_list)
      the_list <- geneList()
      the_list[[input$remove_gene_list]] <- NULL
      geneList(the_list)
    })
    #####################################
    # Download the a JSON file
    #####################################
    output$btn_download_gene_list <- downloadHandler(
      filename = function() {
        paste("geneList", ".json", sep = "")
      },
      content = function(file) {
        print(geneList())
        cat(toJSON(geneList(), indent = 4),file=file,sep="\n")
      }
    )
    #####################################
    # Render a JSON VIEWER
    #####################################
    output$rjed <- renderReactjson({
      reactjson(
        geneList(),
        name = "GeneListSets",
        displayDataTypes = FALSE,
        onEdit = FALSE,
        onAdd = FALSE,
        onDelete = FALSE,
        onSelect = FALSE
      )
    })
    
    observeEvent(input$rjed_edit, {
      str(input$rjed_edit, max.level = 2)
    })
    
    #####################################
    # return
    #####################################
    geneList
  })
}