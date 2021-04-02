server <- function(input, output, session) {
  ####################################
  # Security
  ####################################
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  ####################################
  # Server: Data Loader
  ####################################
  globals <- reactiveValues(the_table = NULL, ori_data = NULL)
  argosDataSet <- dataLoaderSever("dataLoader")
  
  observe({
    cat("[In Server.R]", argosDataSet$title(), "\n")
    output$projectTitle <- renderText({argosDataSet$title()})
  })
  
  # For debug -----
  # observe({
  #   req(argosDataSet)
  #   req(argosDataSet$colData)
  # })
  # is.reactive(argosDataSet) FALSE
  # is.reactive(argosDataSet$geneUniverse) TRUE
  
  ####################################
  # Server: Gene List Manager
  ####################################
  argosGeneList <- geneListManagerServer("geneListManager",
                                         argosDataSet)
  
  ####################################
  # Server: Tab 1 Data Exploration
  ####################################
  matrixExplorerServer("matrixExplorer",
                       argosDataSet, argosGeneList)
  
  # ####################################
  # # Server: Tab 2 Time Series Inspection
  # ####################################
  timeSeriesExplorerServer("timeSeriesExplorer",
                           argosDataSet, argosGeneList)
  
}