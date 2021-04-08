# Argos' Main Server

server <- function(input, output, session) {
  # Security  ----------------
  
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # Data Loader ----------------
  
  argosDataSet <- dataLoaderSever("dataLoader")
  
  output$projectTitle <- renderText(argosDataSet$title)
  
  # Gene List Manager -----------------
  argosGeneList <- geneListManagerServer("geneListManager",
                                         argosDataSet)
  
  
  # Tab 1: Data Exploration  ----------------
  
  matrixExplorerServer("matrixExplorer",
                       argosDataSet, argosGeneList)
  
  
  # Tab 2 Time Series Inspection  ----------------
  timeSeriesExplorerServer("timeSeriesExplorer",
                           argosDataSet, argosGeneList)
  
}