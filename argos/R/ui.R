ui <- dashboardPage(
  dashboardHeader(title = "Argos"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tab",
      menuItem("Data Loader", tabName = "data_upload"),
      menuItem("Gene List Manager", tabName = "gene_list_manager"),
      menuItem("Data Exploration", tabName = "data_exp"),
      menuItem("Time Series Inspection", tabName = "time_series")
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      dataLoaderUI("dataLoader"),
      geneListManagerUI("geneListManager"),
      matrixExplorerUI("matrixExplorer"),
      timeSeriesExplorerUI("timeSeriesExplorer")
    )
  )
)

ui <- secure_app(ui)
