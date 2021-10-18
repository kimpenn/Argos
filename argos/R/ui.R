# Argos' Main UI

ui <- dashboardPage(
  title = "Argos",
  dashboardHeader(title = textOutput("projectTitle"),
                  titleWidth = 180),
  
  dashboardSidebar(
    width = 180,
    sidebarMenu(
      id = "tab",
      menuItem("Data Loader", tabName = "data_upload"),
      menuItem("Gene List Manager", tabName = "gene_list_manager"),
      menuItem("Data Exploration", tabName = "data_exp"),
      menuItem("Time Series Inspection", tabName = "time_series"),
      menuItem("Time Series Inference", tabName = "time_series_inf")
    )
  ),
  
  dashboardBody(
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    useShinyjs(),
    tabItems(
      dataLoaderUI("dataLoader"),
      geneListManagerUI("geneListManager"),
      matrixExplorerUI("matrixExplorer"),
      timeSeriesExplorerUI("timeSeriesExplorer"),
      timeSeriesInferenceUI("timeSeriesInference")
    )
  )
)

# Security  ----------------

# ui <- secure_app(ui)
