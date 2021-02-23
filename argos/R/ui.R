ui <- dashboardPage(
  dashboardHeader(title = "Argos"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tab",
      menuItem("Data Loader", tabName = "data_upload"),
      menuItem("Gene List Manager", tabName = "gene_list_manager"),
      menuItem("Data Exploration", tabName = "data_exp"),
      menuItem("Time Series Inspection", tabName = "time_series")
    )),
  
  dashboardBody(useShinyjs(),
                tabItems(
                  dataLoaderUI("dataLoader"),
                  geneListManagerUI("geneListManager"),
                  matrixExplorerUI("matrixExplorer"),
                  
                  tabItem(
                    tabName = "time_series",
                    h2('Time Series Inspection on Gene'),
                    fluidRow(wellPanel(
                      selectInput(
                        "select_symbols_2"
                        ,
                        "Set the target gene list"
                        ,
                        choices = character(0)
                        ,
                        multiple = TRUE
                      ),
                      actionButton("load_button_2",
                                   "Load Cornerstones",
                                   class = "btn-secondary"),
                      br(),
                      selectInput(
                        "select_target"
                        ,
                        "Select the gene to inspect on"
                        ,
                        choices = character(0)
                        ,
                        multiple = FALSE
                      )
                    )),
                    
                    actionButton("fig_1_tig_button",
                                 "Trigger Fig 1", class = "btn-info"),
                    div(id = "fig_1",
                        fluidRow(
                          box(
                            width = 12,
                            solidHeader = TRUE,
                            status = "primary",
                            plotOutput("plot1", width = "100%", height = "600px")
                          )
                        )),
                    actionButton("fig_2_tig_button",
                                 "Trigger Fig 2", class = "btn-info"),
                    div(id = "fig_2",
                        fluidRow(
                          box(
                            width = 12,
                            solidHeader = TRUE,
                            status = "primary",
                            plotOutput("plot2", width = "100%", height = "600px")
                          )
                        )),
                    br(),
                    cell_image_ui()
                  )
                ))
)

# ui <- secure_app(ui)
