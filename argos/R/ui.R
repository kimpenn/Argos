ui <- dashboardPage(
  dashboardHeader(title = "Argos"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tab",
      menuItem("Data Upload", tabName = "data_upload"),
      menuItem("Data Exploration", tabName = "data_exp"),
      menuItem("Time Series Inspection", tabName = "time_series")
    )),
  
  dashboardBody(useShinyjs(),
                tabItems(
                  tabItem(
                    tabName = "data_upload",
                    h2("Upload Dataset"),
                    fluidRow(wellPanel(
                      shinyFiles::shinyDirButton(
                        'folder',
                        'Select a folder',
                        'Please select a folder',
                        FALSE,
                        class = "btn-success"
                      )
                    )),
                    fluidRow(wellPanel(
                      actionButton("load_data_button",
                                   "Upload Data",
                                   class = "btn-primary")
                    )),
                    fluidRow(wellPanel(verbatimTextOutput("upload_log")))
                  ),
                  tabItem(
                    tabName = "data_exp",
                    h2(textOutput('title')),
                    
                    fluidRow(wellPanel(
                      selectInput(
                        "select_symbols"
                        ,
                        "Subset the table by the following Symbols"
                        ,
                        choices = character(0)
                        ,
                        multiple = TRUE
                      ),
                      actionButton("load_button",
                                   "Load Cornerstones",
                                   class = "btn-secondary"),
                      actionButton("subset_button",
                                   "Subset Table",
                                   class = "btn-primary")
                    )),
                    fluidRow(wellPanel(
                      actionButton("norm_button",
                                   "Raw/Normalized",
                                   class = "btn-success"),
                      actionButton("reset_button",
                                   "Reset Table",
                                   class = "btn-warning"),
                      actionButton("relative_button",
                                   "Relative Amts",
                                   class = "btn-primary")
                    )),
                    fluidRow(
                      box(
                        width = 12,
                        # height = 1024,
                        DT::dataTableOutput("contents"),
                        style = "overflow-y: scroll; overflow-x: scroll;"
                      )
                    )
                  ),
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

ui <- secure_app(ui)
