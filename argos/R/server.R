server <- function(input, output, session) {
  ####################################
  # Security
  ####################################
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
 
   ####################################
  # Server: Data Loader
  ####################################
  globals <- reactiveValues(the_table = NULL, ori_data = NULL)
  argosDataSet <- dataLoaderSever("dataLoader")
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

  # ###################################
  # # Cell Images: Outliers
  # ##################################
  # observe({
  #   req(globals$plot_1_out_lier)
  #   cat("global$plot_1_out_lier:", globals$plot_1_out_lier, "\n")
  #   output$outliers <- renderText({
  #     globals$plot_1_out_lier
  #   })
  #   
  # })
  # 
  # observe({
  #   req(globals$plot_1_out_lier)
  #   updateSelectInput(session, "select_outliers",
  #                     choices = globals$plot_1_out_lier)
  # })
  # 
  # observeEvent(input$select_outliers, {
  #   req(input$select_outliers)
  #   
  #   file_name <-
  #     paste0(
  #       tolower(strsplit(input$select_outliers, ".", fixed = TRUE)[[1]][2]),
  #       "-",
  #       strsplit(input$select_outliers, ".", fixed = TRUE)[[1]][3],
  #       "-",
  #       c("before", "after"),
  #       ".jpg"
  #     )
  #   cat("outlier file name: \n", file_name, "\n")
  #   output$outlier_img_before <- renderUI({
  #     tags$img(src = file_name[1], width = "100%")
  #   })
  #   output$outlier_img_after <- renderUI({
  #     tags$img(src = file_name[2], width = "100%")
  #   })
  # })
  # 
  # ###################################
  # # Cell Images: Any Images
  # ##################################
  # observeEvent(input$select_cell_image_group, {
  #   req(input$select_cell_image_group)
  #   updateSelectInput(
  #     session,
  #     "select_cell_image_id",
  #     choices = get_sample_list(input$select_cell_image_group, globals$my_coldata)
  #   )
  # })
  # 
  # observeEvent(input$select_cell_image_id, {
  #   req(input$select_cell_image_id)
  #   
  #   file_name <-
  #     paste0(
  #       tolower(strsplit(
  #         input$select_cell_image_id, ".", fixed = TRUE
  #       )[[1]][2]),
  #       "-",
  #       strsplit(input$select_cell_image_id, ".", fixed = TRUE)[[1]][3],
  #       "-",
  #       c("before", "after"),
  #       ".jpg"
  #     )
  #   cat("outlier file name: \n", file_name, "\n")
  #   output$any_img_before <- renderUI({
  #     tags$img(src = file_name[1], width = "100%")
  #   })
  #   output$any_img_after <- renderUI({
  #     tags$img(src = file_name[2], width = "100%")
  #   })
  # })
}