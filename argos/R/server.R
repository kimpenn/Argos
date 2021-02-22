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
  # observe({
  #   req(argosGeneList)
  #   print("argosGeneList in server.R:")
  #   print(argosGeneList())
  # })
  
  ####################################
  # Server: Tab 1 Data Exploration
  ####################################
  globals$norm_flg <- FALSE

  output$contents <- DT::renderDataTable({
    datatable(globals$the_table) %>%
      formatRound(columns = colnames(globals$the_table)[-1],
                  digits = 2)
  })

  output$title <- renderText({
    "Raw Counts"
  })

  observeEvent(input$norm_button, {
    if (globals$norm_flg) {
      output$title <- renderText({
        "Raw Counts"
      })
      globals$the_table <- globals$my_data
      globals$ori_data <- globals$my_data
    } else{
      output$title <- renderText({
        "Normalized Counts"
      })
      globals$the_table <- globals$norm_data
      globals$ori_data <- globals$norm_data
    }
    globals$norm_flg <- !globals$norm_flg
  })

  observeEvent(input$load_button, {
    print("trigger!")
    print(globals$my_corner_stone)
    updateSelectInput(session, "select_symbols",
                      selected = globals$my_corner_stone)
    cat("length(input$select_symbols)",
        length(input$select_symbols),
        "\n")

  })


  observeEvent(input$subset_button,
               {
                 if (length(input$select_symbols) > 0) {
                   idx <-  globals$my_data$Symbol %in% input$select_symbols
                   globals$the_table <- globals$ori_data[idx, ]
                 } else{
                   globals$the_table <- globals$ori_data
                 }
               })

  observeEvent(input$reset_button,
               {
                 updateSelectInput(session, "select_symbols",
                                   selected = character(0))
                 globals$the_table <- globals$ori_data
               })

  observeEvent(input$relative_button,
               {
                 if (dim(globals$the_table)[1] < 100) {
                   globals$the_table <- cal_percent(globals$the_table)
                 }

               })
  # ####################################
  # # Server: Tab 2 Time Series Inspection
  # ####################################
  # observeEvent(input$fig_1_tig_button, {
  #   toggle("fig_1")
  # })
  # observeEvent(input$fig_2_tig_button, {
  #   toggle("fig_2")
  # })
  # observeEvent(input$tab, {
  #   print(input$tab)
  #   
  #   if (input$tab == "time_series") {
  #     globals$the_plot_data <-
  #       globals$norm_data[rowSums(globals$norm_data[-1]) != 0,]
  #     globals$the_plot_col_data <- globals$my_coldata
  #     updateSelectInput(session,
  #                       "select_symbols_2",
  #                       choices = globals$the_plot_data$Symbol)
  #   } else{
  #     globals$the_table <- globals$my_data
  #     globals$ori_data <- globals$my_data
  #     globals$norm_flg <- FALSE
  #     updateSelectInput(session, "select_symbols",
  #                       choices = globals$my_data$Symbol)
  #   }
  # })
  # 
  # observeEvent(input$load_button_2, {
  #   updateSelectInput(session, "select_symbols_2",
  #                     selected = globals$my_corner_stone)
  # })
  # 
  # observeEvent(input$select_symbols_2, {
  #   if (length(input$select_symbols_2) > 0) {
  #     updateSelectInput(session, "select_target",
  #                       choices = input$select_symbols_2)
  #   } else{
  #     updateSelectInput(session, "select_target",
  #                       choices = character(0))
  #   }
  # })
  # 
  # output$plot1 <- renderPlot({
  #   print("Get into plot1...")
  #   req(globals$the_plot_data)
  #   req(globals$the_plot_col_data)
  #   req(input$select_target)
  #   req(input$select_symbols_2)
  #   req(input$select_target %in% input$select_symbols_2)
  #   print("Drawing!!!!")
  #   cat("input$select_target: ",
  #       length(input$select_target),
  #       "\n")
  #   cat("input$select_symbols_2: ",
  #       length(input$select_symbols_2),
  #       "\n")
  #   
  #   res <-
  #     plot_fig_1(
  #       globals$the_plot_data,
  #       globals$the_plot_col_data,
  #       input$select_target,
  #       input$select_symbols_2
  #     )
  #   globals$plot_1_out_lier <- res[[2]]
  #   res[[1]]
  #   
  # })
  # 
  # output$plot2 <- renderPlot({
  #   print("Get into plot2...")
  #   req(globals$the_plot_data)
  #   req(globals$the_plot_col_data)
  #   req(input$select_target)
  #   req(input$select_symbols_2)
  #   req(input$select_target %in% input$select_symbols_2)
  #   print("Drawing!!!!")
  #   plot_fig_2(
  #     globals$the_plot_data,
  #     globals$the_plot_col_data,
  #     input$select_target,
  #     input$select_symbols_2
  #   )
  # })
  # 
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