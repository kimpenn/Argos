cell_image_handler <- function() {
  observeEvent(input$select_cell_image_group, {
    req(input$select_cell_image_group)
    print(get_sample_list())
    updateSelectInput(session,
                      "select_cell_image_id",
                      choices = get_sample_list(input$select_cell_image_group))
  })
}

get_sample_list <- function(input_group, col_data) {
  idx <- col_data[["Group"]] == paste0("group_", input_group)
  col_data$Sample[idx]
}