cell_image_handler <- function() {
  observeEvent(input$select_cell_image_group, {
    req(input$select_cell_image_group)
    updateSelectInput(session,
                      "select_cell_image_id",
                      choices = get_sample_list(input$select_cell_image_group))
  })
}

get_sample_list <- function(input_group, col_data) {
  idx <- col_data[["Group"]] == input_group
  str_replace_all(col_data$Sample[idx], "-", ".")
}

get_image_path <- function (sample_name) {
  # data set, treatment, idx
  print(paste0("[CellImagesHandler] Sample_name:", sample_name))
  file_name <- gsub("\\.", "-", tolower(sample_name))
  file_name_pair <- paste0(file_name, "-", c("before", "after"), ".jpg")
  # tmp_str_vec <-
  #   str_split(sample_name, pattern = "\\.", simplify = TRUE)
  # file_name <-
  #   paste0(tolower(tmp_str_vec[2]),
  #          "-",
  #          tmp_str_vec[3],
  #          "-",
  #          c("before", "after"),
  #          ".jpg")
  file.path("dataset","ngf-all-qc", "image", file_name_pair)
}

# get_image_path("NGF2-1-5")