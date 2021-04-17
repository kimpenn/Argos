preprocess_data <- function(norm_matrix, design_table, select_flg) {
  res_list = list("normData" = norm_matrix, "colData" = design_table)
  
  if (select_flg) {
    res_list$colData <-
      design_table %>% filter((Group == "C" & Annotation == "N") |
                                Group %in% c("1", "2", "3", "7") &
                                Annotation == "Y")
    col_idx <- str_replace_all(res_list$colData$Sample, "-", ".")
    res_list$normData <- res_list$normData[, col_idx]
  }
  res_list
}

# # Mocking Test
# library(tidyverse)
# library(shinydashboard)
# library(DT)
# 
# load_dataset <- function(input_path) {
#   my_df <- as.data.frame(read_csv(input_path))
#   rownames(my_df) <- my_df$symbol
#   my_df[, -1]
# }
# 
# load_coldata <- function(input_path) {
#   read_csv(input_path)
# }
# 
# my_data_norm <- load_dataset("dataset/ngf2/count-matrix-norm.csv")
# my_coldata <- load_coldata("dataset/ngf2/design-table.csv")
# 
# res_false <- preprocess_data(my_data_norm, my_coldata, FALSE)
# dim(res_false$normData) # 17308    75
# dim(res_false$colData) # 75  4
# 
# res_true <- preprocess_data(my_data_norm, my_coldata, TRUE)
# dim(res_true$normData) # 17308    55
# dim(res_true$colData) # 55  4