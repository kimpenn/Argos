# library(tidyverse)
# library(shinydashboard)
# library(DT)
#
#
# load_dataset <- function(input_path) {
#   read_csv(input_path) %>%
#     rename(Symbol = X1)
# }
#
# load_coldata <- function(input_path) {
#   read_csv(input_path)
# }
# my_data <- load_dataset("argus-pathway/dataset/counts.csv")
# my_data_norm <-
#   load_dataset("argus-pathway/dataset/norm-counts.csv")
# my_coldata <- load_coldata("argus-pathway/dataset/design_tbl.csv")
# my_corner_stone <-
#   read_csv("argus-pathway/dataset/corner_stone.csv", col_names = FALSE)[["X1"]]

# is_outlier <- function(x) {
#   return(x < quantile(x, 0.25) - 1.5 * IQR(x) |
#            x > quantile(x, 0.75) + 1.5 * IQR(x))
# }

plot_fig_1 <-
  function(input_data,
           input_col_data,
           input_target,
           input_gene_list) {
    cat("input_target:", input_target, "\n")
    cat("input_gene_list:", input_gene_list, "\n")
    
    # the_data <- as.data.frame(input_data[-1])
    # row.names(the_data) <- input_data$Symbol
    the_data <- input_data
    percent_list <- list(c(), c(), c(), c(), c())
    names(percent_list) <-
      c("group_c", "group_1", "group_2", "group_3", "group_7")
    treatment_list <- input_col_data$Group
    
    # Generate Cell ID List -------------------
    cell_id_list <- list(c(), c(), c(), c(), c())
    names(cell_id_list) <-
      c("group_c", "group_1", "group_2", "group_3", "group_7")
    for (idx_i in seq_along(treatment_list)) {
      the_val <- colnames(the_data)[idx_i]
      cell_id_list[[treatment_list[idx_i]]] <-
        c(cell_id_list[[treatment_list[idx_i]]], the_val)
    }
    
    for (idx_i in seq_along(treatment_list)) {
      the_val <-
        100 * the_data[input_target, idx_i] / sum(the_data[input_gene_list, idx_i])
      percent_list[[treatment_list[idx_i]]] <-
        c(percent_list[[treatment_list[idx_i]]], the_val)
    }
    
    treat_col <- c(
      rep("C", length(percent_list[["group_c"]])),
      rep("1", length(percent_list[["group_1"]])),
      rep("2", length(percent_list[["group_2"]])),
      rep("3", length(percent_list[["group_3"]])),
      rep("7", length(percent_list[["group_7"]]))
    )
    
    treat_col <-
      factor(treat_col, levels = c("C", "1", "2", "3", "7"))
    
    
    prec_col <- c(percent_list[["group_c"]],
                  percent_list[["group_1"]],
                  percent_list[["group_2"]],
                  percent_list[["group_3"]],
                  percent_list[["group_7"]])
    
    id_col <- c(cell_id_list[["group_c"]],
                cell_id_list[["group_1"]],
                cell_id_list[["group_2"]],
                cell_id_list[["group_3"]],
                cell_id_list[["group_7"]])
    
    plot_df <- tibble(
      treat = treat_col,
      perc_in_list = prec_col,
      id = id_col,
      out_lier = id_col
    ) %>%
      group_by(treat) %>%
      mutate(outlier_flg = ifelse(is_outlier(perc_in_list), perc_in_list, as.numeric(NA)))
    
    plot_df$out_lier[which(is.na(plot_df$outlier_flg))] <-
      as.numeric(NA)
    # Draw Figures -------------------
    y_max <- 1.3 * max(plot_df$perc_in_list)
    
    p <- ggplot(plot_df) + theme_bw()
    p <- p + geom_boxplot(aes(x = treat,
                              y = perc_in_list))
    p <- p + theme(text = element_text(size = 25))
    p <-
      p + ggtitle(paste0(input_target, ": Percentage in Gene List across Treatments"))
    p <-
      p + geom_text(
        aes(x = treat, y = perc_in_list, label = out_lier),
        na.rm = TRUE,
        nudge_y = y_max / 40,
        size = 5
      )
    res_list <- list(p, plot_df$out_lier[!is.na(plot_df$out_lier)])
    saveRDS(res_list, "my_data.Rds")
    return(res_list)
  }
# input_data <- my_data_norm
# input_col_data <- my_coldata
# input_target <- "Creb1"
# input_gene_list <- my_corner_stone

# plot_fig_1(my_data_norm, my_coldata, "Creb1", my_corner_stone)
