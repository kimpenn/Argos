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
# 
# 
# input_data <- my_data_norm
# input_col_data <- my_coldata
# input_target <- "Creb1"
# input_gene_list <- my_corner_stone

get_treat_idx <- function(input_str) {
  if (input_str == "group_1")
    return(1)
  if (input_str == "group_2")
    return(2)
  if (input_str == "group_3")
    return(3)
  if (input_str == "group_7")
    return(4)
}
addSmallLegend <-
  function(myPlot,
           pointSize = 2,
           textSize = 15,
           spaceLegend = 0.8) {
    myPlot +
      guides(shape = guide_legend(override.aes = list(size = pointSize)),
             color = guide_legend(override.aes = list(size = pointSize))) +
      theme(
        legend.title = element_text(size = textSize),
        legend.text  = element_text(size = textSize),
        legend.key.size = unit(spaceLegend, "lines")
      )
  }

plot_fig_2 <-
  function(input_data,
           input_col_data,
           input_target,
           input_gene_list) {
    ##################################
    # Figuers 2: Gene Mean Counts with Annotation across Treatment
    ##################################
    # the_data <- as.data.frame(input_data[-1])
    # row.names(the_data) <- input_data$Symbol
    the_data <- input_data
    treatment_list <- input_col_data$Group
    
    # Generate Annotation -------------------
    ann_list_cell_ids <- list(c(), c(), c(), c())
    ann_list_target_gene_reads <- list(c(), c(), c(), c())
    ann_list_gene_list_reads <- list(c(), c(), c(), c())
    ann_list_cell_reads <- list(c(), c(), c(), c())
    for (idx in seq_along(treatment_list)) {
      if (treatment_list[idx] != "group_c") {
        treat_idx <- get_treat_idx(treatment_list[idx])
        
        the_val <- sum(the_data[, idx])
        ann_list_cell_reads[[treat_idx]] <-
          c(ann_list_cell_reads[[treat_idx]], the_val)
        
        the_val <- sum(the_data[input_gene_list, idx])
        ann_list_gene_list_reads[[treat_idx]] <-
          c(ann_list_gene_list_reads[[treat_idx]], the_val)
        
        the_val <- unlist(strsplit(colnames(the_data)[idx], "\\."))[3]
        if (nchar(the_val) == 1) {
          the_val <- paste0("0", the_val)
        }
        the_val <- paste0("(", the_val, ")")
        ann_list_cell_ids[[treat_idx]] <-
          c(ann_list_cell_ids[[treat_idx]], the_val)
        
        the_val <- the_data[input_target, idx]
        ann_list_target_gene_reads[[treat_idx]] <- 
          c(ann_list_target_gene_reads[[treat_idx]], the_val)
      }
    }
    # Rank the cells in order -------------------
    for (idx in seq_along(ann_list_gene_list_reads)) {
      # the_order <- order(ann_list_gene_list_reads[[idx]])
      the_order <- order(ann_list_target_gene_reads[[idx]])
      
      ann_list_cell_ids[[idx]] <- 
        ann_list_cell_ids[[idx]][the_order]
      
      ann_list_target_gene_reads[[idx]] <- 
        ann_list_target_gene_reads[[idx]][the_order]
      
      ann_list_gene_list_reads[[idx]] <-
        ann_list_gene_list_reads[[idx]][the_order]   
      
      ann_list_cell_reads[[idx]] <-
        ann_list_cell_reads[[idx]][the_order]
      
    }
    # Prepare Data -------------------
    plot_df <- tibble(
      counts = as.numeric(the_data[input_target, ]),
      gene_list = colSums(the_data[input_gene_list, ]),
      treat = treatment_list
    ) %>%
      group_by(treat) %>%
      summarise(counts_avg = median(counts),
                counts_var = var(counts))
    
    # Draw Figures -------------------
    p <- ggplot(plot_df[-nrow(plot_df), ]) + theme_bw()
    y_max <- 1.3 * max(plot_df$counts_avg)
    p <- p + ylim(0, y_max)
    p <- p + geom_line(aes(x = treat, y = counts_avg, group = 1))
    p <-
      p + geom_point(aes(x = treat, y = counts_avg, size = counts_var))
    p <- p + theme(text = element_text(size = 30))
    p <- p + theme(legend.position = c(0.9, 1))
    p <- addSmallLegend(p)
    
    for (idx_i in seq_along(ann_list_cell_reads)) {
      for (idx_j in seq_along(ann_list_cell_reads[[idx_i]])) {
        loc_x <- plot_df[[idx_i, "treat"]]
        loc_y <-
          plot_df[[idx_i, "counts_avg"]] * 0.8 + (y_max / 35) * idx_j
        
        p <- p + annotate(
          "text",
          x = loc_x,
          y = loc_y,
          hjust = 3,
          size = 5,
          colour = "darkslategray",
          check_overlap = TRUE,
          label = ann_list_cell_ids[[idx_i]][idx_j]
        )
        
        p <- p + annotate(
          "text",
          x = loc_x,
          y = loc_y,
          hjust = 1.2,
          size = 5,
          colour = "chocolate",
          check_overlap = TRUE,
          label = round(ann_list_target_gene_reads[[idx_i]][idx_j])
        ) 
        
        p <- p + annotate(
          "text",
          x = loc_x,
          y = loc_y,
          hjust = -0.2,
          size = 5,
          colour = "darkslategray",
          check_overlap = TRUE,
          label = round(ann_list_gene_list_reads[[idx_i]][idx_j])
        )  
        
        p <- p + annotate(
          "text",
          x = loc_x,
          y = loc_y,
          hjust = -0.6,
          size = 5,
          colour = "chocolate",
          check_overlap = TRUE,
          label = round(ann_list_cell_reads[[idx_i]][idx_j])
        )
    

      }
    }
    p <-
      p + ggtitle(paste0(input_target, ": Median Counts across Treatments"))
    p <- p + theme(plot.title = element_text(size = 30))
    p <-
      p + geom_hline(yintercept = as.numeric(plot_df[nrow(plot_df), "counts_avg"]), colour = "red")
    p <- p + ylab("counts median")
    p
  }
