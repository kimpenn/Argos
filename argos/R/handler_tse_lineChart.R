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

# Figure 2: Gene Mean Counts with Annotation across Treatment

tse_line_chart <-
  function(input_data,
           input_col_data,
           input_target,
           input_gene_list) {
    
    # Prepare Data -------------------
    
    the_data <- input_data
    treatment_list <- input_col_data$Group
    
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
    
    p <-
      p + ggtitle(paste0(input_target, ": Median Counts across Treatments"))
    p <- p + theme(plot.title = element_text(size = 30))
    p <-
      p + geom_hline(yintercept = as.numeric(plot_df[nrow(plot_df), "counts_avg"]), colour = "red")
    p <- p + ylab("counts median")
    p
  }

#####################################
# Mock Testing
#####################################
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
