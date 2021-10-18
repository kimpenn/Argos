tsi_gam_plot <- function(GAM, gene_name) {
  y_mid <- log1p(max(assays(GAM)$counts[gene_name, ])) - 1
  t_1 <- 3.633097
  t_7 <- 7.879723
  t_max <- 9.10
  t_delta <- (t_7 - t_1) / 3
  t_2 <- t_1 + t_delta
  t_3 <- t_2 + t_delta
  t_vec <- c(t_1, t_2, t_3, t_7)
  x_vec <- c((0 + t_1) / 2, (t_1 + t_2) / 2, 
             (t_2 + t_3) / 2, (t_3 + t_7) / 2, 
             (t_7 + t_max) / 2)
  
  tradeSeq::plotSmoothers(GAM,
    assays(GAM)$counts,
    gene = gene_name,
    alpha = 1, border = TRUE
  ) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(gene_name) +
    annotate(
      geom = "vline",
      x = t_vec,
      xintercept = t_vec,
      linetype = rep("dashed", 4)
    ) +
    annotate(geom = "text",
             label = c("control", "day 1", "day 2", "day 3", "day 7"),
             x = x_vec,
             y = rep(y_mid, 5),
             vjust = 1,
             size = 8) +
    theme(text = element_text(size = 20))
}
