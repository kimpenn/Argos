tsi_loess_plot <- function(GAM, gene_name) {
  # Calculate vertical lines about Control, Day-1, Day-2, Day-3, Day-7
  y_mid <- log1p(max(assays(GAM)$counts[gene_name, ])) - 1
  t_1 <- 3.633097
  t_7 <- 7.879723
  t_max <- 9.10
  t_delta <- (t_7 - t_1) / 3
  t_2 <- t_1 + t_delta
  t_3 <- t_2 + t_delta
  t_vec <- c(t_1, t_2, t_3, t_7)
  x_vec <- c(
    (0 + t_1) / 2, (t_1 + t_2) / 2,
    (t_2 + t_3) / 2, (t_3 + t_7) / 2,
    (t_7 + t_max) / 2
  )

  # Extract Pseudo time estimated by Slingshot
  gene_ts_df <- colData(GAM)$slingshot
  gene_ts_df <- data.frame(gene_ts_df[-2]) %>%
    rownames_to_column() %>%
    as_tibble()
  colnames(gene_ts_df) <- c("cell.name", "pseudotime")

  # Extract Reads from SCE Object
  gene_counts_df <- data.frame(assays(GAM)$counts[gene_name, ]) %>%
    rownames_to_column() %>%
    as_tibble()
  colnames(gene_counts_df) <- c("cell.name", "reads")

  # Generate Loess Plot
  gene_ts_df %>%
    merge(gene_counts_df) %>%
    as_tibble() %>%
    mutate(reads = log1p(reads)) %>%
    ggplot(aes(pseudotime, reads)) +
    geom_point(size = 3, alpha = .5, color = "grey") +
    geom_smooth(
      formula = y ~ x,
      method = "loess"
    ) +
    ylab("Log(expression + 1)") +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    ggtitle(gene_name) +
    annotate(
      geom = "vline",
      x = t_vec, # TODO: Warning: Ignoring unknown aesthetics: x
      xintercept = t_vec,
      linetype = rep("dashed", 4)
    ) +
    annotate(
      geom = "text",
      label = c("control", "day 1", "day 2", "day 3", "day 7"),
      x = x_vec,
      y = rep(y_mid, 5),
      vjust = 1,
      size = 8
    )
}
