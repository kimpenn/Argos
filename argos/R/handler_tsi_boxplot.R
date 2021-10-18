tsi_box_plot <- function(GAM, gene_name) {
  # Calculate vertical lines about Control, Day-1, Day-2, Day-3, Day-7
  t_1 <- 3.633097
  t_7 <- 7.879723
  t_max <- 9.10
  t_delta <- (t_7 - t_1) / 3
  t_2 <- t_1 + t_delta
  t_3 <- t_2 + t_delta
  t_vec <- c(t_1, t_2, t_3, t_7)

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

  # Generate Box Plot
  gene_ts_df %>%
    merge(gene_counts_df) %>%
    as_tibble() %>%
    mutate(reads = log1p(reads)) %>%
    mutate(Group = case_when(
      pseudotime <= t_1 ~ "Control",
      pseudotime <= t_2 ~ "Day 1",
      pseudotime <= t_3 ~ "Day 2",
      pseudotime <= t_7 ~ "Day 3",
      pseudotime > t_7 ~ "Day 7"
    )) %>%
    ggplot(aes(x = Group, y = reads)) +
    geom_boxplot() +
    ggtitle(gene_name) +
    theme(text = element_text(size = 20))
}
