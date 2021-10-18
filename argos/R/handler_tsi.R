tsi_count_non_zeros <- function(GAM, gene_name) {
  n <- sum(assays(GAM)$counts[gene_name, ] > 0)
  n_total <- length(assays(GAM)$counts[gene_name, ])
  paste0(
    "Number of Cells with positive reads: ", n, "/", n_total
  )
}

tsi_get_variated_genes <- function(file_name) {
  yhatSmooth <- read.csv(file_name)
  yhatSmooth[, 1]
}

tsi_is_variated <- function(var_gene_list, gene_name) {
  paste0("Is variated over time: ", gene_name %in% var_gene_list)
}
