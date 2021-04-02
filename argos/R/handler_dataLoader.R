
# PCA analysis -------------------------------

pca_plot <- function(ngf_data, my_coldata){
  ngf_sce <- SingleCellExperiment(list(counts=ngf_data), 
                                  colData=DataFrame(batch=my_coldata[["Batch"]], 
                                                    treat=my_coldata[["Group"]]))
  ngf_sce <- logNormCounts(ngf_sce)
  ngf_sce <- runPCA(ngf_sce, ncomponents=30, BSPARAM=BiocSingular::ExactParam())
  percentVar_list <- attributes(reducedDim(ngf_sce, "PCA"))$percentVar
  explained_var <- sum(percentVar_list)
  
  plot_data <- plotReducedDim(ngf_sce, dimred = "PCA", colour_by = "batch", shape_by="treat")$data
  plot_data["sample"] <- row.names(plot_data)
  colnames(plot_data) <- c("X", "Y", "batch", "treat", "sample")
  
  p <- ggplot(plot_data, aes(X, Y, colour = batch,shape = treat, size = 2))
  p <- p + geom_point()
  p <- p + theme_bw(base_size = 30)
  p <- p + xlab(paste0("PC1: ", 
                       format(round(percentVar_list[1], 2), nsmall = 2), "%")) 
  p <- p + ylab(paste0("PC2: ", 
                       format(round(percentVar_list[2], 2), nsmall = 2), "%"))
  # https://statisticsglobe.com/control-size-of-ggplot2-legend-items-in-r
  p <- p + guides(color = guide_legend(override.aes = list(size = 5)),
                  shape = guide_legend(override.aes = list(size = 5)))
  p
  list(plot=p, data=plot_data)
  
}

# UMAP analysis -------------------------------

umap_plot <- function(ngf_data, my_coldata){
  
  # Prepare data
  ngf_sce <- SingleCellExperiment(list(counts=ngf_data), 
                                  colData=DataFrame(batch=my_coldata[["Batch"]], 
                                                    treat=my_coldata[["Group"]],
                                                    sample=my_coldata[["Sample"]]))
  
  ngf_sce <- logNormCounts(ngf_sce)
  ngf_sce <- runPCA(ngf_sce, ncomponents=30, BSPARAM=BiocSingular::ExactParam())
  ngf_sce <- runUMAP(ngf_sce, dimred="PCA", n_dimred=30)
  plot_data <- plotReducedDim(ngf_sce, dimred = "UMAP", colour_by = "batch", shape_by="treat")$data
  plot_data["sample"] <- row.names(plot_data)
  colnames(plot_data) <- c("X", "Y", "batch", "treat", "sample")
  
  # Plot
  p <- ggplot(plot_data, aes(X, Y, colour = batch, shape = treat, size = 3))
  p <- p + geom_point()
  # p <- p + geom_text(aes(label=sample),hjust=0, vjust=0)
  p <- p + theme_bw(base_size = 30)
  p <- p + xlab("UMAP1") 
  p <- p + ylab("UMAP2")
  # https://statisticsglobe.com/control-size-of-ggplot2-legend-items-in-r
  p <- p + guides(color = guide_legend(override.aes = list(size = 5)),
                  shape = guide_legend(override.aes = list(size = 5)))
  p
  
  list(plot=p, data=plot_data)
}

# ngf_sce <- SingleCellExperiment(list(counts=ngf_data), 
#                                 colData=DataFrame(batch=my_coldata[["Batch"]], 
#                                                   treat=my_coldata[["Group"]]))
# 
# ngf_sce <- logNormCounts(ngf_sce)
# ###########################
# # PCA analysis
# ###########################
# ngf_sce <- runPCA(ngf_sce, ncomponents=30, BSPARAM=BiocSingular::ExactParam())
# explained_var <- sum(attributes(reducedDim(ngf_sce, "PCA"))$percentVar)
# cat("Totoal explained var in 30 PCs:", explained_var)
# colData(ngf_sce)
# plot_data <- plotReducedDim(ngf_sce, dimred = "PCA", colour_by = "batch", shape_by="treat")$data
# colnames(plot_data) <- c("PC1", "PC2", "batch", "treat")
# p <- ggplot(plot_data, aes(PC1, PC2, colour = batch,shape = treat, size = 2))
# p + geom_point()

# ###########################
# # UMAP analysis
# ###########################
# # http://bioconductor.org/packages/release/bioc/vignettes/scater/inst/doc/overview.html
# ngf_sce <- runUMAP(ngf_sce, dimred="PCA", n_dimred=30)
# plot_data <- plotReducedDim(ngf_sce, dimred = "UMAP", colour_by = "batch", shape_by="treat")$data
# plot_data
# plot_data["sample"] <- row.names(plot_data)
# colnames(plot_data) <- c("UMAP1", "UMAP2", "batch", "treat")
# p <- ggplot(plot_data, aes(UMAP1, UMAP2, colour = batch, shape = treat, size = 3))
# p <- p + geom_point()
# # p <- p + geom_text(aes(label=sample),hjust=0, vjust=0)
# p <- p + theme_bw(base_size = 30)
# # https://statisticsglobe.com/control-size-of-ggplot2-legend-items-in-r
# p <- p + guides(color = guide_legend(override.aes = list(size = 5)),
#                 shape = guide_legend(override.aes = list(size = 5)))
# p
