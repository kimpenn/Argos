uiDimensionReduction <- function(ns) {
  fluidPage(wellPanel(fluidRow(
    column(
      6,
      selectInput(
        ns("select_norm"),
        "Select the Normalization Method" ,
        choices = c("Log Pooling Scale (SCRAN)", "Median of Ratios (DeSeq)"),
        multiple = FALSE
      )
    ),
    column(
      6,
      selectInput(
        ns("select_dr"),
        "Select the Dimension Reduction Method",
        choices = c("PCA", "UMAP"),
        multiple = FALSE
      )
    )
  )),
  fluidRow(plotOutput(ns("DRplot"), width = "100%", height = "800px")))
}