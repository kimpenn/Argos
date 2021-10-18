tsiTab1UI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("loessLinePlot"), height = "500px")
  # box(
  #   title = "Nonlinear Time Series Plot",
  #   width = 12,
  #   solidHeader = TRUE,
  #   status = "primary",
  #   collapsible = TRUE,
  #   plotOutput(ns("gamLinePlot"), height = "500px")
  # )
}

tsiTab1Server <- function(id, InferenceRVs, GeneSelectedRV) {
  moduleServer(id, function(input, output, session) {
    output$loessLinePlot <- renderPlot({
      req(InferenceRVs$gamSce)
      req(GeneSelectedRV())
      tsi_loess_plot(InferenceRVs$gamSce, GeneSelectedRV())
    })
  })}