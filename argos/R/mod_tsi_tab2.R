tsiTab2UI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("gamLinePlot"), height = "500px")
}

tsiTab2Server <- function(id, InferenceRVs, GeneSelectedRV) {
  moduleServer(id, function(input, output, session) {
    output$gamLinePlot <- renderPlot({
      req(InferenceRVs$gamSce)
      req(GeneSelectedRV())
      tsi_gam_plot(InferenceRVs$gamSce, GeneSelectedRV())
    })
  })}