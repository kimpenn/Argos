library(shiny)

# library(profvis)
# profvis({
# source("R/global.R")
# source("R/cal_perc.R")
# source("R/is_outlier.R")
# source("R/plot_fig_1.R")
# source("R/plot_fig_2.R")
# source("R/server.R")
# source("R/ui.R")
shinyApp(ui, server)
# runApp(app)
# })
# the_file <- "."
# for (idx in seq_along(my_data$path)){
#   print(idx)
#   if (idx > 1){
#     the_file <- file.path(the_file, my_data$path[[idx]])
#     print(the_file)
#   }
# }

# btn_reset_subset
# btn_reset_relative
# isRelatived
# isNormalized