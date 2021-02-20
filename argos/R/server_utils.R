get_path <- function(the_input){
  the_file <- "."
  for (idx in seq_along(the_input$path)){
    if (idx > 1){
      the_file <- file.path(the_file, the_input$path[[idx]])
      # print(the_file)
    }
  }
  return (the_file)
}