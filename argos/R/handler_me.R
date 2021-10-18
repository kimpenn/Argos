#' A set of helper functions for Matrix Explore (me) Component
#' 

#' Calculate the percentage of the reads respect to the column sum.
#' 
#' @param df Data.Frame (Gene x Sample)
#'
#' @return The matrix in term of percentage.
#'
cal_percent <- function(df) {
  res <- df %>%
    lapply(function(x)
      x / sum(x) * 100) %>%
    as.data.frame()
  rownames(res) <- rownames(df)
  res
}

#' Get the Title String based on the computation states.
#' 
#' @param is_norm Boolean 
#' @param is_relative Boolean
#' @return The title string
#'
get_title <- function(is_norm, is_relative){
  if (is_norm)
    str <- "Normalized Counts"
  else
    str <- "Raw Counts"
  
  if (is_relative)
    str <- paste0("Relative ", str)
  
  str
}