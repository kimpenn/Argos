# df <- tibble(
#   symbol = c("a", "b", "c", "d", "e"),
#   x = 1:5,
#   y = 1,
#   z = x ^ 2 + y
# )

cal_percent <- function(df){
  df %>% 
    select(colnames(df)[-1]) %>%
    lapply(function(x) x / sum(x) * 100) %>%
    as.data.frame() %>%
    as_tibble() -> df[colnames(df)[-1]]
  return(df)
}