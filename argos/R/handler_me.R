# df <- tibble(
#   symbol = c("a", "b", "c", "d", "e"),
#   x = 1:5,
#   y = 1,
#   z = x ^ 2 + y
# )

cal_percent <- function(df) {
  res <- df %>%
    lapply(function(x)
      x / sum(x) * 100) %>%
    as.data.frame()
  rownames(res) <- rownames(df)
  res
}