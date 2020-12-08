num_formatter <- function(x) {
  dplyr::case_when(
    x < 1e3 ~ as.character(x),
    x < 1e6 ~ paste0(as.character(x/1e3), "K"),
    x < 1e9 ~ paste0(as.character(x/1e6), "M"),
    x < 1e12 ~ paste0(as.character(x/1e9), "B"),
    TRUE ~ "To be implemented..."
  )
}