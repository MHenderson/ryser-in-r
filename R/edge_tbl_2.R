edge_tbl_2 <- function(R, i, l_order = 3) {

  all_symbols <- 1:l_order

  # symbols used in row i
  used <- R |> dplyr::filter(row == i) |> dplyr::pull(symbol)

  # symbols missing from row i
  missing <- setdiff(all_symbols, used)

  # edge data frame for column i
  edge_df <- tibble::tibble(
    to = paste0("s", missing)
  ) |>
    dplyr::mutate(from = paste0("r", i))

  return(edge_df)
}
