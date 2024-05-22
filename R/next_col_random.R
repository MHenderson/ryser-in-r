next_col_random <- function(R, i, l_order) {

  n_rows <- max(R$row)
  n_cols <- max(R$column)

  R |>
    dplyr::bind_rows(
      tibble::tibble(
        column = rep(i, n_rows),
           row = 1:n_rows,
        symbol = sample(1:l_order, n_rows)
      )
    )

}
