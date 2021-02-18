add_cols <- function(R, cols, l_order) {

  n_rows <- max(R$row)
  n_cols <- max(R$column)

  for (i in cols) {

    R <- R %>%
      bind_rows(
        tibble(
          column = rep(i, n_rows),
          row = 1:n_rows,
          symbol = next_col(R, l_order, n_rows, i - 1)
        )
      )

  }

  return(R)
}
