next_row_random <- function(R, i, l_order) {

  R |>
    dplyr::bind_rows(
      tibble::tibble(
           row = rep(i, l_order),
        column = 1:l_order,
        symbol = sample(1:l_order, l_order)
      )
    )

}
