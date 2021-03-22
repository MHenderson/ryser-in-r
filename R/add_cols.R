add_cols <- function(R, cols, l_order, strategy = next_col_matching) {

  for (i in cols) {

    R <- strategy(R, i, l_order)

  }

  return(R)

}
