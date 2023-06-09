to_tidygraph_2 <- function(R, l_order, n_rows, n_cols) {

  ## VERTEX DATA FRAME
  row_vertices <- paste0("r", 1:n_rows)
  symbol_vertices <- paste0("s", 1:l_order)

  n_dummy_nodes <- l_order - n_rows
  dummy_vertices <- paste0("d", 1:n_dummy_nodes)

  l_nodes <- tibble(
    name = c(row_vertices, dummy_vertices, symbol_vertices),
    type = c(rep(TRUE, n_rows + n_dummy_nodes), rep(FALSE, l_order))
  )

  ## EDGE DATA FRAME
  f <- function(i) return(edge_tbl_2(R, i, l_order))

  l_edges <- map_df(1:n_rows, f)

  G <- tbl_graph(nodes = l_nodes, edges = l_edges)

  d_edges <- tibble(
    to = rep(symbol_vertices, as.numeric(l_order - n_cols - degree(G, symbol_vertices))),
    from = rep(dummy_vertices, each = l_order - n_cols)
  )

  tbl_graph(nodes = l_nodes, edges = bind_rows(l_edges, d_edges))

}

