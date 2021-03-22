next_col_matching <- function(R, i, l_order) {

  n_rows <- max(R$row)
  n_cols <- max(R$column)

  bg <- to_tidygraph_2(R, l_order, n_rows, n_cols)

  m <- max_bipartite_match(bg)

  # names of edges in the matching
  matching_names <- match(m$matching, names(m$matching))

  # add a matching indicator to the edges
  bg <- bg %>%
    activate(edges) %>%
    mutate(
      matching = to == matching_names[from]
    ) %>%
    filter(from <= n_rows)

  # just the matching itself, as a graph
  mg <- bg %>%
    activate(edges) %>%
    filter(matching)

  EE <- ends(mg, E(mg))

  R %>%
    bind_rows(
      tibble(
        column = rep(i, n_rows),
        row = 1:n_rows,
        symbol = as.numeric(gsub("s", "", EE[, 2]))
      )
    )

}
