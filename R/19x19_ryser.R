library(dplyr)
library(ggplot2)
library(here)
library(igraph)
library(tidygraph)
library(tidyr)

source(here("R", "add_cols.R"))
source(here("R", "add_rows.R"))
source(here("R", "edge_tbl.R"))
source(here("R", "edge_tbl_2.R"))
source(here("R", "next_col_matching.R"))
source(here("R", "next_row_matching.R"))
source(here("R", "to_tidygraph.R"))
source(here("R", "to_tidygraph_2.R"))

expand_grid(row = 1:3, column = 1:4) |>
  mutate(
    symbol = c(1, 3, 4, 2, 5, 6, 1, 3, 4, 2, 3, 5)
  ) |>
  add_cols(5:19, 19) |>
  add_rows(4:19) |>
  mutate(
    original = case_when(
      row <= 3 & column <= 4 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  ggplot(aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  geom_text(data = . |> filter(original), aes(label = symbol), size = 3, colour = "red") +
  geom_text(data = . |> filter(!original), aes(label = symbol), size = 2, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )

ggsave(here("plots", "19x19_ryser.png"), width = 3, height = 3)
