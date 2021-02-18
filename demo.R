library(igraph)
library(tidygraph)
library(tidyverse)

source("R/add_cols.R")
source("R/next_col.R")
source("R/to_tidygraph.R")

source("/home/matthew/workspace/mhall-in-r/R/add_rows.R")
source("/home/matthew/workspace/mhall-in-r/R/edge_tbl.R")
source("/home/matthew/workspace/mhall-in-r/R/next_row.R")
source("/home/matthew/workspace/mhall-in-r/R/to_tidygraph.R")

l_order <- 6
n_cols <- 4
n_rows <- 3

R <- expand_grid(row = 1:3, column = 1:4) %>%
  mutate(symbol = c(1, 3, 4, 2, 5, 6, 1, 3, 4, 2, 3, 5))

R %>%
  ggplot(aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  geom_text(aes(label = symbol), size = 5, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )

R %>%
  add_cols(5:6, l_order) %>%
  add_rows(4:6) %>%
  ggplot(aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  geom_text(aes(label = symbol), size = 5, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )

R %>%
  add_cols(5:6, l_order) %>%
  add_rows(4:6) %>%
  add_cols(7:19, 19) %>%
  add_rows(7:19) %>%
  ggplot(aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  geom_text(aes(label = symbol), size = 5, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )
