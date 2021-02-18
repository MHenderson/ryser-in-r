library(igraph)
library(tidygraph)
library(tidyverse)

source("next_col.R")
source("to_tidygraph.R")

l_order <- 6

R <- expand_grid(row = 1:3, column = 1:4) %>%
  mutate(symbol = c(1, 3, 4, 2, 5, 6, 1, 3, 4, 2, 3, 5))

n_rows <- length(unique(R$row))

## update the rectangle
R2 <- R %>%
  bind_rows(
    tibble(
      column = rep(5, 3),
      row = 1:3,
      symbol = next_col(R, l_order, 3, 4)
    )
  )

R2 %>%
ggplot(aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  geom_text(aes(label = symbol), size = 5, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )

# last column just augment the missing symbols

R3 <- R2 %>%
  bind_rows(
    tibble(
      column = rep(6, 3),
      row = 1:3,
      symbol = next_col(R2, l_order, 3, 5)
    )
  )

R3 %>%
ggplot(aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  geom_text(aes(label = symbol), size = 5, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )

# now source code from mhall-in-r

source("/home/matthew/workspace/mhall-in-r/R/add_rows.R")
source("/home/matthew/workspace/mhall-in-r/R/edge_tbl.R")
source("/home/matthew/workspace/mhall-in-r/R/next_row.R")
source("/home/matthew/workspace/mhall-in-r/R/to_tidygraph.R")

R3 %>%
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
