library(dplyr)
library(ggplot2)
library(here)
library(keedwell)
library(tidyr)

source(here("R", "add_rows.R"))

L <- expand_grid(row = 1:3, column = 1:4) |>
  mutate(symbol = c(1, 3, 4, 2, 5, 6, 1, 3, 4, 2, 3, 5)) |>
  add_cols(5:6, 6) |>
  add_rows(4:6)

ggplot(L, aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  geom_text(aes(label = symbol), size = 5, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )

ggsave(here("plots", "6x6_ryser.png"), width = 3, height = 3)
