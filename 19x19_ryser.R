library(igraph)
library(tidygraph)
library(tidyverse)

list.files("R", full.names = TRUE) %>% walk(source)

R <- expand_grid(row = 1:3, column = 1:4) %>%
  mutate(
    symbol = c(1, 3, 4, 2, 5, 6, 1, 3, 4, 2, 3, 5),
    original = TRUE
  ) %>%
  add_cols(5:19, 19) %>%
  add_rows(4:19)

R %>%
  ggplot(aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  geom_text(data = R %>% filter(original), aes(label = symbol), size = 2, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )

ggsave("19x19_ryser.png", width = 3, height = 3)
