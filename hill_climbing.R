library(igraph)
library(tidygraph)
library(tidyverse)

list.files("R", full.names = TRUE) %>% walk(source)

R <- expand_grid(row = 1:3, column = 1:4) %>%
  mutate(symbol = c(1, 3, 4, 2, 5, 6, 1, 3, 4, 2, 3, 5))

set.seed(42)

RR <- R %>%
  add_cols(5:10, 10, strategy = next_col_random) %>%
  add_rows(4:10, strategy = next_row_random)

RR %>%
  ggplot(aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  geom_text(aes(label = symbol), size = 5, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )

# calculate distance
# missing in row i
used_row <- function(RR, i) RR %>% filter(row == i) %>% pull(symbol) %>% unique()
missing_row <- function(RR, i) setdiff(1:10, used_row(RR, i))

missing_row(RR, 1)

used_col <- function(RR, i) RR %>% filter(column == i) %>% pull(symbol) %>% unique()
missing_col <- function(RR, i) setdiff(1:10, used_col(RR, i))

missing_col(RR, 1)

row_error <- function(RR, i) length(missing_row(RR, i))
col_error <- function(RR, i) length(missing_col(RR, i))

row_errors <- map_dbl(1:10, row_error, RR = RR)
col_errors <- map_dbl(1:10, col_error, RR = RR)

error <- sum(row_errors, col_errors)

# find a row with a repeated symbol, arbitrarily swap that symobl
# for a symbol missing in that row
# e.g. row 1 has two 1s and is missing a 6 so replace one of the 1s
# with a 6.

# find row with repeated symbol
row_ <- which(row_errors > 0)[1]
# find repeated symbol in that row
repeated_symbol <- RR %>% filter(row == row_) %>% group_by(symbol) %>% count() %>% filter(n > 1) %>% pull(symbol) %>% first()
# find missing symbol in the same row
missing_symbol <- setdiff(1:10, RR %>% filter(row == row_) %>% pull(symbol)) %>% first()
# find first instance of repeated symbol
x <- RR %>% filter(row == row_, symbol == repeated_symbol) %>% head(1)
# update RR
RR2 <- RR %>%
  mutate(symbol = case_when(
    row == 1 & column == 1 ~ 5,
    TRUE ~ symbol
  ))

RR2 %>%
  ggplot(aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  geom_text(aes(label = symbol), size = 5, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )

row_errors <- map_dbl(1:10, row_error, RR = RR2)
col_errors <- map_dbl(1:10, col_error, RR = RR2)

error <- sum(row_errors, col_errors)
error
