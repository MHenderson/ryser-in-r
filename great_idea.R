library(igraph)
library(tidygraph)
library(tidyverse)

list.files("R", full.names = TRUE) %>% walk(source)

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

# remove clashes
#RR <- RR %>% group_by(row) %>% add_count(symbol, name = "row_count")
#RR <- RR %>% group_by(column) %>% add_count(symbol, name = "column_count")

# n.b. order here matters (col first or row first)
#RR <- RR %>% group_by(row) %>% distinct(symbol, .keep_all = TRUE)
#RR <- RR %>% group_by(column) %>% distinct(symbol, .keep_all = TRUE)

library(naniar)

# remove row duplicates
RR <- RR %>%
  group_by(row) %>%
  mutate(duplicate = duplicated(symbol)) %>%
  mutate(symbol = case_when(
    duplicate ~ 0,
    TRUE ~ symbol
  )) %>%
  replace_with_na(replace = list(symbol = 0)) %>%
  select(-duplicate)

# remove column duplicates
RR <- RR %>%
  group_by(column) %>%
  mutate(duplicate = duplicated(symbol)) %>%
  mutate(symbol = case_when(
    duplicate ~ 0,
    TRUE ~ symbol
  )) %>%
  replace_with_na(replace = list(symbol = 0)) %>%
  select(-duplicate)

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

# augment RR with used symbols
used_rows <- RR %>% group_by(row) %>% summarise(used_row = list(symbol))
used_columns <- RR %>% group_by(column) %>% summarise(used_column = list(symbol))

RR <- left_join(left_join(RR, used_rows), used_columns)

# now augment RR with missing symbols
RR <- RR %>%
  mutate(
    missing_column = map(used_column, ~setdiff(1:10, .)),
    missing_row = map(used_row, ~setdiff(1:10, .))
  )

# now add the intersection of missing_column and missing_row
RR <- RR %>%
  mutate(
    available = map2(missing_row, missing_column, intersect)
  )

# can't be completed because
sum(map_dbl(RR$available, length) == 0) > 0

# hang on, we only care about that condition for empty cells
# i.e. those with na
RR_ <- RR %>%
  filter(is.na(symbol)) %>%
  mutate(
    n_available = map_dbl(available, length),
    blocked = n_available == 0
  )

# so there are three cells that can't be filled, so
# the partial latin square is incompletable
RR_ %>%
  filter(blocked) %>%
  select(row, column, available)

# for a blocked cell (x, y) there must be at least one
# symbol used in the column that could be moved into row x
# in (x, y)
# For example, in (2, 7) I move the 10 down from the first row.

# notice that the original latin rectangle is still intact
# is that inevitable? i think so, because any clashes can
# only occur beyond its boundaries

RR_ <- RR_ %>%
  mutate(
    label = map_chr(available, paste, collapse = ",")
  )

# plot number available in each cell
RR %>%
  ggplot(aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  #geom_text(aes(label = symbol), size = 5, colour = "white") +
  geom_text(data = RR_, aes(label = n_available), size = 3, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )

# plot list of which symbols available in each cell
RR %>%
  ggplot(aes(column, row)) +
  geom_tile(aes(fill = symbol)) +
  #geom_text(aes(label = symbol), size = 5, colour = "white") +
  geom_text(data = RR_, aes(label = label), size = 3, colour = "white") +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position  = "none",
  )

## the next natural thing to do might be to look symbols and
## look for the largest size of an independent subset among
## the symbols and then add that to the square.

## or even just go through the symbols in order. add as many
## 1s as possible then as many 2s and so on ...

## of course, there's no point doing this until we solve the
## issue of the squares that have no available symbol


