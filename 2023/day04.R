# Day 4: Scorecards -------------------------------------------------------

library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Part 1 ------------------------------------------------------------------

cards.df <-
  tibble(text = readr::read_lines("2023/day04.txt")) %>%
  # Parse the text into the numbers
  mutate(
    index = text %>%
      str_extract("^Card[ ]+([0-9]+):", group = 1L) %>%
      as.integer(),
    winning = text %>%
      str_extract(":([ 0-9]+)\\|", group = 1L) %>%
      str_trim() %>%
      map(~as.integer(str_split_1(., "[ ]+"))),
    numbers = text %>%
      str_extract("\\|([ 0-9]+)$", group = 1L) %>%
      str_trim() %>%
      map(~as.integer(str_split_1(., "[ ]+")))
  ) %>%
  # Find numbers that are winning and the score
  mutate(winners = map2(winning, numbers, ~intersect(.x, .y))) %>%
  # Calculate the score based on the winners
  mutate(
    num_winners = map_int(winners, length),
    score = if_else(num_winners > 0L, 2 ^ (num_winners - 1L), 0L)
  )

sum(cards.df$score)

# Part 2 ------------------------------------------------------------------

initial_cards.df <-
  cards.df %>%
  select(index, num_winners) %>%
  mutate(num_cards = 1L)
max_index <- max(initial_cards.df$index)
winning_cards.df <-
  initial_cards.df %>%
  select(index, num_winners) %>%
  filter(num_winners > 0)
current_cards.df <- initial_cards.df %>% select(index, num_cards)
totaled_cards.df <- current_cards.df

while (nrow(current_cards.df) > 0) {
  current_cards.df <-
    current_cards.df %>%
    inner_join(winning_cards.df, by = "index") %>%
    transmute(
      index = map2(index, num_winners, ~(.x + 1L):(.x + .y),),
      num_cards
    ) %>%
    unnest(index) %>%
    filter(index <= max_index) %>%
    group_by(index) %>%
    summarise(num_cards = sum(num_cards)) %>%
    ungroup()
  totaled_cards.df <-
    totaled_cards.df %>%
    bind_rows(current_cards.df) %>%
    group_by(index) %>%
    summarise(num_cards = sum(num_cards)) %>%
    ungroup()
}

totaled_cards.df %>%
  pull(num_cards) %>%
  sum()
