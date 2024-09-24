# Day 07 ------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Definitions -------------------------------------------------------------

combinations.df <- tribble(
  ~combination, ~rank, ~identifier,
  "five-of-a-kind",  0, \(t) max(t$n) == 5,
  "four-of-a-kind",  1, \(t) max(t$n) == 4,
  "full-house",      2, \(t) all(tail(sort(t$n), 2) == c(2, 3)),
  "three-of-a-kind", 3, \(t) max(t$n) == 3,
  "two-pair",        4, \(t) all(tail(sort(t$n), 2) == c(2, 2)),
  "one-pair",        5, \(t) max(t$n) == 2,
  "high-card",       6, \(t) TRUE
)

card_to_rank <- function(card) {
  which(
    card == c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
  )
}

# Read in data ------------------------------------------------------------

hands.df <-
  read_delim(
    "2023/day07.txt",
    delim = " ",
    col_names = c("cards", "bid"), col_types = "ci"
  ) %>%
  # Tally up the cards
  mutate(
    cards = map(cards, ~str_split_1(., "")),
    cards_tally = map(cards, ~tibble(card = .) %>% count(card))
  ) %>%
  # Identify the combinations
  mutate(
    combination = map(
      cards_tally,
      function(tally) {
        combinations.df %>%
          mutate(identified = map_lgl(identifier, ~exec(., tally))) %>%
          filter(identified) %>%
          filter(rank == min(rank)) %>%
          select(combination, combination_rank = rank)
      }
    )
  ) %>%
  unnest(combination) %>%
  mutate(
    cards_rank = map(
      cards,
      function(cards) {
        map_int(cards, card_to_rank)
      }
    )
  ) %>%
  unnest_wider(cards_rank, names_sep = "_") %>%
  arrange(
    combination_rank,
    cards_rank_1, cards_rank_2, cards_rank_3, cards_rank_4, cards_rank_5
  ) %>%
  mutate(rank = n() - row_number() + 1L)

# Part 1 ------------------------------------------------------------------

hands.df %>%
  # Calculate winnings
  mutate(winnings = rank * bid) %>%
  pull(winnings) %>%
  sum()


