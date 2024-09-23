# Day 6 -------------------------------------------------------------------

library(readr)
library(stringr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(purrr)

# Exploration -------------------------------------------------------------

distance <- function(press, time) {
  speed <- press
  speed * (time - press)
}

# Understand the shape of the objective function
ggplot() +
  geom_function(fun = distance, args = list(time = 7L)) +
  xlim(0, 7L)

# Notes: It's essentially the intersection between a line and a quadratic
# curve. There is an analytic solution to this using the quadratic formula.

max_press <- function(time, record) {
  (time + sqrt(time^2 - 4 * record))/2
}
min_press <- function(time, record) {
  (time - sqrt(time^2 - 4 * record))/2
}

# Part 1 ------------------------------------------------------------------

# Read in the data
read_lines("2023/day06.txt") %>% {
    tibble(
      time = str_split_1(.[1], "[ ]+") %>% tail(length(.) - 1L),
      record = str_split_1(.[2], "[ ]+") %>% tail(length(.) - 1L),
    )
  } %>%
  mutate_all(as.integer) %>%
  # Compute minimum and maximum press to hit record
  mutate(
    min_press = min_press(time, record),
    max_press = max_press(time, record)
  ) %>%
  # Get the integer equivalents
  mutate(
    int_min_press = floor(min_press) + 1L,
    int_max_press = ceiling(max_press) - 1L
  ) %>%
  # Compute margin of error
  mutate(
    margin_of_error = int_max_press - int_min_press + 1L
  ) %>%
  pull(margin_of_error) %>%
  prod()

# Part 2 ------------------------------------------------------------------

read_lines("2023/day06.txt") %>% {
  tibble(
    time = str_split_1(.[1], ":")[2] %>% str_remove_all("[ ]+"),
    record = str_split_1(.[2], ":")[2] %>% str_remove_all("[ ]+")
  )
} %>%
  mutate_all(as.numeric) %>%
  # Compute minimum and maximum press to hit record
  mutate(
    min_press = min_press(time, record),
    max_press = max_press(time, record)
  ) %>%
  # Get the integer equivalents
  mutate(
    int_min_press = floor(min_press) + 1L,
    int_max_press = ceiling(max_press) - 1L
  ) %>%
  # Compute margin of error
  mutate(
    margin_of_error = int_max_press - int_min_press + 1L
  ) %>%
  pull(margin_of_error) %>%
  prod()
