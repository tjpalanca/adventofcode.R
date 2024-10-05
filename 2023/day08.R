# Day 08 ------------------------------------------------------------------

library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(magrittr)

# Processing --------------------------------------------------------------

text <- read_lines("2023/day08.txt")
instructions <- str_split_1(text[1], "")
nodes.df <-
  tibble(text = text[3:length(text)]) %>%
  transmute(
    node = str_extract(text, "^([A-Z]{3})", 1L),
    left = str_extract(text, "\\(([A-Z]{3}),", 1L),
    right = str_extract(text, ", ([A-Z]{3})\\)", 1L),
    current = node == "AAA",
    final = node == "ZZZ"
  )

# Part 1 ------------------------------------------------------------------

current <- function(nodes) {
  nodes %>%
    filter(current) %>%
    pull(node)
}

is_final <- function(nodes) {
  nodes %>%
    filter(current, final) %>%
    nrow() %>%
    is_greater_than(0)
}

move <- function(nodes, instruction) {
  nodes %>%
    mutate(
      current = node == nodes %>%
        filter(current) %>%
        pull(if_else(instruction == "L", left, right))
    )
}

steps <- 0L
while (!is_final(nodes.df)) {
  nodes.df %<>% move(instructions[steps %% length(instructions) + 1])
  steps <- steps + 1L
}

print(paste("Steps: ", steps))
