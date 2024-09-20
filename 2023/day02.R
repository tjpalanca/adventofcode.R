library(readr)
library(stringr)
library(magrittr)
library(purrr)
library(dplyr)

games.df <-
  tibble(text = read_lines("2023/day02.txt")) %>%
  mutate(
    index = as.integer(str_extract(text, "^Game ([0-9]+):", group = 1L)),
    sets = text %>% map(function(text) {
      text %>%
        str_split_1(":") %>%
        extract2(2) %>%
        str_split_1(";") %>%
        map(function(set) {
          set %>%
            str_split_1(",") %>%
            str_trim() %>%
            map(function(set) {
              set <- str_split_1(set, " ")
              colors <- list()
              colors[set[2]] <- as.integer(set[1])
              colors
            }) %>%
            list_c()
        })
    }),
    max_values = sets %>% map(function(sets) {
      list(
        red = max(map_int(sets, "red", .default = 0L)),
        blue = max(map_int(sets, "blue", .default = 0L)),
        green = max(map_int(sets, "green", .default = 0L))
      )
    }),
    part1_is_possible = map_lgl(max_values, function(max_values) {
      all(
        max_values["red"] <= 12L,
        max_values["blue"] <= 14L,
        max_values["green"] <= 13L
      )
    }),
    part2_power = map_int(max_values, function(max_values) {
      max_values[["red"]] * max_values[["blue"]] * max_values[["green"]]
    })
  )

games.df %>%
  pull(part1_is_possible) %>%
  sum()

games.df %>%
  pull(part2_power) %>%
  sum()
