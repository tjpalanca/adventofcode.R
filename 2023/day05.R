# Day 5: If you give a seed a fertilizer ----------------------------------

library(readr)
library(stringr)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)

# Preparation -------------------------------------------------------------

text <- read_lines("2023/day05-sample.txt")

mappings.df <-
  tibble(text = text[3:length(text)]) %>%
  mutate(group = cumsum(text == "")) %>%
  filter(text != "") %>%
  group_by(group) %>%
  summarise(text = list(text)) %>%
  mutate(title = map_chr(text, 1L) %>% str_remove(" map:$")) %>%
  transmute(
    src = str_extract(title, "^([a-z]+)-to-", group = 1L),
    dst = str_extract(title, "-to-([a-z]+)$", group = 1L),
    mapping = map(text, function(text) {
      tibble(text = text[2L:length(text)]) %>%
        separate(text, c("dst_lower", "src_lower", "length"), sep = " ") %>%
        mutate_all(as.numeric) %>%
        mutate(dst_upper = dst_lower + length - 1,
               src_upper = src_lower + length - 1)
    })
  ) %>%
  unnest(mapping) %>%
  select(src, dst, src_lower, src_upper, dst_lower, dst_upper)

maps.df <-
  mappings.df %>%
  distinct(src, dst)

# Part 1 ------------------------------------------------------------------

seeds.df <-
  tibble(
    type = "seed",
    index = text[1] %>%
      str_split_1(": ") %>%
      extract2(2) %>%
      str_split_1(" ") %>%
      as.numeric()
  )

map_to_next <- function(seeds, mappings) {

  seeds %>%
    left_join(
      mappings,
      by = join_by(
        type == src,
        between(index, src_lower, src_upper)
      )
    ) %>%
    transmute(
      type = first(dst, na_rm = TRUE),
      index = coalesce(index - src_lower + dst_lower, index)
    )

}

seeds.df %>%
  map_to_next(mappings.df) %>%
  map_to_next(mappings.df) %>%
  map_to_next(mappings.df) %>%
  map_to_next(mappings.df) %>%
  map_to_next(mappings.df) %>%
  map_to_next(mappings.df) %>%
  map_to_next(mappings.df) ->
  locations.df

min(locations.df$index)

