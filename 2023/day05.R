# Day 5: If you give a seed a fertilizer ----------------------------------

library(readr)
library(stringr)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)

text <- readr::read_lines("2023/day05.txt")

seeds.df <-
  tibble(
    type = "seed",
    index = text[1] %>%
      str_split_1(": ") %>%
      extract2(2) %>%
      str_split_1(" ")
  )

mappings.df <-
  tibble(text = text[3:length(text)]) %>%
  mutate(group = cumsum(text == "")) %>%
  filter(text != "") %>%
  group_by(group) %>%
  summarise(text = list(text)) %>%
  mutate(title = map_chr(text, 1L) %>% str_remove(" map:$")) %>%
  transmute(
    source = str_extract(title, "^([a-z]+)-to-", group = 1L),
    destination = str_extract(title, "-to-([a-z]+)$", group = 1L),
    mapping = map(text, function(text) {
      tibble(text = text[2L:length(text)]) %>%
        separate(text, c("dst_lower", "src_lower", "length"), sep = " ") %>%
        mutate_all(as.numeric) %>%
        mutate(dst_upper = dst_lower + length,
               src_upper = src_lower + length)
    })
  )

seeds.df %>%
  left_join(mappings.df, by=c("type" = "source")) ->
  A

index <- A$index[1]
mapping <- A$mapping[[1]]

mapping %>%
  left_join()


  transmute(
    type = destination,
    index = map_to_destination(index, mapping)
  )
