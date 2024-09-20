library(readr)
library(stringr)
library(magrittr)
library(purrr)

# Part 1
read_lines("2023/day01.txt") %>%
  map_int(function(line) {
    as.integer(str_c(
      str_extract(line, "[0-9]"),
      str_extract(stringi::stri_reverse(line), "[0-9]")
    ))
  }) %>%
  sum()

# Part 2
digits <- map_chr(1:9, ~as.character(english::english(.)))
digits_regex <- paste0(
  "(?=([1-9]|",
  str_c(digits, collapse = "|"),
  "))"
)
from_words <- function(input) {
  if (input %in% digits) {
    as.character(which(input == digits))
  } else {
    input
  }
}
read_lines("2023/day01.txt") %>%
  map_int(function(line) {
    matches <- str_extract_all(line, digits_regex, simplify = FALSE)[[1]]
    str_c(from_words(head(matches, 1)), from_words(tail(matches, 1))) %>%
      as.integer()
  }) %>%
  sum()

str_extract_all("eightwo", digits_regex)
