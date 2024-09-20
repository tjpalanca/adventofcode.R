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
digits_regex <- str_c("([1-9]|", str_c(digits, collapse = "|"),")")
from_words <- function(input) {
  if (input %in% digits) {
    as.character(which(input == digits))
  } else {
    input
  }
}
read_lines("2023/day01.txt") %>%
  map_int(function(line) {
    first <- stringi::stri_extract_first_regex(line, digits_regex)
    last_index <- tail(pracma::refindall(line, digits_regex), 1)
    last <- stringi::stri_extract_last_regex(str_sub(line, last_index), digits_regex)
    as.integer(str_c(from_words(first), from_words(last)))
  }) %>%
  sum()
