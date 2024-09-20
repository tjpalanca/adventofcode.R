library(readr)
library(dplyr)
library(tidyr)

# Load the map in
lines <- readr::read_lines("2023/day03.txt")
width <- max(map_int(lines, nchar))
height <- length(lines)

# Locate all the numbers on the map
str_locate_all(lines, "[0-9]+") %>%
  map_dfr(as_tibble, .id = "row") %>%
  mutate(row = as.integer(row)) %>%
  # For each position retrive the number
  mutate(num = pmap_int(
    list(row, start, end),
    ~as.integer(str_sub(lines[..1], ..2, ..3))
  )) %>%
  # Find the adjacent points to each position
  mutate(
    adjacent_coords = pmap(
      list(row, start, end),
      function(row, start, end) {
        bind_rows(
          expand_grid(
            col = c(pmin(start, end) - 1L, pmax(start, end) + 1L),
            row = c(row, row - 1L, row + 1L)
          ),
          expand_grid(
            row = c(row - 1L, row + 1L),
            col = start:end
          )
        ) %>%
          filter(row >= 1, row <= height) %>%
          filter(col >= 1, col <= width)
      }
    ),
    # Find the symbols adjacent
    adjacent_symbols = map(adjacent_coords, function(coords) {
      coords %>%
        mutate(
          symbol = map2_chr(row, col, function(row, col) {
            str_sub(lines[row], col, col)
          })
        )
    }),
    # Check if any of them are symbols
    is_part_number = map_lgl(
      adjacent_symbols,
      ~some(pull(., symbol), ~!. %in% c(".", as.character(0:9)))
    )
  ) ->
  numbers.df

# Part 1
numbers.df %>%
  filter(is_part_number) %>%
  pull(num) %>%
  sum()

# A gear is any "*" that is beside exactly 2 part numbers
numbers.df %>%
  # Only get the part numbers
  filter(is_part_number) %>%
  # Unnest to the symbol level
  select(num, adjacent_symbols) %>%
  unnest(adjacent_symbols) %>%
  # Find all the gears
  filter(symbol == "*") %>%
  # Count the number of part numbers for each gear
  group_by(symbol, row, col) %>%
  summarise(
    part_numbers = list(num),
    num_part_numbers = map_int(part_numbers, length)
  ) %>%
  ungroup() %>%
  # Get only the gears with 2 part numbers adjacent
  filter(num_part_numbers == 2) %>%
  # Get the gearing ratio
  mutate(gearing_ratio = map_int(part_numbers, prod)) %>%
  pull(gearing_ratio) %>%
  sum()

