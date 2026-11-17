library(purrr)
library(tibble)
library(dplyr)

# Base R leap year function from Exercise 1
is_leap_year <- function(year) {
  if (!is.numeric(year)) {
    stop("Argument 'year' must be a numeric vector.", call. = FALSE)
  }
  (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
}

years_vec <- c(1800, 1900, 1996, 2000, 2020, 2023, 2024)
leap_flags = map_lgl(years_vec, is_leap_year)

sample_groups <- list(
  group_a = rnorm(30, mean = 10, sd = 2),
  group_b = rnorm(30, mean = 15, sd = 4),
  group_c = rnorm(30, mean = 5,  sd = 1)
)

summarize_group = function(vec, group_name) {
  tibble(
    #group = group_name,
    mean = mean(vec),
    sd = sd(vec),
    median = median(vec)
  )
}

imap(sample_groups, ~summarize_group(.x, .y)) |>
  list_rbind()

tapply(iris$Sepal.Length, iris$Species, summarize_group)

dirty_years <- list(1996, "year_2000", 2004, TRUE, 2024)
map(dirty_years, is_leap_year)
safe_leap_check <- possibly(
  .f = is_leap_year, 
  otherwise = NA
)

tmp = map(dirty_years, safe_leap_check)
list_c(tmp)
