rm(list = ls())
library(ggplot2)

# Load the function
source("code/is_leap_year.R")

# Test the function
test_years <- c(1900, 2000, 2004, 2023, 2024, 2100)
is_leap_year(test_years)

is_leap_year(20)
is_leap_year(20.1)
is_leap_year(1900)
is_leap_year(2000)
is_leap_year(2004)
is_leap_year(2023)
is_leap_year("two thousand")

test_years <- c(1900, 2000, 2004, 2023, 2024, 2100)
is_leap_year(test_years)
