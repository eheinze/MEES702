library(rvest)
library(purrr) # for the keep() and pluck() functions
library(dplyr)

url <- "https://en.wikipedia.org/wiki/Washington,_D.C."
page <- read_html(url)

# Extract all html tables and find the population table
pop_table <- page |>
  html_elements("table") |>
  html_table() |>
  keep(~ any(grepl("Census", names(.x)))) |>
  pluck(1)

head(pop_table)

#Change the variable names and remove last row after third column
pop = pop_table |> 
  rename(Year = Census,
         Population = Pop.,
         Change_pct = `%±`) |> 
  slice(-n()) |>
  select(-3) |>
  mutate(Year = as.numeric(substr(Year, 1, 4)))
