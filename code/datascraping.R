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
  mutate(Population = gsub(",", "", Population),
         Change_pct = gsub("%", "", Change_pct)) |>
  mutate(Change_pct = gsub("−", "-", Change_pct)) |>
  mutate(Year = as.numeric(substr(Year, 1, 4)),
         Population = as.numeric(Population),
         Change_pct = as.numeric(Change_pct))

summary(pop)

write.csv(pop, "dataderived/popDC.csv", row.names = FALSE)


# API -----
library(httr2)
library(readr)

# Build API request for NOAA station 8577330 (Solomons Island, MD)
req <- request("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter") |>
  req_url_query(
    begin_date  = "20240101",
    end_date    = "20240131",
    station     = "8577330",
    product     = "water_temperature",
    units       = "metric",
    time_zone   = "gmt",
    application = "UMCES_MEES702",
    format      = "csv"
  )

# Perform request and parse CSV response
resp <- req_perform(req)
water_temp <- resp |>
  resp_body_string() |>
  I() |>
  read_csv()

head(water_temp)

write.csv(water_temp, "dataderived/water_temp.csv", row.names = FALSE)
