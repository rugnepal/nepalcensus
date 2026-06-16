# Scrape the data catalog from the NSO census download portal
# Run this script to regenerate data/datacatalog.rda

library(tidyverse)
library(rvest)
library(janitor)
library(tools)

base_url    <- "https://censusnepal.cbs.gov.np"
target_url  <- paste0(base_url, "/results/downloads/census-dataset")

page <- read_html(target_url)

table_info <- page |>
  html_nodes("li") |>
  html_text() |>
  str_subset("^Table") |>
  tibble(table_name = _) |>
  mutate(
    table_name = str_remove(table_name, "\\.csv$") |> str_trim(),
    table_no   = str_extract(table_name, "(?<=Table:?\\s)\\d+"),
    title      = str_remove(table_name, "^Table:?\\s*\\d+\\s*:?\\s*"),
    href       = page |>
      html_nodes("a[href$='.csv']") |>
      html_attr("href") |>
      paste0(base_url, x = _),
    title      = str_remove(title, "csv"),
    dataset    = basename(href) |> str_remove("\\.csv$"),
    table_no   = if_else(
      str_detect(dataset, "Hhld"),
      paste0("Household ",   table_no),
      paste0("Individual ", table_no)
    )
  ) |>
  select(table_no, title, dataset, href)

datacatalog <- table_info
usethis::use_data(datacatalog, overwrite = TRUE, compress = "xz")
