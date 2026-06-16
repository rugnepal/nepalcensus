# Convert .rds files to .rda format for inclusion in the package data/
# Run once after downloading fresh CSVs and converting to RDS.

library(purrr)
library(stringr)
library(here)

rds_files <- list.files(
  path       = here::here("man", "inst", "backup"),
  pattern    = "[.]rds$",
  full.names = TRUE
)

walk(rds_files, function(rds_path) {
  nm      <- basename(rds_path) |> str_extract("^[A-Za-z]+\\d+")
  dataset <- readRDS(rds_path)
  assign(nm, dataset)
  out_path <- here::here("data", paste0(nm, ".rda"))
  save(list = nm, file = out_path, compress = "xz", compression_level = 9)
  message("Saved ", out_path)
})
