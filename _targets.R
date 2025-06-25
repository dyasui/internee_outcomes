# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

suppressPackageStartupMessages(library(dplyr))

# Set target options:
tar_option_set(
  packages = c("tidyverse", "ipumsr", "haven", "fixest", "duckdb", "dbplyr"), # Packages that your targets need for their tasks.
  format = "fst_tbl", # Optionally set the default storage format. qs is fast.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Target list describes workflow
list(
  tar_target(form26_file, "data/WRA.FORM26.PU.txt", format = "file"),
  tar_target(wra_addr, "data/WRA_prev_address_list.csv", format = "file"),
  tar_target(nhgis, "data/nhgis_codes.csv", format = "file"),
  tar_target(wract, wra_counties(nhgis, wra_addr)),
  tar_target(wra_data, compile_WRA(form26_file, wract))
)
