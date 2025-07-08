# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

suppressPackageStartupMessages(library(dplyr))

# Set target options:
tar_option_set(
  packages = c("tidyverse", "ipumsr", "haven", "fixest", "duckdb", "dbplyr"), # Packages that your targets need for their tasks.
  format = "rds", # Optionally set the default storage format. qs is fast.
  error = "null" # produce a result even if the target errored out.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Target list describes workflow
list(
  # Process internment camp data
  tar_target(form26_raw, "data/WRA.FORM26.PU.txt", format = "file"),
  tar_target(wra_addr, "data/WRA_prev_address.csv", format = "file"),
  tar_target(wra_data, compile_WRA(form26_raw, wra_addr)),

  # download IPUMS data and save extract locations
  tar_target(extract_def, define_ipums_extract()),
  tar_target(
    extract_ready,
    submit_and_download_extract(extract_def, download_dir = "data/fullcount_census/")
  ),
  tar_target(ddi_path, extract_ready, format = "file"),

  # Write MLP crosswalks to local database to save on memory
  tar_target(
    cw_csv,
    "data/mlp_1940_1950_v1_2_csv/mlp_1940_1950_v1.2.csv",
    format = "file"
  ),
  tar_target(cw_db, write_cw_db(db_path = "data/ipums_db.duckdb", cw_file = cw_csv, tbl_name = "mlp_crosswalks")),

  # Link microrecords
  tar_target(linked_data, link_mlp(ddi_path, cw_db, tbl = "mlp_crosswalks"))
  
)
