# Load packages required to define the pipeline:
library(targets)
library(dplyr)
library(ipumsr)
library(fixest)
# library(tarchetypes) # Load other packages as needed.

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
  tar_target(form26_raw, "data/WRA.FORM26.PU.txt", format = "file"),
  tar_target(wra_addr, "data/WRA_prev_address.csv", format = "file"),
  tar_target(wra_data, compile_WRA(form26_raw, wra_addr)),

  tar_target(extract_def, define_ipums_extract()),
  tar_target(
    extract_ready,
    submit_and_download_extract(
      extract_def,
      download_dir = "data/fullcount_census/"
    )
  ),
  tar_target(ddi_fullcount, extract_ready, format = "file"),
  tar_target(ddi_mlp, "data/mlp_v2_0/usa_00131.xml", format = "file"),
  tar_target(mlp_db, "data/mlp.duckdb", format = "file"),
  tar_target(mlp_tbl, write_ipums_db(ddi_mlp, mlp_db, "mlp_1940_1950", debug = TRUE, chunk_size = 1e7)),


  tar_target(county_stats,
             collect_county_stats(ddi_fullcount, inflator = 1.69)),
  tar_target(mlp_raw, collect_mlp(mlp_db, mlp_tbl)),
  tar_target(mlp_sample, clean_mlp(mlp_raw, county_stats, wra_data, ddi_mlp, 1.69)),
  tar_target(wage_sample, define_wage_sample(mlp_sample))

)
