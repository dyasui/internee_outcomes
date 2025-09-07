# Load packages required to define the pipeline:
library(targets)
library(dplyr)
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
  tar_target(ddi_fullcount, extract_ready, format = "file"),

  # TODO script extract def and download for MLP v2 data
  # write MLP data to local database
  tar_target(ddi_mlp, "data/mlp_v2_0/usa_00131.xml", format = "file"),
  tar_target(mlp_db, "data/mlp.duckdb", format = "file"),
  tar_target(mlp_tbl, write_ipums_db(ddi_mlp, mlp_db, "mlp_1940_1950", debug = TRUE, chunk_size = 1e7)),

  # calculate internment proportions
  tar_target(internpr_county,
             calc_int_proportion(wra_data, ddi = ddi_fullcount,
                                 by = c("STATEFIP", "COUNTYICP"),
                                 label = "internment_county")),
  tar_target(internpr,
             calc_int_proportion(wra_data, ddi = ddi_fullcount,
                                 by = c("STATEFIP", "COUNTYICP", "RACE"),
                                 label = "internment_prob")),

  # collect main analysis samples
  tar_target(sample_wide, collect_sample(mlp_db, mlp_tbl))

)
