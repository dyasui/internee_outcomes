# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "ipumsr", "haven", "fixest", "duckdb", "dbplyr"), # Packages that your targets need for their tasks.
  format = "rds", # Optionally set the default storage format. qs is fast.
  error = "null" # produce a result even if the target errored out.
)

tar_source()

# global object definitions
form26_raw <- "data/WRA.FORM26.PU.txt"
wra_addr <- "data/WRA_prev_address.csv"
ddi_mlp <- "data/mlp_v2_0/usa/00131.xml"
db_file_mlp <- "data/mlp.duckdb"
county_shp <- "data/gis/nhgis0035_shape.zip"

# Target list describes workflow
list(
  # WRA camp records cleaning
  tar_target(wra_data, compile_WRA(form26_raw, wra_addr)),
  tar_target(wra_grps, collect_wra_demographics(wra_data)),
  tar_target(internment_groups, calculate_proportions(wra_grps, fc_grps)),

  # 1940 Full count census data
  tar_target(extract_def, define_ipums_extract()),
  tar_target(
    extract_ready,
    submit_and_download_extract(
      extract_def,
      download_dir = "data/fullcount_census/"
    )
  ),
  tar_target(ddi_fullcount, extract_ready, format = "file"),
  tar_target(fc_grps, collect_census_demographics(ddi_fullcount)),
  tar_target(county_demographics, collect_county_demographics(internment_groups)),
  tar_target(county_income,
             collect_county_income(ddi_fullcount, inflator = 1.69)),

  # MLP data
  tar_target(mlp_tbl, write_ipums_db(ddi_mlp, db_file_mlp, "mlp_1940_1950", debug = TRUE, chunk_size = 1e7)),
  tar_target(mlp_raw, collect_mlp(db_file_mlp, mlp_tbl)),
  tar_target(mlp_sample, clean_mlp(mlp_raw, county_stats, wra_data, ddi_fullcount, 1.69)),
  tar_target(wage_sample, define_wage_sample(mlp_sample)),


  # Figures
  tar_target(internment_map, internment_proportion_map(county_demographics, county_shp))

)
