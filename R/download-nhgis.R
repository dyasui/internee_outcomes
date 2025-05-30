library(ipumsr)
library(tidyverse)
library(sf)

nhgis_extract <- define_extract_nhgis(
  description = "1940 and 1950 historical county shapefiles",
  shapefiles = c("us_county_1940_tl2008", "us_county_1950_tl2008") ) |>
  submit_extract()

if (is_extract_ready(nhgis_extract)) {
  download_extract(nhgis_extract, download_dir = "data"
  }

nhgis_codes <- read_ipums_sf("data/nhgis0035_shape.zip", bind_multiple = T) |>
  st_drop_geometry() |>
  select(DECADE:STATENAM)
  pivot_wider(id_cols = c("STATENAM", "NHGISNAM"), names_from(DECADE), values_from(NHGISST, NHGISCTY, ))
