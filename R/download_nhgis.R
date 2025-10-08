library(ipumsr)
library(tidyverse)
library(sf)
nhgis_extract <- define_extract_nhgis(
  description = "1940 county-level census data on population, race, and economic outcomes",
  datasets = list(
    ds_spec(
      "1940_cPHAe",
      data_tables = c("NT1", "NT2", "NT16", "NT18", "NT19", "NT20", "NT26", "NT91"),
      geog_levels = c("state", "county")
    ),
    ds_spec(
      "1940_sOcc",
      data_tables = "NT1",
      geog_levels = c("state", "county")
    )
  )
  
)
