library(ipumsr)
library(tidyverse)

ipums_samples <- c("us1940b", "us1950b")
vars <- list(
  var_spec("RACE", case_selections = c("4","5")), # Japanese and Chinese
  "STATEFIP", "STATEICP", "COUNTYICP", "ENUMDIST", "CITY", # geographic variables
  "SEX", "BIRTHYR", "MARST",# demographics
  var_spec("BPL", attached_characteristics = c("mother", "father")),
  var_spec("NATIVITY",  attached_characteristics = c("mother", "father", "spouse")),
  "CITIZEN",  "EDUC", # assimilation/culture
  "LABFORCE", "OCC1950", "EMPSTAT", # labor market status
  "OCCSCORE", "INCTOT", "INCWAGE", "INCBUSFM"
)
# download data using IPUMS API
internment_extract <- define_extract_micro(
  collection = "usa",
  description = "linked microdata for internee and control population from 1940 to 1950",
  samples = ipums_samples,
  variables = vars
) |>
  submit_extract() |>
  wait_for_extract() |>
  download_extract("data/fullcount_census")
