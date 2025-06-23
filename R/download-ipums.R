geo_vars <- c("STATEFIP", "STATEICP", "COUNTYICP", "ENUMDIST", "CITY", "URBAN", "FARM")

dem_vars <-c("SEX", "BIRTHYR", "AGE", "MARST", "CHBORN")

fam_vars <-c("MOMLOC", "POPLOC", "SPLOC", "NCHILD", "NSIBS", "NFAMS")

nat_vars <- c("RACE", "BPL", "MBPL", "FBPL", "NATIVITY", "CITIZEN")

edu_vars <- c("EDUC", "SCHOOL", "HIGRADE")

emp_vars <- c("EMPSTAT", "LABFORCE", "CLASSWKR",
                 "OCC", "OCC1950", "IND", "IND1950",
                 "WKSWORK1", "HRSWORK1", "DURUNEMP")

inc_vars <- c("INCTOT", "INCWAGE", "INCBUSFM", "INCOTHER", "OCCSCORE")

library(ipumsr)
library(tidyverse)
ipums_samples <- c("us1940b", "us1950b")

fcount_vars <- list(
  geo_vars,
  dem_vars,
  nat_vars,
  edu_vars,
  emp_vars,
  inc_vars,
  "MIGRATE5", "MIGPLAC5", "MIGCOUNTYICP5",
  "SLWT"
) |>
  unlist() |>
  as.list()
# download data using IPUMS API
fullcount_extract <- define_extract_micro(
  collection = "usa",
  description = "Full count census data for 1940 and 1950",
  samples = ipums_samples,
  variables = fcount_vars
)

submit_extract(fullcount_extract)

# get extract number of most recent submitted extract
usa_extract_submitted <- get_last_extract_info("usa")
# download to disk if extract is ready
if (is_extract_ready(usa_extract_submitted)) {
  download_extract("data/fullcount_census")
  } else {
    print("Still waiting, check back later")
  }

# only select Japanese and Chinese Americans
immigrant_vars <- list(
  list(
  "STATEFIP", "STATEICP", "COUNTYICP", "ENUMDIST", "CITY", # geographic variables
  "SEX", "BIRTHYR", "MARST",# demographics
  var_spec("BPL", attached_characteristics = c("mother", "father")),
  var_spec("NATIVITY",  attached_characteristics = c("mother", "father", "spouse")),
  "FARM", "URBAN", 
  "CITIZEN",  "EDUC", # assimilation/culture
  "LABFORCE", "OCC1950", "EMPSTAT", # labor market status
  "OCCSCORE", "INCTOT", "INCWAGE", "INCBUSFM"
  "MIGRATE5", "MIGPLAC5", "MIGCOUNTYICP5", "SAMEPLAC5", #migration (1940 only)
  var_spec("RACE", case_selections = c("4","5"))
  )
)

# download data using IPUMS API
internment_extract <- define_extract_micro(
  collection = "usa",
  description = "linked microdata for internee and control population from 1940 to 1950",
  samples = ipums_samples,
  variables = immigrant_vars
) |>
  submit_extract() |>
  wait_for_extract() |>
  download_extract("data/fullcount_census")
