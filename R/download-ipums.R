library(ipumsr)
library(tidyverse)
ipums_samples <- c("us1940b", "us1950b")

geo_vars <- c("STATEFIP", "STATEICP", "COUNTYICP", "ENUMDIST", "CITY", "URBAN", "FARM")
dem_vars <-c("SEX", "BIRTHYR", "AGE", "MARST", "CHBORN")
fam_vars <-c("MOMLOC", "POPLOC", "SPLOC", "NCHILD", "NSIBS", "NFAMS")
nat_vars <- c("RACE", "BPL", "MBPL", "FBPL", "NATIVITY", "CITIZEN")
edu_vars <- c("EDUC", "SCHOOL", "HIGRADE")
emp_vars <- c("EMPSTAT", "LABFORCE", "CLASSWKR",
                 "OCC", "OCC1950", "IND", "IND1950",
                 "WKSWORK1", "HRSWORK1", "DURUNEMP")
inc_vars <- c("INCTOT", "INCWAGE", "INCBUSFM", "INCOTHER", "OCCSCORE")

# list all variables to be downloaded
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
define_ipums_extract <- function(samples = c("us1940b","us1950b"),
                                 vars,
                                 description = "Full count census data for 1940 and 1950") {
  define_extract_micro(
    collection = "usa",
    description = description,
    samples = samples,
    variables = vars
  )
}

submit_and_download_extract <- function(extract_def, download_dir = "data/fullcount_census") {
  existing <- get_extract_history("usa")
  match <- purrr::detect(existing, ~ identical(.x$definition, extract_def$definition))

  if (!is.null(match) && match$status == "completed") {
    return(download_extract(match, download_dir))
  }

  submitted <- submit_extract(extract_def)
  ready <- wait_for_extract(submitted)
  download_extract(ready, download_dir)
  return(ready)
}
