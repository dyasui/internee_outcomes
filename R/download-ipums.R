library(ipumsr)
library(tidyverse)

# download data using IPUMS API
define_ipums_extract <- function(samples = c("us1940b","us1950b"),
                                 description = "Full count census data for 1940 and 1950") {

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

  define_extract_micro(
    collection = "usa",
    description = description,
    samples = samples,
    variables = fcount_vars
  )
}

submit_and_download_extract <- function(extract_def, download_dir = "data/fullcount_census") {
  # 1. Check for existing downloads
  existing_files <- list.files(path = download_dir, pattern = "^usa_\\d+\\.xml$", full.names = TRUE)
  
  if (length(existing_files) > 0) {
    # Assume the latest by modification time is what we want
    latest_file <- existing_files[which.max(file.info(existing_files)$mtime)]
    message("Using existing extract file: ", latest_file)
    return(latest_file)
  }
  
  # 2. If no existing extract, submit and download
  submitted <- submit_extract(extract_def)
  ready <- wait_for_extract(submitted)
  download_extract(ready, download_dir)
  
  # 3. Construct path of new download
  new_file <- file.path(download_dir, sprintf("usa_%05d.xml", ready$description$number))
  message("Downloaded new extract file: ", new_file)
  
  return(new_file)
}
