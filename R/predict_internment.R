predict_internment <- function(census_1940, internees="data/all_internees.dta", vars=c("STATEFIP", "COUNTYICP", "SEX")) {
  library(tidyverse)
  library(dbplyr)
 
  # Ensure internee_data is a tibble; read if character
  if (is.character(internees)) {
    internees <- haven::read_dta(internees)
  }
  # Cleanup internee data
  data_int <- internees |>
    rename_with(toupper) |>
    mutate(STATEFIP = as.integer(NHGISST)/10,
           COUNTYICP = as.integer(NHGISCTY),
           across(RACE:FATH_OCC_ABROAD, as.numeric)) |>
    filter(!is.na(STATE),!is.na(COUNTY),RACE==5) |>
    # AK, HI don't have census data, so drop them here
    filter(!STATE %in% c("Alaska", "Hawaii"))

  # Check required columns
  missing_census <- setdiff(vars, colnames(census_1940))
  if (length(missing_census) > 0) {
    stop("Census data is missing required columns: ",
         paste(missing_census, collapse = ", "))
  }

  # Count by group in census
  pop_grp <- census_1940 |>
    group_by(across(all_of(vars))) |>
    summarise(n_j=n(), .groups = "drop")

  # Count by group in internee data
  int_grp <- data_int |>
    group_by(across(all_of(vars))) |>
    summarise(n_i = n(), .groups = "drop")
  
  # Join and calculate fraction
  combined <- pop_grp |>
    full_join(int_grp, by = vars) |>
    mutate(
      n_i = replace_na(n_i, 0),
      p_ij = n_i / n_j
    )

  # Attach back to census data
  census_augmented <- census_1940 |>
    left_join(combined, by = vars)

  return(census_augmented)
}
