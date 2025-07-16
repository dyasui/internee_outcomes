calc_int_proportion <- function(internees,
                                db = "data/ipums_db.duckdb",
                                table = "ipums_microdata",
                                by = c("STATEFIP", "COUNTYICP")
                                ) {
  library(tidyverse)
  library(dbplyr)
  library(dbplyr)
  library(duckdb)

  # collect groups from 1940 census
  con <- dbConnect(duckdb(), dbdir = db)
  linking_vars <- c("STATEFIP", "COUNTYICP", "RACE", "SEX", "BIRTHYR", "BPL")

  pop_grp <- tbl(con, table) |>
    filter(YEAR==1940) |>
    # add cohort variable to census
    collect() |>
    mutate(
      bpl_grp = group_bpl(BPL),
      byr_grp = group_birthyr(BIRTHYR)
    ) |> 
    select(unname(by)) |>
    group_by(across(all_of(unname(by)))) |>
    summarise(n_j=n(), .groups = "drop") 
  dbDisconnect(con)

  # Count by group in internee data
  int_grp <- internees |>
    filter(race==5) |>
    rename_with(toupper, c("race", "sex", "birthyr", "bpl", "bpl_pop", "bpl_mom", "yrimmig", "nativity", "degfield", "educ")) |>
    group_by(across(all_of(unname(by)))) |>
    summarise(n_i = n(), .groups = "drop")

  # Join and calculate fraction
  proportions <- pop_grp |>
    full_join(int_grp, by = by) |>
    mutate(
      n_i = replace_na(n_i, 0),
      p_ij = n_i / n_j
    )

  return(proportions)
}

predict_internment <- function(df, options = c("full", "county", "Arellano-Bover"), custom_vars = NULL, wra_data) {

  df <- df |>
    mutate(
      birthyr = (BIRTHYR_1940 + BIRTHYR_1950) / 2, # average if different 
      byr_grp = group_birthyr(birthyr),
      bpl_grp = group_bpl(BPL_1940), # use 1940 birthplace
    )

  if ("county" %in% options) {
    probs <- calc_int_proportion(wra_data,
                                 by = c("STATEFIP", "COUNTYICP")) |>
      rename(prI_county = p_ij)

    df <- df |>
      left_join(probs, by = c(
                         "STATEFIP_1940"="STATEFIP", "COUNTYICP_1940"="COUNTYICP"
                       ))
  }

  if ("full" %in% options) {
    probs <- calc_int_proportion(wra_data,
                                 by = c("STATEFIP", "COUNTYICP",
                                        "SEX", "bpl_grp", "byr_grp"
                                        )) |>
      rename(prI_full = p_ij)

    df <- df |>
      left_join(probs,
                by = c("STATEFIP_1940"="STATEFIP",
                       "COUNTYICP_1940"="COUNTYICP",
                       "SEX_1940"="SEX",
                       "bpl_grp", "byr_grp"))
  }

  if ("Arellano-Bover" %in% options) {
    probs <- calc_int_proportion(wra_data,
                                 by = c("STATEFIP",
                                        "SEX", "bpl_grp", "byr_grp"
                                        )) |>
      rename(prI_AB = p_ij)

    df <- df |>
      left_join(probs, 
                by = c("STATEFIP_1940"="STATEFIP",
                       "SEX_1940"="SEX",
                       "bpl_grp", "byr_grp"))
  }

  return(df)
}
