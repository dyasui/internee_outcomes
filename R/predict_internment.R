calc_int_proportion <- function(wra_data, ddi,
                                by = c("STATEFIP", "COUNTYICP", "RACE"),
                                chunk_size = 1e6,
                                label = "intern_p") {
  library(tidyverse)
  library(dbplyr)
  library(dbplyr)
  library(duckdb)

  by <- c("STATEFIP", "COUNTYICP", "RACE", "SEX", "BIRTHYR", "BPL")
  mutate_vars <- list() # variables to be mutated if present
  groups <- by
  
  if ("BPL" %in% by) {
    mutate_vars <- append(mutate_vars, list(bpl_grp = expr(group_bpl(BPL))))
    groups <- setdiff(groups, "BPL")
  }
  
  if ("BIRTHYR" %in% by) {
    mutate_vars <- append(mutate_vars, list(byr_grp = expr(group_birthyr(BIRTHYR))))
    groups <- setdiff(groups, "BIRTHYR")
  }
  
  groups <- c(groups, names(mutate_vars))
  
  callback <- function(x, pos) {
    if (length(mutate_vars) > 0) {
      x <- x |> mutate(!!!mutate_vars)
    }
    result <- x |> count(across(all_of(c(groups, "YEAR"))))
    print(result) # for debuging
    return(result)
  }

  chunks <- read_ipums_micro_chunked(
    ddi = ddi,
    callback = IpumsDataFrameCallback$new(callback),
    chunk_size = chunk_size,
    vars = c(by, "YEAR")
  )

  pop_grp <- chunks |>
    filter(YEAR==1940) |> 
    group_by(across(all_of(groups))) |>
    summarise(n_census = sum(n), .groups = "drop") 

  # Count by group in internee data
  int_grp <- wra_data |>
    rename_with(toupper, c("race", "sex", "birthyr", "bpl", "bpl_pop", "bpl_mom", "yrimmig", "nativity", "degfield", "educ")) |>
    mutate(!!!mutate_vars) |> 
    ## filter(RACE==5) |>
    group_by(across(all_of(unname(groups)))) |>
    summarise(n_interned = n(), .groups = "drop")

  # Join and calculate fraction
  proportions <- pop_grp |>
    full_join(int_grp, by = groups) |>
    mutate(
      n_interned = replace_na(n_interned, 0),
      pr_interned = n_interned / n_census
    ) |>
    rename(!!label := pr_interned)

  return(proportions)
}

predict_internment <- function(df, options = c("full", "county", "Arellano-Bover"),  wra_data) {

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
