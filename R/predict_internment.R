calc_int_proportion <- function(wra_data, ddi,
                                by = c("STATEFIP", "COUNTYICP", "RACE"),
                                chunk_size = 1e6,
                                label = "intern_p") {
  library(tidyverse)
  library(dbplyr)
  library(duckdb)

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
    ## print(result) # for debuging
    return(result)
  }

  chunks <- read_ipums_micro_chunked(
    ddi = ddi,
    callback = IpumsDataFrameCallback$new(callback),
    chunk_size = chunk_size,
    vars = c(all_of(by), "YEAR")
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

predict_internment <- function(data,
                               wra_data, ddi,
                               methods = c("main", "county", "AB"),
                               state_var = "STATEFIP_1940",
                               county_var = "COUNTYICP_1940",
                               race_var = "RACE_1940",
                               sex_var = "SEX_1940",
                               bpl_var = "BPL_1940",
                               byr_var = "BIRTHYR_1940"
                               ) {
  library(tidyverse)

  print(paste("Looking for columns:", race_var, state_var, county_var))
  print(paste("Available columns:", paste(names(data), collapse = ", ")))

  mutate_vars <- list()

  # Internment probability based on county and race
  if ("main" %in% methods) {
    int_prob_dt <- calc_int_proportion(
      wra_data, ddi,
      by = c("STATEFIP", "COUNTYICP", "RACE"),
      label = "int_prob"
    )
    # join probabilities with main dataset
    data <- data |>
      left_join(
        int_prob_dt,
        by = setNames(
          c("RACE", "STATEFIP", "COUNTYICP"),
          c(race_var, state_var, county_var)
        )
      )
  }

  # rate of Japanese internees in individual's 1940 county origin
  if ("county" %in% methods) {
    ct_int_dt <- calc_int_proportion(
      wra_data, ddi,
      by = c("STATEFIP", "COUNTYICP"),
      label = "county_intprop"
    )
    # join probabilities with main dataset
    data <- data |>
      left_join(
        ct_int_dt,
        by = setNames(
          c("STATEFIP", "COUNTYICP"),
          c(state_var, county_var)
        )
      )
  }

  # Jaime Arellano-Bover's method:
  # group by state, race, birthyr, bpl
  if ("AB" %in% methods) {
    int_AB_dt <- calc_int_proportion(
      wra_data, ddi,
      by = c("STATEFIP", "RACE", "BIRTHYR", "BPL"),
      label = "intprob_AB"
    )
    # join probabilities with main dataset
    data <- data |>
      mutate(bpl_grp = group_bpl(BPL_1940), byr_grp = group_birthyr(BIRTHYR_1940)) |>
      left_join(
        int_AB_dt,
        by = setNames(
          c("STATEFIP", "RACE", "bpl_grp", "byr_grp"),
          c(state_var, race_var, bpl_var, byr_var)
        )
      )
  }

  # group by all demographics at county level
  if ("full" %in% methods) {
    int_full_dt <- calc_int_proportion(
      wra_data, ddi,
      by = c("STATEFIP", "COUNTYICP", "SEX", "RACE", "BIRTHYR", "BPL"),
      label = "intprob_full"
    )
    # join probabilities with main dataset
    data <- data |>
      mutate(bpl_grp = group_bpl(BPL_1940), byr_grp = group_birthyr(BIRTHYR_1940)) |>
      left_join(
        int_full_dt,
        by = setNames(
          c("STATEFIP", "COUNTYICP", "SEX", "RACE", "bpl_grp", "byr_grp"),
          c(state_var, county_var, sex_var, race_var, "bpl_grp", "byr_var")
        )
      )
  }
    
  return(data)

}
