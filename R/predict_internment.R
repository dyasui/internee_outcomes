calc_int_proportion <- function(wra_data, ddi,
                                by = c("STATEFIP", "COUNTYICP", "RACE"),
                                chunk_size = 1e6,
                                label = "intern_p") {
  library(tidyverse)
  library(dbplyr)
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
