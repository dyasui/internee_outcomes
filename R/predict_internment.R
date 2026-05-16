#' Calculate proportions across multiple grouping variable combinations
#'
#' @param x A tibble or dataframe representing a subset
#' @param y A tibble or dataframe representing the full dataset
#' @param vars Either:
#'   - A character vector of variable names
#'   - A list of character vectors, where each vector contains variable names
#' @param vars.y Optional. Same format as var_combos with corresponding
#'   variable names in y. If NULL, assumes same names as in x
#'
#' @return A tibble with x joined to all calculated proportions. Each set of
#'   grouping variables gets columns: n.x_{vars}, n.y_{vars}, and p.{vars}
#'
#' @examples
#' x <- slice_sample(mtcars, n = 15)
#'
#' # Single variable
#' result1 <- calculate_multiple_proportions(x, mtcars, "cyl")
#'
#' # Single combination
#' result2 <- calculate_multiple_proportions(x, mtcars, c("cyl", "vs"))
#'
#' # Multiple combinations
#' result3 <- calculate_multiple_proportions(
#'   x, mtcars,
#'   list(c("cyl", "vs"), c("gear", "am"))
#' )
calculate_proportions <- function(x, y,
                                  vars = c("STATEFIP", "COUNTYICP",
                                           "RACE", "SEX",
                                           "BIRTHYR", "BPL"),
                                  vars.y = NULL,
                                  suffix = c("_wra", "_fc")) {

  # If vars.y not provided, use same names as x
  if (is.null(vars.y)) {
    vars.y <- vars
  }

  # Ensure both lists have same length
  if (length(vars) != length(vars.y)) {
    stop("vars and vars.y must have the same length")
  }

  # Calculate proportions for this combination
  props <- full_join(
    x = x,
    y = y,
    by = vars
  ) |>
    mutate(
      n.x = replace_na(n.x, 0),
      n.y = replace_na(n.y, 0),
      prop = n.x / n.y
    )

  names(props)[colnames(props) == "n.x"] <- paste0("n", suffix[1])
  names(props)[colnames(props) == "n.y"] <- paste0("n", suffix[2])

  return(props)
}

predict_internment <- function(input, proportions,
                               # variable names from data
                               by = c("STATEFIP", "COUNTYICP", "RACE", "SEX",
                                      "BIRTHYR", "BPL"),
                               threshold = 0.5) {
  library(tidyverse)

  if ("BIRTHYR" %in% by) {
    # add year groupings if missing
    if (!"byr_grp" %in% colnames(input)) {
      input <- input |>
        mutate(bpl_yr = group_birthyr(BIRTHYR))
    }
    if (!"byr_grp" %in% colnames(proportions)) {
      proportions <- proportions |>
        mutate(bpl_yr = group_birthyr(BIRTHYR))
    }
    # use groupings instead of raw birthyear
    join_vars = str_replace(by, "BIRTHYR", "byr_grp")
  }

  if ("BPL" %in% by) {
    # add birthplace groupings if missing
    if (!"bpl_grp" %in% colnames(input)) {
      input <- input |>
        mutate(bpl_grp = group_bpl(BPL))
    }
    if (!"bpl_grp" %in% colnames(proportions)) {
      proportions <- proportions |>
        mutate(bpl_grp = group_bpl(BPL))
    }
    join_vars = str_replace(by, "BPL", "bpl_grp")
  }

  intern_grps <- proportions |>
    group_by(across(all_of(join_vars))) |>
    summarise(
      n_wra = sum(n_wra),
      n_fc = sum(n_fc),
      intern_prop = sum(n_wra) / sum(n_fc)
    )

  data <- input |>
    left_join(intern_grps, by = join_vars) |>
    mutate(
      intern_status = (intern_prop > threshold)
    )


  return(data)

}
