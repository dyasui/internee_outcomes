#' Calculate proportions across multiple grouping variable combinations
#'
#' @param x A tibble or dataframe representing a subset
#' @param y A tibble or dataframe representing the full dataset
#' @param var_combos Either:
#'   - A character vector of variable names
#'   - A list of character vectors, where each vector contains variable names
#' @param var_y_combos Optional. Same format as var_combos with corresponding
#'   variable names in y. If NULL, assumes same names as in x
#' @param prefix Prefix for the proportion column names. Default is "p"
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
calculate_proportions <- function(x, y, var_combos, 
                                  var_y_combos = NULL,
                                  prefix = "p") {
  
  # Convert var_combos to list format if needed
  if (is.character(var_combos)) {
    var_combos <- list(var_combos)
  } else if (!is.list(var_combos)) {
    stop("var_combos must be a character vector or list of character vectors")
  }
  
  # Convert var_y_combos to list format if provided
  if (!is.null(var_y_combos)) {
    if (is.character(var_y_combos)) {
      var_y_combos <- list(var_y_combos)
    } else if (!is.list(var_y_combos)) {
      stop("var_y_combos must be a character vector or list of character vectors")
    }
  }
  
  # If var_y_combos not provided, use same names as x
  if (is.null(var_y_combos)) {
    var_y_combos <- var_combos
  }
  
  # Ensure both lists have same length
  if (length(var_combos) != length(var_y_combos)) {
    stop("var_combos and var_y_combos must have the same length")
  }
  
  # Start with original x
  result <- x
  
  # Loop through each combination of variables
  for (i in seq_along(var_combos)) {
    x_vars <- var_combos[[i]]
    y_vars <- var_y_combos[[i]]
    
    # Create suffix for column names (based on variable names)
    suffix <- paste(x_vars, collapse = "_")
    
    # Calculate proportions for this combination
    props <- calculate_proportions(
      x = x,
      y = y,
      by = x_vars,
      var_y = y_vars,
      join = FALSE
    )
    
    # Rename the n_x, n_y, and p columns to avoid conflicts
    props <- props |>
      rename(
        !!paste0("n.x_", suffix) := n_x,
        !!paste0("n.y_", suffix) := n_y,
        !!paste0(prefix, ".", suffix) := p
      )
    
    # Join to result
    result <- result |>
      left_join(props, by = x_vars)
  }
  
  return(result)
}

predict_internment <- function(data,
                               wra_data, ddi,
                               methods = c("main", "county", "AB"),
                               # variable names from data
                               lvars = c("STATEFIP_1940", "COUNTYICP_1940",
                                         "RACE_1940", "SEX_1940", "bpl_grp",
                                         "byr_grp"),
                               rvars = c("STATEFIP", "COUNTYICP", "RACE", "SEX", "bpl_grp", "byr_grp")
                               ) {
  library(tidyverse)

  join_vars <- setNames(rvars, lvars)

  # Internment probability based on county and race
  if ("main" %in% methods) {
    int_prob_dt <- calc_int_proportion(
      wra_data, ddi,
      by = c("STATEFIP", "COUNTYICP", "RACE"),
      label = "int_prob"
    )
    print(join_vars[1:3])
    # join probabilities with main dataset
    data <- data |>
      left_join(
        int_prob_dt,
        by = join_vars[1:3]
      )
    print("county internment successfully joined")
  }

  # rate of Japanese internees in individual's 1940 county origin
  if ("county" %in% methods) {
    ct_int_dt <- calc_int_proportion(
      wra_data, ddi,
      by = c("STATEFIP", "COUNTYICP"),
      label = "county_intprop"
    )
    print(join_vars[1:2])
    # join probabilities with main dataset
    data <- data |>
      left_join(
        ct_int_dt,
        by = join_vars[1:2]
      )
    print("county proportions successfully joined")
  }

  # Jaime Arellano-Bover's method:
  # group by state, race, birthyr, bpl
  if ("AB" %in% methods) {
    int_AB_dt <- calc_int_proportion(
      wra_data, ddi,
      by = c("STATEFIP", "RACE", "BIRTHYR", "BPL"),
      label = "intprob_AB"
    )
    print(c(join_vars[1], join_vars[3], join_vars[5:6]))
    # join probabilities with main dataset
    data <- data |>
      mutate(bpl_grp = group_bpl(BPL_1940), byr_grp = group_birthyr(BIRTHYR_1940)) |>
      left_join(
        int_AB_dt,
        by = c(join_vars[1], join_vars[3], join_vars[5:6])
      )
    print("Arellano-Bover internment successfully joined")
  }

  # group by all demographics at county level
  if ("full" %in% methods) {
    int_full_dt <- calc_int_proportion(
      wra_data, ddi,
      by = c("STATEFIP", "COUNTYICP", "SEX", "RACE", "BIRTHYR", "BPL"),
      label = "intprob_full"
    )
    print(join_vars)
    # join probabilities with main dataset
    data <- data |>
      mutate(bpl_grp = group_bpl(BPL_1940), byr_grp = group_birthyr(BIRTHYR_1940)) |>
      left_join(
        int_full_dt,
        by = join_vars
      )
    print("Full demographic internment successfully joined")
  }
    
  return(data)

}
