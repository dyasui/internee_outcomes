define_wage_sample <- function(mlp_sample) {
  mlp_sample |>
    pivot_wider(
      names_from = YEAR,
      values_from = RACE:age_adj
    ) |>
    mutate(
      consistent_wage = ifelse((is.na(INCWAGE_1940) | is.na(INCWAGE_1950)), FALSE, TRUE),
      consistent_race = ifelse(RACE_1940 != RACE_1950 | is.na(RACE_1940), FALSE, TRUE),
      consistent_place = ifelse(is.na(COUNTYICP_1940)|is.na(COUNTYICP_1950), FALSE, TRUE),
      consistent_occ  = ifelse(is.na(OCC1950_1940) | is.na(OCC1950_1950), FALSE, TRUE),
      consistent_all  = ifelse(consistent_wage & consistent_race & consistent_place & consistent_occ, TRUE, FALSE)
    ) |>
    filter(consistent_all == TRUE) |>
    rename(
      RACE = race_adj,
      SEX  = sex_adj
    ) |>
    select(-c("RACE_1940", "RACE_1950", "SEX_1940", "SEX_1950"))
}

clean_mlp <- function(wide_df, internpr, countystats, ddi, inflator = 1.6) {
  wide_df |>
    left_join(
      internpr,
      by = c(
        "RACE_1940"="RACE",
        "STATEFIP_1940"="STATEFIP",
        "COUNTYICP_1940"="COUNTYICP"
      )
    ) |>
    clean_wide_vars(ddi = ddi, inflator = inflator) |>
    mutate(
      Earning_growth =
        (INCWAGE_adj_1950 - INCWAGE_adj_1940) /
        INCWAGE_adj_1940
    ) |>
    # pivot to longer and clean variables by year
    clean_long_vars(ddi = ddi) |>
    left_join(countystats, by = c("STATEFIP", "COUNTYICP")) |>
    select(-c("n_census", "n_interned", "INCWAGE")) |>
    rename(INCWAGE = INCWAGE_adj) |>
    mutate(
      age_adj = as.integer(YEAR) - birthyr_adj,
      employed = as_factor(employed),
      migrant = as_factor(migrate10)
    ) 
}

collect_mlp <- function(db, table,
                           vars = c("RACE", "BIRTHYR", "INCWAGE",
                                    "STATEFIP", "COUNTYICP", "AGE",
                                    "SEX", "EDUCD", "CLASSWKR",
                                    "WKSWORK1", "OCC1950", "OCCSCORE",
                                    "EMPSTAT")) {
  # connect to database on file
  con <- dbConnect(duckdb(), db)
  # disconnect from database on function exit
  on.exit(dbDisconnect(con))

  # save table as temporary object
  db_tbl <- tbl(con, table)

  # collect HIKs of interest from 1940 data
  hiks_to_keep <- db_tbl |>
    filter(YEAR == 1940, RACE %in% c(4,5)) |>
    select(HIK)

  # Main query: join to keep only relevent HIKs,
  # then filter and collect
  reg_data <- db_tbl |>
    semi_join(hiks_to_keep, by = "HIK") |>
    filter(
      !is.na(STATEFIP),
      !is.na(COUNTYICP),
      (BIRTHYR < 1926)
    ) |>
    select(HIK, YEAR, all_of(vars)) |>
    collect() |>
    pivot_wider(
      id_cols = HIK, names_from = YEAR,
      values_from = all_of(vars)
    ) 

  return(reg_data)
}

adjust_dollar_vars <- function(df, inflator = 1.69,
                               harmonize_incwage = TRUE,
                               replace = FALSE,
                               name_style = c("var_adj_year", "var_year_adj")) {
  name_style <- match.arg(name_style)
  cols <- grep("^(INCWAGE|INCTOT|INCBUSFM|INCOTHER)_(1940|1950)$", names(df), value = TRUE)
  if (!length(cols)) return(df)

  have_incwage_1940 <- "INCWAGE_1940" %in% names(df)
  incwage_1950_top <- if (harmonize_incwage && have_incwage_1940) 5001 * inflator else 10000

  top_1950 <- c(INCWAGE = incwage_1950_top, INCTOT = 10000, INCBUSFM = 10000, INCOTHER = 10000)

  na_codes_for <- function(var) {
    switch(var,
      INCWAGE  = c(999999, 999998),
      INCTOT   = c(9999999, 9999998),
      INCBUSFM = c(99999, 99998),
      INCOTHER = c(99999, 99998),
      numeric(0)
    )
  }

  for (col in cols) {
    base <- sub("_(1940|1950)$", "", col)
    yr   <- as.integer(sub(".*_(\\d{4})$", "\\1", col))
    x    <- as.numeric(df[[col]])

    # set NA/unknown codes to NA (preserve negatives like net loss)
    na_codes <- na_codes_for(base)
    x[x %in% na_codes] <- NA_real_

    # inflate 1940 to 1950 dollars
    if (yr == 1940) x <- x * inflator

    # pick topcode
    top <- if (yr == 1940 && base == "INCWAGE") 5001 * inflator else
           if (yr == 1950) top_1950[[base]] else NA_real_

    # cap at top (vectorized)
    if (!is.na(top)) x <- ifelse(x > top, top, x)

    # name for output column
    newname <- if (replace) {
      col
    } else if (name_style == "var_adj_year") {
      paste0(base, "_adj_", yr)
    } else {
      paste0(base, "_", yr, "_adj")
    }

    df[[newname]] <- x
  }

  df
}

clean_wide_vars <- function(wide_df, ddi, inflator = 1.69) {
  # ten-year inflation rate from bls.gov/data/inflation_calculator.htm
  wide_df <- wide_df |>
    # adjust dollar amounts for inflation and missing values
    adjust_dollar_vars(inflator = 1.69) |>
    # deal with inconsistencies in reported birthyear across censuses by averaging
    mutate(birthyr_adj = round( (BIRTHYR_1950 + BIRTHYR_1940) / 2 )) |>
    # base time-invariant demographics on 1940 response
    mutate(
      sex_adj = ifelse(SEX_1940==1, "Male", "Female"),
      race_adj = case_when(RACE_1940==5 ~ "Japanese",
                       RACE_1940==4 ~ "Chinese")
    ) |>
    mutate(
      # create indicator for migration status
      migrate10 = ifelse(COUNTYICP_1940!=COUNTYICP_1950, 1, 0),
      # create new individual-level id
      id = row_number()
    ) 
  return(wide_df)
}

clean_long_vars <- function(wide_df, ddi) {
  long_df <- wide_df |>
    # select(!starts_with("YEAR")) |> # redundant
    pivot_longer(
      cols = matches("_(1940|1950)$") & !contains("histid_"),
      names_to = c(".value", "YEAR"),
      names_pattern = "(.*)_(1940|1950)"
    ) |>
    set_ipums_var_attributes(var_info = ipumsr::read_ipums_ddi(ddi)) |>
    mutate(
      across(any_of("YEAR"), ~ as.integer(.)),
      across(any_of("MARST"), ~ifelse(. %in% 1:2, 1, 0), .names = "married"),
      across(any_of("NATIVITY"), ~ifelse(. == 5, 1, 0), .names = "foreign"),
      across(any_of("EDUC"), ~ifelse(. %in% 7:11, 1, 0), .names = "college"),
      across(any_of("EMPSTAT"), ~ifelse(. == 1, 1, 0), .names = "employed"),
      ## across(any_of("OCC1950"), ~ifelse(. %in% 979:999, NA), .names = "occupation"),
      OCC1950 = as_factor(OCC1950),
      across(any_of(c("OCC1950", "RACE", "SEX", "EDUCD", "CLASSWKR", "EMPSTAT")), as_factor)
    )

  return(long_df)
}

collect_county_stats <- function(ddi, inflator = 1.69) {
  # callback function to calculate county summary stats
  countystat_cb <- function(x, pos) {
    x |>
      filter(YEAR == 1940) |>
      ## mutate(INCWAGE = ifelse(INCWAGE %in% c(999999,999998), NA, INCWAGE * inflator)) |>
      rename(INCWAGE_1940 = INCWAGE) |> # var name for adjust_dollar_vars formating
      adjust_dollar_vars(inflator = inflator) |>
      group_by(STATEFIP, COUNTYICP) |>
      summarise(
        county_tot.salary   = sum(INCWAGE_adj_1940, na.rm=T),
        county_haswage.n    = sum(!is.na(INCWAGE_adj_1940)),
        county_pop          = n(),
        county_pop.white    = sum(RACE==1),
        county_pop.chinese  = sum(RACE==4),
        county_pop.japanese = sum(RACE==5),
        .groups = "drop"
      )
  }
  
  county_data <- read_ipums_micro_chunked(
    ddi,
    callback = IpumsDataFrameCallback$new(countystat_cb),
    chunk_size = 1e6,
    vars = c("YEAR", "STATEFIP", "COUNTYICP", "INCWAGE", "RACE"),
    verbose = TRUE
  ) |>
    group_by(STATEFIP, COUNTYICP) |>
    summarise(
      # hopefully mean of sample median is good aprx for median?
      county_med.salary   = sum(county_tot.salary, na.rm=T) / sum(county_haswage.n, na.rm=T),
      county_pop          = sum(county_pop),
      county_pop.white    = sum(county_pop.white),
      county_pct.white    = sum(county_pop.white) / sum(county_pop, na.rm=T),
      county_pop.chinese  = sum(county_pop.chinese),
      county_pct.chinese  = sum(county_pop.chinese) / sum(county_pop, na.rm=T),
      county_pop.japanese = sum(county_pop.japanese),
      county_pct.japanese = sum(county_pop.japanese) / sum(county_pop, na.rm=T),
      .groups = "drop"
    )

  return(county_data)
}
