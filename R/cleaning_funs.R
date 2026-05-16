define_wage_sample <- function(mlp_sample) {
  mlp_sample |>
    pivot_wider(
      id_cols = any_of(c(
        "HIK", "id", "bypl_grp", "bpl_grp", "migrate10", "migrant", "sex_adj", "race_adj", "birthyr_adj",
        "internment_prob", "int_prob", "county_intprop", "intprob_AB", "intprob_full",
        "Earning_growth"
        )), 
      names_from = YEAR,
      values_from = any_of(c(
        "RACE", "BIRTHYR", "BPL", "SEX", "AGE", "EDUCD", "age_adj",
        "INCWAGE", "INCTOT", "INCBUSFM", "INCOTHER", "OCCSCORE", "OCC1950", "OCC",
        "EMPSTAT", "CLASSWKR", "LABFORCE", "employed", "selfemp", "wagewrk", "occ_grp",
        "IND1950", "FARM", "STATEFIP", "COUNTYICP", "HRSWORK1", "WKSWORK1",
        "county_pop", "county_med.salary", "county_pop.japanese", "county_pct.japanese"
      ))
    ) |>
    mutate(
      consistent_wage = ifelse((is.na(INCWAGE_1940) | is.na(INCWAGE_1950)), FALSE, TRUE),
      consistent_race = ifelse(RACE_1940 != RACE_1950 | is.na(RACE_1940), FALSE, TRUE),
      consistent_place = ifelse(is.na(COUNTYICP_1940)|is.na(COUNTYICP_1950), FALSE, TRUE),
      consistent_occ  = ifelse(is.na(OCC1950_1940) | is.na(OCC1950_1950), FALSE, TRUE),
      consistent_all  = ifelse(consistent_wage & consistent_race & consistent_place & consistent_occ, TRUE, FALSE)
    ) |>
    filter(consistent_all == TRUE) 
    ## mutate(
    ##   RACE = race_adj,
    ##   SEX  = sex_adj
    ## ) 
    ## ## select(-c("RACE_1940", "RACE_1950", "SEX_1940", "SEX_1950"))
}

clean_mlp <- function(wide_df, countystats, wra_data, ddi,
                      inflator = 1.6, methods = c("main", "county", "AB", "full")) {
  wide_df |>
    clean_wide_vars(ddi = ddi, inflator = inflator) |>
    predict_internment(wra_data, ddi, method = methods) |>
    mutate(
      Earning_growth =
        (INCWAGE_1950 - INCWAGE_1940) /
        INCWAGE_1940
    ) |>
    # pivot to longer and clean variables by year
    clean_long_vars(ddi = ddi) |>
    left_join(countystats, by = c("STATEFIP", "COUNTYICP")) |>
    ## select(-c("n_census", "n_interned", "INCWAGE")) |>
    mutate(
      age_adj = as.integer(YEAR) - birthyr_adj,
      employed = as_factor(employed),
      migrant = as_factor(migrate10)
    ) 
}

collect_mlp <- function(db, table,
                           vars = c("RACE", "BIRTHYR", "BPL",
                                    "INCWAGE", "INCTOT", "INCBUSFM", "INCOTHER",
                                    "STATEFIP", "COUNTYICP", 
                                    "AGE", "SEX", "EDUCD",
                                    "CLASSWKR", "EMPSTAT", "LABFORCE",
                                    "FARM", "IND1950",
                                    "HRSWORK1", "WKSWORK1",
                                    "OCC1950", "OCCSCORE",
                                    "EMPSTAT")) {
  # connect to database on file
  con <- dbConnect(duckdb(), db)
  # disconnect from database on function exit
  on.exit(dbDisconnect(con))

  # save table as temporary object
  db_tbl <- tbl(con, table)

  # collect HIKs of interest from 1940 data
  hiks_to_keep <- db_tbl |>
    ## filter(YEAR == 1940, RACE %in% c(4,5)) |>
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

clean_wide_vars <- function(wide_df, ddi, inflator = 1.69) {
  # ten-year inflation rate from bls.gov/data/inflation_calculator.htm
  wide_df <- wide_df |>
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
    ) |>
  mutate(
    bpl_grp = group_bpl(BPL_1940),
    byr_grp = group_birthyr(BIRTHYR_1940)
  )

  return(wide_df)
}

occ_category <- function(OCC1950) {
  occ_grp <- case_when(
    OCC1950 %in% 000:099 ~ "Professional, Technical",
    OCC1950 == 100 ~ "Farmers (owners and tenants)",
    OCC1950 == 123 ~ "Farmer managers",
    OCC1950 %in% 200:290 ~ "administration, management",
    OCC1950 %in% 300:490 ~ "clerical, sales",
    OCC1950 %in% 500:690 ~ "craftsmen, operatives",
    OCC1950 %in% 700:790 ~ "Service Workers",
    OCC1950 %in% 800:840 ~ "Farm laborers",
    OCC1950 %in% 910:970 ~ "Laborers"
  )
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
      across(any_of("EDUC"), ~ifelse(. %in% 7:11, 1, 0), .names = "edu_college"),
      across(any_of("EDUC"), ~ifelse(. %in% 6:11, 1, 0), .names = "edu_hs"),
      across(any_of("EMPSTAT"), ~ifelse(. == 1, 1, 0), .names = "employed"),
      across(any_of("CLASSWKR"), ~ifelse(. == 1, 1, 0), .names = "selfemp"),
      across(any_of("CLASSWKR"), ~ifelse(. == 1, 2, 0), .names = "wagewrk"),
      across(any_of("CLASSWKRD"), ~ifelse(. == 29, 2, 0), .names = "familywrk"),
      ## across(any_of("OCC1950"), ~ifelse(. %in% 979:999, NA), .names = "occupation"),
      occ_grp = occ_category(OCC1950),
      OCC1950 = as_factor(OCC1950),
      across(
        any_of(c("OCC1950", "IND1950", "RACE", "SEX", "EDUCD", "CLASSWKR", "EMPSTAT", "FARM")),
        as_factor
        )
    )

  return(long_df)
}

collect_county_income <- function(ddi, inflator = 1.69,
                                  dollar_vars = c("INCWAGE"),
                                  group_vars = c("YEAR", "STATEFIP", "COUNTYICP")) {

  # callback function to calculate county summary stats
  cb <- function(x, pos) {
    x |>
      na_dollar_vals() |>
      mutate(across(all_of(dollar_vars),
                    ~ ifelse(YEAR == 1940, .x * inflator, .x))) |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        across(all_of(dollar_vars),
               list(total = ~ sum(.x, na.rm = TRUE),
                    nwith = ~ sum(!is.na(.x))),
               .names = "{.fn}_{.col}"),
        .groups = "drop"
      )
  }

  chunk_data <- read_ipums_micro_chunked(
    ddi,
    callback = IpumsDataFrameCallback$new(cb),
    chunk_size = 1e6,
    vars = any_of(c(group_vars, dollar_vars)),
    verbose = TRUE
  )

  county_data <- chunk_data|>
    group_by(across(any_of(group_vars))) |>
    summarise(
      across(any_of(starts_with("total_")),
             ~ sum(.x, na.rm = TRUE),
             .names = "{.col}"),
      across(any_of(starts_with("nwith")),
             ~ sum(.x, na.rm = TRUE),
             .names = "{.col}"),
      .groups = "drop") |>
    pivot_longer(
      cols = starts_with(c("total_", "nwith_")),
      names_to = c(".value", "variable"),
      names_pattern = "(total|nwith)_(.*)"
    ) |>
    pivot_wider(
      names_from = c(variable),
      values_from = c(total, nwith),
      names_glue = "{.value}_{variable}",
      values_fn = sum) |>
    mutate(
      mean_INCWAGE = total_INCWAGE / nwith_INCWAGE,
    )

  return(county_data)
}
