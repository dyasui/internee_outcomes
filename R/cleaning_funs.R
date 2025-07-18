clean_sample <- function(wide_df, ddi, inflator = 1.69) {
  # ten-year inflation rate from bls.gov/data/inflation_calculator.htm
  wide_df <- wide_df |>
    # deal with inconsistencies in reported birthyear across censuses by averaging
    mutate(
      birthyr_avg = (BIRTHYR_1950 + BIRTHYR_1940) / 2,
      age_1940 = 1940 - birthyr_avg,
      age_1950 = 1950 - birthyr_avg
    ) |>
    # base time-invariant demographics on 1940 response
    mutate(
      sex = ifelse(SEX_1940==1, "Male", "Female"),
      race = case_when(RACE_1940==5 ~ "Japanese",
                       RACE_1940==4 ~ "Chinese")
    ) |>
    # adjust dollar amounts for inflation and missing values
    mutate(
      # remove NA values and convert 1940 incomes to 1950 dollars
      salary_1940 = ifelse(INCWAGE_1940 %in% c(999999,999998), NA, INCWAGE_1940 * inflator ),
      salary_1950 = case_when(INCWAGE_1950 %in% c(999999,999998) ~ NA,
                              INCWAGE_1950 > (5001 * inflator) ~ 5001 * inflator,
                              .default = INCWAGE_1950),
      income_bsfm = case_when(INCBUSFM_1950 %in% c(99999, 99998) ~ NA,
                                .default = INCBUSFM_1950),
      income_tot = case_when(INCTOT_1950 %in% c(9999999, 9999998) ~ NA,
                             .default = INCTOT_1950),
      income_oth = case_when(INCOTHER_1950 %in% c(99999, 99998) ~ NA,
                             .default = INCOTHER_1950)
    ) |>
    mutate(
      # create indicator for migration status
      migrate10 = ifelse(COUNTYICP_1940!=COUNTYICP_1950, 1, 0),
      # create new individual-level id
      id = row_number()
    ) 

  long_df <- wide_df |>
    select(!starts_with("YEAR")) |> # redundant
    pivot_longer(
      cols = matches("_(1940|1950)$") & !contains("histid_"),
      names_to = c(".value", "YEAR"),
      names_pattern = "(.*)_(1940|1950)"
    ) |>
    set_ipums_var_attributes(ipumsr::read_ipums_ddi(ddi)) |>
    mutate(
      YEAR = as.integer(YEAR),
      married = ifelse(MARST %in% 1:2, 1, 0),
      foreign = ifelse(NATIVITY == 5, 1, 0),
      college = ifelse(EDUC %in% 7:11, 1, 0),
      employed = ifelse(EMPSTAT == 1, 1, 0),
      generation = case_when(
        NATIVITY == 5 ~ "first-generation", # foriegn born
        NATIVITY %in% 2:4 ~ "second-generation", # both or either parent foriegn born
        NATIVITY == 1 & (FBPL %in% 1:120 | MBPL %in% 1:120) ~ "third-generation",
        .default = NA
      ),
      yearsschool = case_when(
        as_factor(EDUCD) == "Missing" ~ NA_integer_,
        as_factor(EDUCD) == "No schooling completed" ~ 0,
        as_factor(EDUCD) == "Kindergarten" ~ 0.5,
        as_factor(EDUCD) == "Grade 1" ~ 1,
        as_factor(EDUCD) == "Grade 2" ~ 2,
        as_factor(EDUCD) == "Grade 3" ~ 3,
        as_factor(EDUCD) == "Grade 4" ~ 4,
        as_factor(EDUCD) == "Grade 5" ~ 5,
        as_factor(EDUCD) == "Grade 6" ~ 6,
        as_factor(EDUCD) == "Grade 7" ~ 7,
        as_factor(EDUCD) == "Grade 8" ~ 8,
        as_factor(EDUCD) == "Grade 9" ~ 9,
        as_factor(EDUCD) == "Grade 10" ~ 10,
        as_factor(EDUCD) == "Grade 11" ~ 11,
        as_factor(EDUCD) == "Grade 12" ~ 12,
        as_factor(EDUCD) == "1 year of college" ~ 13,
        as_factor(EDUCD) == "2 years of college" ~ 14,
        as_factor(EDUCD) == "3 years of college" ~ 15,
        as_factor(EDUCD) == "4 years of college" ~ 16,
        as_factor(EDUCD) == "5 years of college" ~ 17,
        as_factor(EDUCD) == "6 years of college" ~ 18,
        as_factor(EDUCD) == "7 years of college" ~ 19,
        as_factor(EDUCD) == "8+ years of college" ~ 20
        ),
      occupation = ifelse(OCC1950 %in% 979:999, NA, as_factor(OCC1950)),
      county_ez = is_evac_county(STATEFIP, COUNTYICP)
    )
    

  return(long_df)
}

collect_county_stats <- function(ddi, inflator = 1.69) {
  # callback function to calculate county summary stats
  countystat_cb <- function(x, pos) {
    x |>
      filter(YEAR == 1940) |>
      mutate(
        INCWAGE = ifelse(INCWAGE %in% c(999999,999998), NA, INCWAGE * inflator)
      ) |>
      group_by(STATEFIP, COUNTYICP) |>
      summarise(
        county_med.salary   = median(INCWAGE, na.rm=T),
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
      county_med.salary   = mean(county_med.salary, na.rm=T),
      county_pop          = sum(county_pop),
      county_pop.white    = sum(county_pop.white),
      county_pct.white    = sum(county_pop.white) / n(),
      county_pop.chinese  = sum(county_pop.chinese),
      county_pct.chinese  = sum(county_pop.chinese) / n(),
      county_pop.japanese = sum(county_pop.japanese),
      county_pct.japanese = sum(county_pop.japanese) / n(),
      .groups = "drop"
    )
}
