# open database connection with duckdb
library(ipumsr)
library(dplyr)
library(dbplyr)
library(duckdb)
database <- dbConnect(duckdb(), dbdir = "data/ipums_db.duckdb")
tbl <- "ipums_microdata"
# bring needed microdata into a tibble for joining
d1 <- tbl(database, tbl) |>
  filter(YEAR == 1940) # decade start year
d2 <- tbl(database, tbl) |>
  filter(YEAR == 1950) # decade end year

# histid to be used in appropriate cw file
cw <- tbl(database, "mlp_crosswalks")

# join histids to selected data from year 1
l1 <- cw |>
  inner_join(d1, by = c("histid_1940" = "HISTID")) |>
  collect()

# join histids to selected data from year 2
l2 <- cw |>
  inner_join(d2, by = c("histid_1950" = "HISTID")) |>
  collect()

# join all selected data between years by crosswalked histids
results <- bind_rows(l1, l2) |>
  # get rid of leftover linking and technical varibles
  select(!c("step", "SAMPLE", "SERIAL", "HHWT", "PERWT",
            "PERNUM", "GQ", "VERSIONHIST", "CITY", "ENUMDIST"))

# generate new unique ids for each linked person
histid_pairs <- results |>
  count(histid_1940, histid_1950) |>
  filter(n>1) |> # multiple matches
  mutate(id = row_number()) |>
  select(!n)

results <- results |>
  left_join(histid_pairs, by = c("histid_1940", "histid_1950")) |>
  filter(!is.na(id)) |>
  select(!c("histid_1940", "histid_1950")) |>
  relocate(id)

# clean up other variables
results <- results |>
  mutate(
    # replace missing values with NAs and adjust all dollar amounts to 1940 levels
    across(c(INCWAGE, INCTOT, INCBUSFM),
           ~ case_when(.x %in% c(999999,999998) ~ NA,
                             YEAR == 1950 ~ .x * 0.59, # deflated dollars to 1940 standards
                             .default = .x)
           ),
    married = ifelse(MARST %in% 1:2, 1, 0),
    foreign = ifelse(NATIVITY == 5, 1, 0),
    college = ifelse(EDUC %in% 7:11, 1, 0),
    employed = ifelse(EMPSTAT == 1, 1, 0),
    female = ifelse(SEX==2, 1, 0),
    OCC1950 = ifelse(OCC1950 %in% 979:999, NA, OCC1950),
    age = YEAR - BIRTHYR,
    female = ifelse(SEX == 2, 1, 0),
    wage = ifelse(is.na(INCWAGE), 0, INCWAGE),
    lnwage = log(INCWAGE),
    citizen = ifelse(CITIZEN %in% c(0, 1, 2), 1, 0),
    homestate = ifelse(BPL == STATEFIP, 1, 0),
    generation = case_when(
      NATIVITY == 5 ~ "first", # foriegn born
      NATIVITY %in% 2:4 ~ "second", # both or either parent foriegn born
      NATIVITY == 1 & (NATIVITY_POP %in% 2:4 | NATIVITY_MOM %in% 2:4) ~ "third",
      ## .default == NA
    )
  )

results_wide <- results |>
  select(id, YEAR, STATEFIP, COUNTYICP) |>
  pivot_wider(id_cols = id, names_from = YEAR, names_sep = "_", values_from = c(STATEFIP, COUNTYICP))

results <- left_join(results, results_wide, by = "id")

readr::write_csv(results, file = "data/linked_census_sample.csv")
