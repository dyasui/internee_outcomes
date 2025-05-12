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
    across(c(INCWAGE, INCTOT, INCBUSFM)),
    ~ case_when(
      . %in% c(999999,999998) ~ NA,
      YEAR == 1950 ~ . * 0.59, # deflated dollars to 1940 standards
      .default = .
    ),
    married = ifelse(MARST %in% 1:2, 1, 0),
    foreign = ifelse(NATIVITY == 5, 1, 0),
    college = ifelse(EDUC %in% 7:11, 1, 0),
    employed = ifelse(EMPSTAT == 1, 1, 0),
    female = ifelse(SEX==2, 1, 0),
    OCC1950 = ifelse(OCC1950 %in% 979:999, NA, OCC1950),
  )

readr::write_csv(results, file = "data/linked_census_sample.csv")
