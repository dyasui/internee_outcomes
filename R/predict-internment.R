library(ipumsr)
library(dbplyr)
library(duckdb)
# data extract file
db <- dbConnect(duckdb(), dbdir = "data/ipums_db.duckdb")
tbl = "ipums_microdata"

dt40 <- tbl(db, tbl) |>
  filter(YEAR == 1940, RACE == 5) |>
  select(STATEFIP, COUNTYICP, SEX, BIRTHYR, BPL)

JApop_groups <- dt40 |>
  count(STATEFIP, COUNTYICP, SEX, BIRTHYR, BPL) |>
  collect() |>
  mutate(
    # proportion in each demographic group
    p = n / sum(n),
    # state-county geography code based on NHGIS
    NHGISST = str_pad(string = STATEFIP*10, width = 3, side = "left", pad = "0"),
    NHGISCTY = str_pad(COUNTYICP, width = 4, side = "left", pad = "0") )

# save results
readr::write_csv(JApop_groups, file = "data/ja_pop_groups.csv")
