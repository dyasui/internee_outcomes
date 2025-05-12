# open database connection with duckdb
library(ipumsr)
library(dplyr)
library(dbplyr)
library(duckdb)
database <- dbConnect(duckdb(), dbdir = "data/ipums_db.duckdb")
# data extract file
extract <- "data/fullcount_census/usa_00124.xml"
ddi <- read_ipums_ddi(extract)
# table name in database
tbl <- "ipums_microdata"
# crosswalk files for each adjacent census year pair
cw45_csv <- "data/fullcount_census/mlp_1940_1950_v1_2_csv/mlp_1940_1950_v1.2.csv"

# read in ipums extract to database in chunks
read_ipums_micro_chunked(
  ddi, # path to ddi .xml file in same location as data download
  callback = readr::SideEffectChunkCallback$new(
    function(x, pos) {
      if (pos == 1) {
        dbWriteTable(database, tbl, x, overwrite = TRUE)
      } else {
        dbWriteTable(database, tbl, x, overwrite = FALSE, append = TRUE)
      }
    }
  ),
  chunk_size = 1e7 # observations to read per chunk
)

# read crosswalk csv files into database
duckdb_read_csv(database, "mlp_crosswalks", cw45_csv)
