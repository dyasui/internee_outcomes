write_cw_db <- function(db_path = "data/ipums_db.duckdb", cw_file = "data/mlp_1940_1950_v1_2_csv/mlp_1940_1950_v1.2.csv", tbl_name = "mlp_crosswalks") {
  # open duckdb connection to local file
  conn <- dbConnect(duckdb(), dbdir = db_path)
  # read crosswalk csv files into database if not already written
  if (!dbExistsTable(conn, tbl_name)) {
    duckdb_read_csv(conn, tbl_name, cw_file)
    dbDisconnect(conn)
  }
  # disconnect from database after reading
  dbDisconnect(conn)
  # return path of database for targets workflow
  tbl_name
}

test_dt <- read_ipums_micro_yield(ddi = "data/fullcount_census/usa_00128.xml")$yield(1e7)
print(object.size(test_dt), units = "auto")

write_ipums_db <- function(ddi, db_path = "data/ipums_db.duckdb", tbl_name) {
  conn <- dbConnect(duckdb(), dbdir = db_path)

  writedb_cb <- function(x,pos) {
    if (pos == 1) {
      dbWriteTable(conn, tbl_name, x, overwrite = TRUE)
    } else {
      data_filtered <- x |>
        filter(RACE %in% c(4,5))
      dbWriteTable(conn, tbl_name, data_filtered, overwrite = FALSE, append = TRUE)
    }
  }

  read_ipums_micro_chunked(
    ddi,
    callback = readr::SideEffectChunkCallback$new(writedb_cb),
    verbose = T,
    chunk_size = 1e7 # observations to read per chunk
  )

  dbDisconnect(conn)

  tbl_name
}

link_mlp <- function(db_path = "data/ipums_db.duckdb", cw_tbl = "mlp_crosswalks",
                     fc_tbl = "fullcount") {
  # open crosswalk database connection
  database <- dbConnect(duckdb(), dbdir = db_path)

  # unnecessary variables
  drop_vars <- c("step","SAMPLE","HHWT","PERWT","VERSIONHIST")

  # bring needed microdata into a tibble for joining
  d1 <- tbl(database, fc_tbl) |>
    select(-any_of(drop_vars)) |>
    filter(
      YEAR == 1940, # decade start year
      RACE %in% c(4,5)
    ) 
  d2 <- tbl(database, fc_tbl) |>
    select(-any_of(drop_vars)) |>
    filter(
      YEAR == 1950, # decade end year
      RACE %in% c(4,5)
    ) 

  # histid to be used in appropriate cw file
  cw <- tbl(database, cw_tbl)

  # join histids to selected data from year 1
  l1 <- cw |>
    inner_join(d1, by = c("histid_1940" = "HISTID")) |>
    collect()
  
  # join histids to selected data from year 2
  l2 <- cw |>
    inner_join(d2, by = c("histid_1950" = "HISTID")) |>
    collect()

  linked_data <- l1 |>
    inner_join(
      l2,
      by = c("histid_1940","histid_1950"),
      suffix = c("_1940", "_1950")
    )

  # disconnect from duckdb connection
  dbDisconnect(database)

  # return all observations linked between 1940 and 1950
  return(linked_data)
}
