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
  db_path
}

link_mlp <- function(ddi,
                     cw_db = "data/ipums_db.duckdb", tbl = "mlp_crosswalks",
                     filter_race = c(4,5)) {
  # open crosswalk database connection
  database <- dbConnect(duckdb(), dbdir = cw_db)
  cw <- tbl(database, "mlp_crosswalks")

  link1940_cb <- function(x, pos) {
    x |>
      filter( YEAR=="1940", RACE %in% {{ filter_race }} ) |>
      left_join( cw , by = c("HISTID" = "histid_1940"), copy = T)
  }

  link40_dt <- read_ipums_micro_chunked(
    ddi,
    callback = IpumsDataFrameCallback$new(link1940_cb),
    chunk_size = 1e5,
    verbose = T
  ) |>
    bind_rows()

  link1950_cb <- function(x,pos) {
    x |>
      filter( YEAR==1950, RACE %in% {{ filter_race }} ) |>
      left_join( cw, by = c("HISTID" = "histid_1940"), copy = T )
  }

  link50_dt <- read_ipums_micro_chunked(
    ddi,
    callback = IpumsDataFrameCallback$new(link1950_cb),
    chunk_size = 1e5,
    verbose = T
  ) |>
    bind_rows()

  all_rows <- bind_rows(link40_dt, link50_dt)

  dbDisconnect()

  return(all_rows)
}
