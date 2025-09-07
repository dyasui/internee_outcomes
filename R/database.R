#' Writes census microdata from an IPUMS ddi path to a local database
#'
#' @param ddi Path to an IPUMS DDI .xml file.
#' @param dbdir Path to a local duckdb database.
#'              A new file will be created if it doesnt exist already
#' @param tbl_name String name of the table in the database
#' @param chunk_size Number of rows to read in each chunk
#' @param replace Logical. If TRUE, replace existing table. If FALSE, skip if table exists.
#' @param debug Logical. If TRUE, print chunk information during processing
#' @returns A name of the table where data is written
write_ipums_db <- function(ddi, dbdir, tbl_name,
                           debug = FALSE, chunk_size = 1e6,
                           replace = FALSE) {
  library(duckdb)
  library(DBI)
  library(ipumsr)
  
  con <- dbConnect(duckdb(), dbdir = dbdir)
  
  # Check if table already exists
  table_exists <- tbl_name %in% dbListTables(con)
  
  if (table_exists && !replace) {
    message("Table '", tbl_name, "' already exists. Use replace = TRUE to overwrite.")
    dbDisconnect(con)
    return(tbl_name)
  }
  
  if (table_exists && replace) {
    message("Replacing existing table '", tbl_name, "'")
    dbRemoveTable(con, tbl_name)
  } else if (!table_exists) {
    message("Creating new table '", tbl_name, "'")
  }
  
  # Track progress
  total_rows <- 0
  
  write_chunk <- function(chunk, pos) {
    dbWriteTable(con, tbl_name, chunk, append = TRUE)
    total_rows <<- total_rows + nrow(chunk)
    
    if (debug) {
      message("Processed chunk at position ", pos, " with ", nrow(chunk), " rows. Total: ", total_rows)
    }
  }

  # Read and write data in chunks
  tryCatch({
    read_ipums_micro_chunked(
      ddi,
      callback = readr::SideEffectChunkCallback$new(write_chunk),
      chunk_size = chunk_size
    )
    
    message("Successfully wrote ", total_rows, " rows to table '", tbl_name, "'")
    
  }, error = function(e) {
    message("Error writing to database: ", e$message)
    if (dbExistsTable(con, tbl_name)) {
      dbRemoveTable(con, tbl_name)
      message("Removed incomplete table due to error")
    }
    stop(e)
  })

  dbDisconnect(con)
  return(tbl_name)
}
