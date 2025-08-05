#, Writes census microdata from an IPUMS ddi path to a local database
#,
#, @param ddi Path to an IPUMS DDI .xml file.
#, @param dbdir Path to a local duckdb database.
#,              A new file will be created if it doesnt exist already
#, @param tbl_name String name of the table in the database
#, @param chunk_size Number of rows to read in each chunk
#, @returns A name of the table where data is written
write_ipums_db <- function(ddi, dbdir, tbl_name, debug = FALSE, chunk_size = 1e6) {
  con <- dbConnect(duckdb(), dbdir = dbdir)
  
  # Remove previous version of the table if it exists
  if (tbl_name %in% dbListTables(con)) {
    dbRemoveTable(con, tbl_name)
  }

  write_chunk <- function(chunk, pos) {
    dbWriteTable(con, tbl_name, chunk, append = TRUE)
    if (debug == TRUE) {
      print(chunk)
    }
  }

  read_ipums_micro_chunked(
    ddi,
    callback = readr::SideEffectChunkCallback$new(write_chunk),
    chunk_size = chunk_size # observations to read per chunk
  )

  dbDisconnect(con)

  return(tbl_name)
}
