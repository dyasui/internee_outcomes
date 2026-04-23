collect_census_demographics <- function(ddi) {
  summary_cb <- function(x, pos) {
    x |>
      filter(YEAR==1940) |>
      count(STATEFIP, COUNTYICP, RACE, SEX, BIRTHYR, BPL) |>
      mutate(n_chunk = n)
  }

  chunk_data <- read_ipums_micro_chunked(
    ddi,
    callback = IpumsDataFrameCallback$new(summary_cb),
    chunk_size = 1e7,
    vars = c("YEAR", "STATEFIP", "COUNTYICP", "RACE", "SEX" , "BIRTHYR" , "BPL"),
    verbose = TRUE
  ) |>
    group_by(STATEFIP, COUNTYICP, RACE, SEX, BIRTHYR, BPL) |>
    summarise(
      n = sum(n_chunk),
      .groups = "drop"
    )
}
