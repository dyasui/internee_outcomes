group_birthyr <- function(birthyr, from = 1860, to = 1942, width = 3) {
  breaks <- seq(from, to, by = width)
  starts <- breaks
  ends <- pmin(breaks + width - 1, to)
  labels <- paste0(starts, "-", ends)

  cut(
    birthyr,
    breaks = c(starts, to + 1),
    labels = labels,
    right = FALSE,
    include.lowest = TRUE
  )
}

group_bpl <- function(bpl) {
  grp_num <- case_when(
    bpl %in% c(4, 6, 41, 53) ~ 1,
    bpl %in% c(1, 5, 8:13, 16:40, 42:51, 54:56, 99) ~ 2,
    bpl %in% 501 ~ 3,
    .default = 4
  )

  factor(
    grp_num,
    levels = 1:4,
    labels = c(
      "AZ, CA, OR, WA",
      "Rest of continental US",
      "Japan",
      "Alaska, Hawaii, other"
    )
  )
}
