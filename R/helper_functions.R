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

is_evac_county <- function(STATEFIP, COUNTYICP) {
  case_when(
    STATEFIP==6 ~ 1, # all of california evacuation zone
    (STATEFIP==4 & COUNTYICP %in% c(30, 130, 190, 230, 270)) ~ 1, # Arizona evacuation zones
    (STATEFIP==41 & COUNTYICP %in% c(30, 50, 70, 90, 110, 190, 270, 290, 330, 390, 410, 430, 470, 510, 530, 570, 670, 690, 730, 770)) ~ 1, # Oregon evacuation zones
    (STATEFIP==53 & COUNTYICP %in% c(50, 70, 90, 150, 270, 310, 330, 350, 370, 390, 410, 430, 470, 510, 530, 570, 590, 610, 690, 730, 770)) ~ 1, # Washington evacuation zones
    .default = 0
  )
}
