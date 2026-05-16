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
      "West Coast (AZ, CA, OR, WA)",
      "Rest of Continental US",
      "Japan",
      "Alaska, Hawaii, other"
    )
  )
}

is_evac_county <- function(STATEFIP, COUNTYICP) {
  case_when(
    ! STATEFIP %in% c(4, 6, 41, 53) ~ FALSE,
    STATEFIP==6 ~ 1, # all of california evacuation zone
    internment_location(STATEFIP, COUNTYICP) == "Southern AZ" ~ TRUE, # Arizona evacuation zones
    internment_location(STATEFIP, COUNTYICP) == "Northern AZ" ~ FALSE, # Arizona evacuation zones
    internment_location(STATEFIP, COUNTYICP) %in%  c("Northwest OR", "Southern OR", "Fall Line OR", "Clackamas, OR", "Hood River, OR", "Marion, OR", "Multnomah, OR", "Washington, OR") ~ TRUE, # Oregon
    internment_location(STATEFIP, COUNTYICP) %in% c("Clallam-Jefferson-Island-San-Juan, WA", "Skagit-Snohomish-Whatcom, WA", "Skamania-Kilikatat, WA", "Lewis-Mason-Thurston, WA", "Grays-Harbor-Pacific-Wahkiakum, WA", "Clark, WA", "Cowlitz, WA", "King, WA", "Kitsap, WA", "Pierce, WA", "Yakima, WA") ~ TRUE,
    .default = FALSE
  )
}

internment_location <- function(STATEFIP, COUNTYICP) {
  case_when(
    ! STATEFIP %in% c(4, 6, 41, 53) ~ as_factor(STATEFIP),
    STATEFIP == 4 & COUNTYICP %in% c(30, 70, 90, 110, 130, 190, 210, 230, 250, 270) ~ "Southern AZ",
    STATEFIP == 4 & COUNTYICP %in% c(10, 50, 150, 170) ~ "Northern AZ",
    STATEFIP == 6 & COUNTYICP %in% c(30, 50, 90, 170, 270, 430, 510) ~ "South Sierras CA",
    STATEFIP == 6 & COUNTYICP %in% c(150, 210, 230, 350, 490, 630, 890, 930, 1030, 1050) ~ "Northern CA",
    STATEFIP == 6 & COUNTYICP %in% c(330, 410, 450, 550, 970) ~ "North Coast CA",
    STATEFIP == 6 & COUNTYICP %in% c(570, 910, 1150) ~ "Yuba CA",
    STATEFIP == 6 & COUNTYICP ==   10 ~ "Alameda, CA",
    STATEFIP == 6 & COUNTYICP ==   70 ~ "Butte, CA",
    STATEFIP == 6 & COUNTYICP ==  110 ~ "Colusa, CA",
    STATEFIP == 6 & COUNTYICP ==  130 ~ "Contra Costa, CA",
    STATEFIP == 6 & COUNTYICP ==  190 ~ "Fresno, CA",
    STATEFIP == 6 & COUNTYICP ==  250 ~ "Imperial, CA",
    STATEFIP == 6 & COUNTYICP ==  290 ~ "Kern, CA",
    STATEFIP == 6 & COUNTYICP ==  310 ~ "Kings, CA",
    STATEFIP == 6 & COUNTYICP ==  370 ~ "Los Angeles, CA",
    STATEFIP == 6 & COUNTYICP ==  390 ~ "Madera, CA",
    STATEFIP == 6 & COUNTYICP ==  470 ~ "Merced, CA",
    STATEFIP == 6 & COUNTYICP ==  530 ~ "Monterey, CA",
    STATEFIP == 6 & COUNTYICP ==  590 ~ "Orange, CA",
    STATEFIP == 6 & COUNTYICP ==  610 ~ "Placer, CA",
    STATEFIP == 6 & COUNTYICP ==  650 ~ "Riverside, CA",
    STATEFIP == 6 & COUNTYICP ==  670 ~ "Sacramento, CA",
    STATEFIP == 6 & COUNTYICP ==  690 ~ "San Benito, CA",
    STATEFIP == 6 & COUNTYICP ==  710 ~ "San Bernardino, CA",
    STATEFIP == 6 & COUNTYICP ==  730 ~ "San Diego, CA",
    STATEFIP == 6 & COUNTYICP ==  750 ~ "San Francisco, CA",
    STATEFIP == 6 & COUNTYICP ==  770 ~ "San Joaquin, CA",
    STATEFIP == 6 & COUNTYICP ==  790 ~ "San Luis Obispo, CA",
    STATEFIP == 6 & COUNTYICP ==  810 ~ "San Mateo, CA",
    STATEFIP == 6 & COUNTYICP ==  830 ~ "Santa Barbara, CA",
    STATEFIP == 6 & COUNTYICP ==  850 ~ "Santa Clara, CA",
    STATEFIP == 6 & COUNTYICP ==  870 ~ "Santa Cruz, CA",
    STATEFIP == 6 & COUNTYICP ==  950 ~ "Solano, CA",
    STATEFIP == 6 & COUNTYICP ==  970 ~ "Sonoma, CA",
    STATEFIP == 6 & COUNTYICP ==  990 ~ "Stanislaus, CA",
    STATEFIP == 6 & COUNTYICP == 1010 ~ "Sutter, CA",
    STATEFIP == 6 & COUNTYICP == 1070 ~ "Tulare, CA",
    STATEFIP == 6 & COUNTYICP == 1110 ~ "Ventura, CA",
    STATEFIP == 6 & COUNTYICP == 1130 ~ "Yolo, CA",
    STATEFIP == 41 & COUNTYICP %in% c(30, 70, 90, 410, 530, 570, 710) ~ "Northwest OR",
    STATEFIP == 41 & COUNTYICP %in% c(10, 130, 210, 230, 250, 370, 450, 490, 550, 590, 610, 630, 690) ~ "Eastern Oregon",
    STATEFIP == 41 & COUNTYICP %in% c(110, 150, 190, 330) ~ "Southern OR",
    STATEFIP == 41 & COUNTYICP %in% c(170, 310, 350, 650) ~ "Fall Line OR",
    STATEFIP == 41 & COUNTYICP == 50 ~ "Clackamas, OR",
    STATEFIP == 41 & COUNTYICP == 270 ~ "Hood River, OR",
    STATEFIP == 41 & COUNTYICP == 470 ~ "Marion, OR",
    STATEFIP == 41 & COUNTYICP == 510 ~ "Multnomah, OR",
    STATEFIP == 41 & COUNTYICP == 670 ~ "Washington, OR",
    STATEFIP == 53 & COUNTYICP %in% c(10, 30, 50, 130, 210, 230, 710, 750) ~ "South East WA",
    STATEFIP == 53 & COUNTYICP %in% c(70, 170, 190, 250, 430, 470, 510, 650) ~ "North East WA",
    STATEFIP == 53 & COUNTYICP %in% c(90, 290, 310, 550) ~ "Clallam-Jefferson-Island-San-Juan, WA",
    STATEFIP == 53 & COUNTYICP %in% c(570, 610, 730) ~ "Skagit-Snohomish-Whatcom, WA",
    STATEFIP == 53 & COUNTYICP %in% c(390, 590) ~ "Skamania-Klikatat, WA",
    STATEFIP == 53 & COUNTYICP %in% c(410, 450, 670) ~ "Lewis-Mason-Thurston, WA",
    STATEFIP == 53 & COUNTYICP %in% c(270, 490, 690) ~ "Grays-Harbor-Pacific-Wahkiakum, WA",
    STATEFIP == 53 & COUNTYICP == 110 ~ "Clark, WA",
    STATEFIP == 53 & COUNTYICP == 150 ~ "Cowlitz, WA",
    STATEFIP == 53 & COUNTYICP == 330 ~ "King, WA",
    STATEFIP == 53 & COUNTYICP == 350 ~ "Kitsap, WA",
    STATEFIP == 53 & COUNTYICP == 530 ~ "Pierce, WA",
    STATEFIP == 53 & COUNTYICP == 630 ~ "Spokane, WA",
    STATEFIP == 53 & COUNTYICP == 770 ~ "Yakima, WA",
  )
}

na_dollar_vals <- function(df) {
  cols <- grep("(INCWAGE|INCTOT|INCBUSFM|INCOTHER)$",
               names(df), value = TRUE)

  df |>
    mutate(
      across(any_of("INCWAGE"), ~ ifelse(.x %in% 999998:999999, NA, .x)),
      across(any_of("INCTOT"), ~ ifelse(.x %in% 9999998:9999999, NA, .x)),
      across(any_of(c("INCBUSFM", "INCOTHER")), ~ ifelse(.x %in% 99998:99999, NA, .x))
    )

}

get_nhgis_codes <- function(shp) {
  library(dplyr)
  library(ipumsr)
  nhgis_codes <- read_ipums_sf(shp, file_select = matches("us_county_1950")) |>
    sf::st_drop_geometry() |>
    mutate(STATENAM = case_when(
           STATENAM == "Alaska Territory" ~ "Alaska",
           STATENAM == "Hawaii Territory" ~ "Hawaii",
           .default = STATENAM
           )) |>
    select(STATENAM, NHGISNAM ,NHGISST, NHGISCTY, ICPSRFIP) |>
    mutate(STATEFIP = floor(as.numeric(NHGISST) /10), # floor deals with historic AK, HI, territories
           COUNTYICP = as.numeric(NHGISCTY))
  return(nhgis_codes)
}

wra_counties <- function(nhgis, addr) {
  library(tidyverse)
  # read nhgiscodes from shapefile
  wra_counties <- read_csv(addr) |>
    select(prev_address, state, county, city)
  join_counties <- inner_join(
    wra_counties, nhgis,
    by = c("state" = "STATENAM", "county" = "NHGISNAM")
  )
  return(join_counties)
}
