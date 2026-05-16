collect_county_demographics <- function(data) {
  library(ipumsr)

  pop_data <- data |>
    ungroup() |>
    mutate(race_group = case_when(RACE == 5 ~ "japn", RACE == 4 ~ "chin", .default = "other")) |>
    group_by(STATEFIP, COUNTYICP, race_group) |>
    summarise(pop = sum(n_fc, na.rm = TRUE)) |>
    pivot_wider(id_cols = c("STATEFIP", "COUNTYICP"),
                names_prefix = "pop_",
                names_from = race_group,
                values_from = c("pop")) |>
    mutate(across(all_of(c("pop_chin", "pop_japn")), ~ ifelse(is.na(.x), 0, .x))) |>
    mutate(pop_tot = pop_japn + pop_chin + pop_other) |>
    select(STATEFIP, COUNTYICP, pop_tot, pop_japn, pop_chin)

  int_data <- data |>
    ungroup() |>
    group_by(STATEFIP, COUNTYICP) |>
    summarise(internees = sum(n_wra, na.rm = TRUE)) |>
    mutate(evac_zone = is_evac_county(STATEFIP, COUNTYICP),
           intern_location = internment_location(STATEFIP, COUNTYICP))

  result <- full_join(pop_data, int_data, by = c("STATEFIP", "COUNTYICP")) |>
    mutate(int_prop_tot = internees / pop_tot,
           int_prop_japn = internees / pop_japn)

  return(result)
}

collect_county_shp <- function(shp_file) {
  library(sf)
  # read shapefiles and append FIPS and ICPSR codes
  shp <- read_ipums_sf(shp_file, file_select = ends_with("tl2008_us_county_1950.zip")) |>
    mutate(
      STATEFIP = as.numeric(NHGISST) / 10,
      COUNTYICP = as.numeric(NHGISCTY),
    ) |>
    select(State = STATENAM, County = NHGISNAM, STATEFIP, COUNTYICP, geometry)
}

internment_proportion_map <- function(df, shp_file) {
  library(tidyverse)
  library(ggplot2)

  shp_data <- collect_county_shp(shp_file)

  map_data <- left_join(shp_data, df, by = c("STATEFIP", "COUNTYICP")) |>
    group_by(intern_location) |>
    summarise(across(any_of(c("pop_tot", "pop_chin", "pop_japn", "internees")),
                     ~ sum(.x, na.rm = TRUE)),
              evac_zone = mean(evac_zone, na.rm = TRUE)) |>
    mutate(int_prop_tot = internees / pop_tot,
           int_prop_japn = internees / pop_japn)

  plot <- ggplot(data = map_data, aes(geometry = geometry)) +
    geom_sf(aes(fill=int_prop_japn,
                color = as_factor(evac_zone))) +
    theme_linedraw() +
    theme(
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0,0)
      ) +
    ## labs(title="Internment Proportion of 1940 Japanese Population") +
    scale_color_manual(values = c("0" = NA, "1" = "#C34043")) +
    scale_fill_gradient2(
      limits = c(0,1),
      oob = scales::squish,
      na.value = "#625e5a",
      low = "#252535",
      mid = "#2D4F67",
      high = "#f2ecbc",
      midpoint = 0.5,
      ## transform = "exp"
    ) +
    coord_sf(crs = st_crs(8858),
             xlim = c(-3000000, -1000000),
             ylim = c(3900000, 5900000),
             expand = FALSE)

  return(plot)
}
