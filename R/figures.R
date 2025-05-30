library(ipumsr)
library(tidyverse)
library(dbplyr)
library(duckdb)
# data extract file
db <- dbConnect(duckdb(), dbdir = "data/ipums_db.duckdb")
tbl = "ipums_microdata"

pr_I <- read_csv("data/pr_intern_status.csv")

ja_pop <- tbl(db, tbl) |>
  select(YEAR, STATEFIP, COUNTYICP, SEX, BIRTHYR, MARST, BPL, BPLD, NATIVITY, CITIZEN,
         EDUC, EMPSTAT, LABFORCE, OCC1950, INCTOT, INCWAGE, BPL_MOM, BPL_POP) |>
  collect() |>
  mutate(
    # replace missing values with NAs and adjust all dollar amounts to 1940 levels
    across(c(INCWAGE),
           ~ case_when(.x %in% c(999999,999998) ~ NA,
                             YEAR == 1950 ~ .x * 0.59, # deflated dollars to 1940 standards
                             .default = .x)
           ),
    ) |>
  left_join(pr_I, by = c("STATEFIP", "COUNTYICP", SEX="sex", BIRTHYR="birthyr", BPL="birthplace")) |>
  mutate(internee = ifelse(pr_intern > 0.5, T, F))

dbDisconnect()

data_int <- read_csv("data/internees_data.csv")

d1 <- data_int |>
  mutate(group = "Internees") |>
  select(group, NHGISST, NHGISCTY, SEX=sex, BIRTHYR=birthyr,
         BPL=bpl, NATIVITY=nativity)

d2 <- ja_pop |>
  filter(YEAR == 1940, RACE == 5) |>
  mutate(
    group = "All Japanese",
    SEX = ifelse(SEX==1, "male", "female"),
    NHGISST = str_pad(string = STATEFIP*10, width = 3, side = "left", pad = "0"),
    NHGISCTY = str_pad(COUNTYICP, width = 4, side = "left", pad = "0") ) |>
  select(group, NHGISST, NHGISCTY, SEX, BIRTHYR, BPL, NATIVITY)

plot_d <- bind_rows(d1,d2)

ggplot(data = plot_d, aes(x=BIRTHYR, group = group, fill = group)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  theme(
    ## legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Internee Birth Year Distribution vs Overall Japanese Population",
    x = "Birth Year",
    )

ggplot(data = ja_pop, aes(x=INCWAGE, group = internee, fill = internee)) +
  geom_density(alpha = 0.5) +
  facet_wrap("YEAR") +
  scale_x_log10() +
  theme_minimal() +
  theme(
    ## legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Internee Wage Distribution vs Overall Japanese Population",
    x = "Real Wages (1940 $)",
    )
