library(tidyverse)
library(haven)
mlp_panel <- read_csv("data/linked_census_sample.csv")
intrn_stat <- read_csv("data/pr_intern_status.csv")

# assign predicted internment status by 1940 location and demographic group
df <- left_join(mlp_panel, intrn_stat,
                        by = c("STATEFIP", "COUNTYICP",
                               "SEX"="sex", "BIRTHYR"="birthyr",
                               "BPL"="bpl")) |>
  # assign did internment status
  mutate(
    race = case_when(RACE == 4 ~ "Chinese",
                     RACE == 5 ~ "Japanese"),
    post = ifelse(YEAR==1950, 1, 0), # post-treatment
    treat = ifelse(pr_intern >=1, 1, 0), #treatment group
    did = treat * post, # treatment status
    pr_intern = case_when(
      RACE != 5 ~ 0, # Chinese not interned
      !is.na(pr_intern) ~ pr_intern,
      is.na(pr_intern) ~ 0,
      .default = 0),
    ) |>
  select(
    id, YEAR, STATEFIP, COUNTYICP, race, female, age, college, married, foreign, citizen,
    homestate, BPL, BPLD, employed, wage, OCCSCORE, OCC1950, n_z, n_z_I, pr_intern,
    post, treat, did
    )

write_csv("data/linked_internee_sample.csv")
