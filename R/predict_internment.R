library(tidyverse)

# proportion of each demographic group in internee population
pr_z_I <- read_csv("data/internment_groups.csv")

# proportion of 1940 Japanese population interned
pr_I <- 108525/126701

# proportion of each demographic group in 1940 Japanese population
pr_z <- read_csv("data/ja_pop_groups.csv")

dt <- left_join(pr_z_I, pr_z,
                by = c("NHGISST", "NHGISCTY",
                       "sex"="SEX", "birthyr"="BIRTHYR", "bpl"="BPL"),
                suffix = c("_z_I","_z")) |>
  mutate(pr_intern = ifelse(is.na(p_z_I), 0, (p_z_I * pr_I) / p_z))

dt <- dt |>
  mutate(pr_intern = ifelse(pr_intern > 1, 1, pr_intern))

write_csv(dt, file = "data/pr_intern_status.csv")
