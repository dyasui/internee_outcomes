library(tidyverse)

# proportion of each demographic group in internee population
pr_z_I <- read_csv("data/internment_groups.csv")

# proportion of 1940 Japanese population interned
pr_I <- 108525/126701

# proportion of each demographic group in 1940 Japanese population
pr_z <- read_csv("data/ja_pop_groups.csv")

dt <- left_join(pr_z_I, pr_z,
                by = c("NHGISST", "NHGISCTY",
                       "sex"="SEX", "birthyr"="BIRTHYR", "birthplace"="BPL"),
                suffix = c("_z_I","_z")) |>
  mutate(
    ## pr_intern = (p_z_I * pr_I) / p_z ,
    pr_intern = n_z_I / n_z
  )

write_csv(dt, file = "data/pr_intern_status.csv")

ggplot(data = dt) +
  geom_histogram(aes(x=pr_intern), fill = "blue", alpha = 0.5) +
  ## geom_histogram(aes(x=pr_intern_test), fill = "green", alpha = 0.5) +
  xlim(0,2) +
  theme_minimal()

dt |>
  group_by(state, county) |>
  summarise(
    n_interned = sum(n_z_I, na.rm = T),
    n_1940 = sum(n_z, na.rm = T),
    prop_interned = round(n_interned / n_1940, 2)
  ) |>
  arrange(desc(n_interned))
