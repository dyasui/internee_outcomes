library(tidyverse)
library(haven)
mlp_panel <- read_csv("data/linked_census_sample.csv")
intrn_stat <- read_csv("data/pr_intern_status.csv")

# assign predicted internment status by 1940 location and demographic group
df <- left_join(mlp_panel, intrn_stat,
                        by = c(
                          "STATEFIP_1940"="STATEFIP",
                          "COUNTYICP_1940"="COUNTYICP",
                          "SEX"="sex",
                          "BIRTHYR"="birthyr",
                          "BPL"="birthplace"
                          )) |>
  # assign did internment status
  mutate(
    pr_intern = replace_na(pr_intern, 0),
    race = case_when(RACE == 4 ~ "Chinese",
                     RACE == 5 ~ "Japanese"),
    post = ifelse(YEAR==1950, 1, 0), # post-treatment
    treat = case_when(
      race == "Chinese" ~ 0,
      pr_intern >= 0.9 & race == "Japanese" ~ 1, #treatment group
      pr_intern == 0 & race == "Japanese" ~ 0, #treatment group
      .default = NA
    ),
    did = treat * post # treatment status
    )
## write_csv(df, "data/linked_internee_sample.csv")

df |> filter(YEAR==1940) |>
ggplot() +
  geom_histogram(aes(x=pr_intern), fill = "blue", alpha = 0.5) +
  ## xlim(0,2) +
  theme_minimal()
