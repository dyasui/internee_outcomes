library(tidyverse)
df <- read_csv("data/linked_internee_sample.csv") |>
  select(
    id, YEAR, STATEFIP, COUNTYICP, female, race, married, BIRTHYR, age,
    foreign, citizen, generation, homestate,
    employed, wage, lnwage, OCCSCORE, OCC1950, college,
    pr_intern, treat, post, did
    )

df |>
  group_by(treat, race, YEAR) |>
  summarise(
    n = n(),
    across(c(wage, OCCSCORE, pr_intern, female, age, college, foreign, employed),
           ~round(mean(., na.rm=T), digits = 2)
           )
  )

df |>
  select(id, YEAR, race, BIRTHYR)

df |>
  ggplot(aes(x=YEAR, y = INCWAGE, color = as_factor(race))) +
  geom_line(data = group_averages, aes(x=YEAR, y = INCWAGE, group = c(race,treat)), size=1, linetype = "dashed") +
  scale_color_manual(values=c("#003049", "#D62828", "#F77F00", "#FCBF49")) +
  scale_x_discrete("YEAR") +
  theme_minimal() +
  labs(title="Interned Japanese Experienced Higher Wage Growth",
       x="Year",
       y="Wages (1940 dollars)",
       caption="") +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

library(fixest)

reg_did_1 <- feols(
  lnwage ~ did + treat + post + female + age + age^2 + college + married, data = df)

reg_did_2 <- feols(
  lnwage ~ did + treat + post + female + age + age^2 + college + married |
    id,
  data = df)

reg_did_3 <- feols(
  lnwage ~ did + treat + post + female + age + age^2 + college + married |
    as_factor(STATEFIP) + id,
  data = df)

reg_did_4 <- feols(
  lnwage ~ did + treat + post + female + age + age^2 + college + married |
    as_factor(STATEFIP),
  data = df)

etable(reg_did_1, reg_did_2, reg_did_3, reg_did_4
       ## , tex = TRUE
       )

library(fixest)

reg_did_1 <- feols(
  wage ~ did + treat + post + female + age + age^2 + college + married, data = df)

reg_did_2 <- feols(
  wage ~ did + treat + post + female + age + age^2 + college + married |
    id,
  data = df)

reg_did_3 <- feols(
  wage ~ did + treat + post + female + age + age^2 + college + married |
    as_factor(STATEFIP) + id,
  data = df)

reg_did_4 <- feols(
  wage ~ did + treat + post + female + age + age^2 + college + married |
    as_factor(STATEFIP),
  data = df)

etable(reg_did_1, reg_did_2, reg_did_3, reg_did_4
       ## , tex = TRUE
       )

wide_df <- df |>
  pivot_wider(id_cols = id, names_from = YEAR, values_from = STATEICP:group) |>
  mutate(lf_drop = case_when(EMPSTAT_1940 == 1 & EMPSTAT_1950 !=1),
         OCCSCORE

reg_did_1 <- feols(
  employed ~ did + treat + post + female + age + age^2 + college + married, data = df)

reg_did_2 <- feols(
  employed ~ did + treat + post + female + age + age^2 + college + married |
    id,
  data = df)

reg_did_3 <- feols(
  employed ~ did + treat + post + female + age + age^2 + college + married |
    as_factor(STATEFIP) + id,
  data = df)

reg_did_4 <- feols(
  employed ~ did + treat + post + female + age + age^2 + college + married |
    as_factor(STATEFIP),
  data = df)

etable(reg_did_1, reg_did_2, reg_did_3, reg_did_4
       ## , tex = TRUE
       )
