clean_ipums <- function(data) {
  library(tidyverse)
  data |>
    mutate( # replace missing values with NAs and adjust all dollar amounts to 1950 levels
      INCWAGE = ifelse(INCWAGE %in% c(999999,999998), NA, INCWAGE),
      # adjust 1940 wages for inflation to 1950 dollars
      INCWAGE = ifelse(YEAR == 1940, INCWAGE * 1.71, INCWAGE),
      married = ifelse(MARST %in% 1:2, 1, 0),
      foreign = ifelse(NATIVITY == 5, 1, 0),
      college = ifelse(EDUC %in% 7:11, 1, 0),
      employed = ifelse(EMPSTAT == 1, 1, 0),
      female = ifelse(SEX==2, 1, 0),
      OCC1950 = ifelse(OCC1950 %in% 979:999, NA, OCC1950),
      age = YEAR - BIRTHYR,
      female = ifelse(SEX == 2, 1, 0),
      wage = ifelse(is.na(INCWAGE), 0, INCWAGE),
      lnwage = ifelse(INCWAGE!=0, log(INCWAGE), NA),
      citizen = ifelse(CITIZEN %in% c(0, 1, 2), 1, 0),
      homestate = ifelse(BPL == STATEFIP, 1, 0),
      generation = case_when(
        NATIVITY == 5 ~ "first", # foriegn born
        NATIVITY %in% 2:4 ~ "second", # both or either parent foriegn born
        NATIVITY == 1 & (NATIVITY_POP %in% 2:4 | NATIVITY_MOM %in% 2:4) ~ "third"
        ## .default == NA
        )
    )
}
