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
      ),
      educ = case_when(
        EDUCD == "Missing" ~ NA,
        EDUCD == "No schooling completed" ~ 0,
        EDUCD == "Kindergarten" ~ 0,
        EDUCD == "Grade 1" ~ 1,
        EDUCD == "Grade 2" ~ 2,
        EDUCD == "Grade 3" ~ 3,
        EDUCD == "Grade 4" ~ 4,
        EDUCD == "Grade 5" ~ 5,
        EDUCD == "Grade 6" ~ 6,
        EDUCD == "Grade 7" ~ 7,
        EDUCD == "Grade 8" ~ 8,
        EDUCD == "Grade 9" ~ 9,
        EDUCD == "Grade 10" ~ 10,
        EDUCD == "Grade 11" ~ 11,
        EDUCD == "Grade 12" ~ 12,
        EDUCD == "1 year of college" ~ 13,
        EDUCD == "2 years of college" ~ 14,
        EDUCD == "3 years of college" ~ 15,
        EDUCD == "4 years of college" ~ 16,
        EDUCD == "5 years of college" ~ 17,
        EDUCD == "6 years of college" ~ 18,
        EDUCD == "7 years of college" ~ 19,
        EDUCD == "8+ years of college" ~ 20,
        )
    )
}
