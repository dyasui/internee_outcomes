as_cohort <- function(birthyr) {
  cohort = case_when(
    birthyr %in% 1940:1942 ~ "1940-1942",
    birthyr %in% 1929:1939 ~ "1930-1939",
    birthyr %in% 1924:1928 ~ "1920-1929",
    birthyr %in% 1913:1923 ~ "1910-1919",
    birthyr %in% 1903:1912 ~ "1893-1909",
    birthyr %in% 1893:1902 ~ "1877-1992",
    birthyr < 1877 ~ "pre1876",
    )
  return(cohort)
}
