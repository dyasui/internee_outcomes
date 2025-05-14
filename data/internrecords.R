####################
#  LOAD PACKAGES   #
library(tidyverse) #
library(readr)     #
library(usdata)    #
####################

# Data from THE EVACUATED PEOPLE : A QUANTITATIVE DESCRIPTION
#   Courtesy of Cooper Thomas:
#   https://data.world/infinitecoop/japanese-internment-camps
JapaneseAmericanPopulation_1940_1945 <- 
  read_csv("data/internrecords/WRA-infinitecoop/JapaneseAmericanPopulation_1940_1945.csv")
RelocationDestinations_Cities <-
  read_csv("data/internrecords/WRA-infinitecoop/RelocationDestinations_Cities.csv") %>% 
  mutate(City = str_replace(City, "Berkely", "Berkeley"))

# City to county data set from https://simplemaps.com/data/us-cities
uscities <- read_csv("data/internrecords/simplemaps_uscities_basicv1.78/uscities.csv") %>% 
  # select(city, state_id, county_name) %>% 
  add_row(city = "Bridgeton", state_id = "NJ", county_name = "Cumberland") %>% 
  add_row(city = "Venice", state_id = "CA", county_name = "Los Angeles")

RelocationDestinations_Cities <-
  left_join(RelocationDestinations_Cities, uscities, 
            by = c('City'='city', 'State'='state_id')) %>% 
  group_by(State, county_name) %>% 
  mutate(state_name = usdata::abbr2state(State),
         county_fips = as.numeric(county_fips)) %>% 
  rename(RelInternees = People) %>% 
  select(state_name, county_name, RelInternees, county_fips)

# ICPSR County codes
icpsrcnt <- read_csv("data/icpsr2fip.csv")

RelocationDestinations_Counties <- 
  full_join(RelocationDestinations_Cities, icpsrcnt, 
            by = c("state_name"="State", "county_name"="County")) %>% 
  rename(County = county_name)

################################################################################
# CODING EVACUATED AREAS BY COUNTY
################################################################################
ezAZ = c( 30, 130, 190, 230, 270)
ezOR = c( 30,  50,  70,  90, 110, 190, 270, 290, 330, 390, 410, 430, 470, 510,
         530, 570, 670, 710)
ezWA = c( 50,  70,  90, 150, 270, 310, 330, 350, 370, 390, 410, 450, 490, 530,
         570, 590, 610, 670, 690, 730, 770)

# CLASSIFYING TREATMENT STATUS
interncounties_df <- RelocationDestinations_Counties %>% 
  mutate(
    evacuated = case_when(
      state_name == "California" ~ TRUE,
      state_name == "Arizona"    & COUNTYICP %in% ezAZ ~ TRUE,
      state_name == "Oregon"     & COUNTYICP %in% ezOR ~ TRUE,
      state_name == "Washington" & COUNTYICP %in% ezWA ~ TRUE,
      .default = FALSE
    ),
    ReceivedInternees = !is.na(RelInternees),
    # group = case_when(
    #   evacuated == TRUE  ~ "evacuated",
    #   evacuated == FALSE & ReceivedInternees == TRUE  ~ "treated",
    #   evacuated == FALSE & ReceivedInternees == FALSE ~ "control"
    # )
    )

save(interncounties_df, file = "data/internrecords.Rdata")
