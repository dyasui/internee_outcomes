library(pacman)
p_load(tidyverse)

# Categories and stuff: ---------------
# evacuation zone west coast states:
c(61, 71, 72, 73)
# evac zone counties ICPSR codes:
ezAZ = c(30, 130, 190, 230, 270)
ezOR = c(30, 50, 70, 90, 110, 190, 270, 290, 330, 390, 410, 430, 470, 510, 530, 570, 670, 710)
ezWA = c(50, 70, 90, 150, 270, 310, 330, 350, 370, 390, 410, 450, 490, 530, 570, 590, 610, 670, 690, 730, 770)
# counties split by zone boundary:
spAZ = c(90, 130, 150, 210, 250)
spOR = c(170, 310, 350, 550, 650)
spWA = c(470)
# drop overseas military and Wash. D.C. and unclassified:
drop_states = c(83, 96, 97, 98, 99)
# categorize by US region:
WC = c("Washington","Oregon","California","Arizona") # evac zone states
CP = c("Utah","Idaho","Wyoming","Colorado","Arkansas") # states w/ WRA camps
MT = c("Montana","Nevada", "New Mexico")
MW = c("North Dakota","South Dakota","Nebraska","Kansas","Minnesota",
       "Iowa","Missouri","Wisconsin","Illinois","Michigan","Indiana","Ohio")
NE = c("Maine","New Hampshire","Vermont","Massachusetts",
       "Rhode Island","Connecticut","New York","New Jersey",
       "Pennsylvania","Maryland","Delaware")
SO = c("Oklahoma","Texas","Louisiana","Mississippi","Alabama",
       "Georgia","Florida","South Carolina","North Carolina",
       "Tennessee","Kentucky","West Virginia","Virginia")



# IPUMS DATA: ------------------------
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("/Volumes/Backup Plus/Internment Project/Time Series Sample Data/IPUMS census sample data/usa_00018.xml")
df = read_ipums_micro(ddi) %>% 
  filter(RACE==5) %>% 
  filter(!(STATEICP %in% drop_states)) %>% # drop extra states/territories
  mutate(EZ = ifelse(#STATEICP %in% c(61, 71, 72, 73),
                     (STATEICP == 71) | # all CA counties
                     ((STATEICP == 61) & (COUNTYICP %in% ezAZ)) | # ez county in AZ
                     ((STATEICP == 72) & (COUNTYICP %in% ezOR)) | # ez county in OR
                     ((STATEICP == 73) & (COUNTYICP %in% ezWA)), # ez county in WA
                     1, # 1 is subject to evacuation
                     0 # 0 were not (fully) subject to evac
  )) %>% 
  mutate(INCWAGE = ifelse(INCWAGE==999999, NA, INCWAGE)) %>% 
  mutate(INCWAGE = ifelse(INCWAGE==999998, NA, INCWAGE)) %>% 
  mutate(FARMWRK = ifelse((OCC1950 %in% c(100, 810, 820, 830, 840)), 1, 0)) %>% 
  mutate(ISSEI = ifelse(NATIVITY==5, 1, 0)) %>% 
  mutate(EMPL = ifelse(EMPSTAT==1, 1, 0))

sum_data = df %>%
  #filter(AGE>=18) %>% 
  group_by(EZ) %>% 
  summarise(SEX = mean(SEX, na.rm=TRUE),
            AGE = mean(AGE, na.rm=TRUE),
            ISSEI = mean(ISSEI, na.rm=TRUE),
            EDUC = mean(EDUC),
            EMPL = mean(EMPL),
            INCWAGE = mean(INCWAGE, na.rm=TRUE),
            FARMWRK = mean(FARMWRK, na.rm=TRUE),
            N = n()
            )

county_data = df %>% 
  group_by(STATEICP, COUNTYICP) %>% 
  summarise(SEX = mean(SEX, na.rm=TRUE),
            AGE = mean(AGE, na.rm=TRUE),
            ISSEI = mean(ISSEI, na.rm=TRUE),
            EDUC = mean(EDUC),
            EMPL = mean(EMPL),
            INCWAGE = mean(INCWAGE, na.rm=TRUE),
            FARMWRK = mean(FARMWRK, na.rm=TRUE),
            N = n()
  )

# Get historical state id codes:
library("readxl")
icpsrcnt <- read_excel("icpsrcnt.xlsx") # historical state ID codes
p_load(maps, stringr)
p_load(sf) # shapefiles package
counties40 = st_read("nhgis0002_shapefile_tl2000_us_county_1940/nhgis0002_shapefile_tl2000_us_county_1940/US_county_1940.shp")
states <- map_data("county") %>% 
  mutate(State = str_to_title(region),
         County = str_to_title(subregion)) 
countiesICP = left_join(states, icpsrcnt, by = c("State","County")) %>% 
  mutate(EZ = ifelse(#STATEICP %in% c(61, 71, 72, 73),
    (STATEICP == 71) | # all CA counties
      ((STATEICP == 61) & (COUNTYICP %in% ezAZ)) | # ez county in AZ
      ((STATEICP == 72) & (COUNTYICP %in% ezOR)) | # ez county in OR
      ((STATEICP == 73) & (COUNTYICP %in% ezWA)), # ez county in WA
    1, # 1 is subject to evacuation
    0 # 0 were not (fully) subject to evac
  ))

# get data into map form:
county_map_data = left_join(countiesICP, county_data, by = c("STATEICP","COUNTYICP"))
county_map_data[is.na(county_map_data)] = 0 # replace NA with zeros

# create map of Japanese distribiution:
p_load(ggplot2, ggthemes, paletteer, viridis, gganimate, transformr)
county_map = ggplot() + 
  geom_polygon(data=county_map_data, aes(x=long, y=lat, group=group,
                                        fill=N),
               color="black", size=.1 ) +
  scale_fill_viridis(breaks = c(10, 100, 1000, 10000),
                     trans=scales::pseudo_log_trans(), option="D") +
  geom_polygon(data=county_map_data, aes(x=long, y=lat, group=group),
               fill=NA, color=ifelse(county_map_data$EZ==1,"red",NA), size=.3) +
  labs(title="County-Level Japanese American Populations",
       subtitle="1940 Full-Count Census",
       x="",
       y="",
       caption="Number of individuals in 1940 census whose race is Japanese for each county (drawn with 2010 boundaries).
       Counties which were within the Japanese Exclusion Zone are outlined in blue.") +
  theme_stata() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom") +
  guides(colour = guide_legend(title="Number of Japanese Individuals",
                               title.position = "top"))
county_map
ggsave("county_JAmap.png")

