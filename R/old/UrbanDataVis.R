library(pacman)
p_load(tidyverse)

# Get data from IPUMS download files:
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("usa_00008.xml")

# Get historical state id codes:
library("readxl")
icpsr <- read_excel('ICPSR1940.xlsx') # historical state ID codes

#evacuation zone west coast states:
c(61, 71, 72, 73)
#evac zone counties ICPSR codes:
ezAZ = c(30, 130, 190, 230, 270)
ezOR = c(30, 50, 70, 90, 110, 190, 270, 290, 330, 390, 410, 430, 470, 510, 530, 570, 670, 710)
ezWA = c(50, 70, 90, 150, 270, 310, 330, 350, 370, 390, 410, 450, 490, 530, 570, 590, 610, 670, 690, 730, 770)
# counties split by zone boundary:
spAZ = c(90, 130, 150, 210, 250)
spOR = c(170, 310, 350, 550, 650)
spWA = c(470)

p_load(maps, stringr)
states <- map_data("state") %>% 
  mutate(STATENAME = str_to_title(region)) 

counties <- map_data("county") %>% 
  mutate(STATENAME = str_to_title(region)) %>% 
  mutate(COUNTYNAME = str_to_title(subregion))

statesICP = left_join(states, icpsr, by = "STATENAME")

EZ = statesICP %>% 
  mutate(EZ = ifelse(
    (STATEICP == 71) | # all CA counties
      ((STATEICP == 61) & (COUNTYICP %in% ezAZ)) | # ez county in AZ
      ((STATEICP == 72) & (COUNTYICP %in% ezOR)) | # ez county in OR
      ((STATEICP == 73) & (COUNTYICP %in% ezWA)), # ez county in WA
    1, # 1 is subject to evacuation
    0 # 0 were not (fully) subject to evac
  ))

data = read_ipums_micro(ddi) %>% 
  #filter(RACE==5) %>% #only take Japanese
  mutate(EZ = ifelse(STATEICP %in% c(61, 71, 72, 73),
        #(STATEICP == 71) | # all CA counties
        #((STATEICP == 61) & (COUNTYICP %in% ezAZ)) | # ez county in AZ
        #((STATEICP == 72) & (COUNTYICP %in% ezOR)) | # ez county in OR
        #((STATEICP == 73) & (COUNTYICP %in% ezWA)), # ez county in WA
        1, # 1 is subject to evacuation
        0 # 0 were not (fully) subject to evac
      )) %>% 
  select(YEAR, COUNTYICP, STATEICP, EZ, RACE)

# get sum stats
sum_data = data %>% 
  group_by(YEAR, EZ, RACE==5) %>% 
  summarise(N = n()) 
length(which(data$EZ==1)) # number of indivs in ez in sample

sum_data = data %>% 
  group_by(YEAR, STATEICP, EZ, RACE==5) %>% 
  summarise(N = n())  # get count in each group

#Get data into wide form with counts:
counts_40 = data %>% filter(YEAR==1940) %>% 
  group_by(YEAR, STATEICP, EZ, RACE) %>% 
  summarise(N = n())
counts_50 = data %>% filter(YEAR==1950) %>% 
  group_by(YEAR, STATEICP, EZ, RACE) %>% 
  summarise(N = n())
state_data = full_join(counts_40, counts_50, by=c('STATEICP', 'EZ', 'RACE')) %>% 
  mutate(N_1940=N.x, N_1950=N.y, ) %>% 
  select(STATEICP, EZ, RACE, N_1940, N_1950)
state_data[is.na(state_data)] = 0 # replace NA with zeros
state_data = state_data %>% 
  group_by(STATEICP, EZ) %>% 
  #mutate(N_JA_1940 = N_1940[RACE==5])
  summarise(JAPERC_40 = N_1940[RACE==5]/(N_1940[RACE==1]),
            JAPERC_50 = N_1950[RACE==5]/(N_1950[RACE==1])) %>% 
  mutate(JACHG = (JAPERC_50-JAPERC_40)/(.5*JAPERC_40+.5*JAPERC_50)) #%>% # take change in pop over times
  #filter(JACHG != Inf)


#state_data = state_data %>% 
#  group_by(STATEICP, EZ) %>% 
#  mutate(JACHG = (N_1950[RACE==5]-N_1940[RACE==5])/(N_1940))


# heat maps of JA residents:

state_map_data = left_join(statesICP, state_data, by = "STATEICP")
state_map_data[is.na(state_map_data)] = 0 # replace NA with zeros

p_load(ggplot2, ggthemes)
JA_map = ggplot() + 
  geom_polygon(data=state_map_data, aes(x=long, y=lat, group=group,
                                       fill=JACHG),
                color="white" ) +
  #geom_polygon(data=state_map_data, aes(x=long, y=lat, group=EZ),color="red" ) +
  scale_fill_continuous(low = "firebrick", high = "mediumseagreen", name = '% change in Japanese Population') +
  labs(title="Moving Patterns of Japanese Americans from 1940 to 1950",
       subtitle="Evidence for Permanent Settlement after Internment?",
       x="",
       y="",
       caption="") +
  theme_stata() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        legend.position = "bottom")


p_load(knitr)
kable(sum_data, "latex")



