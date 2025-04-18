library(pacman)
p_load(tidyverse,data.table)
# drop Alaska, Hawaii, overseas military and Wash. D.C. and unclassified:
drop_states = c(81, 82, 83, 96, 97, 98, 99)
# get ipums data:
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("/Volumes/Backup Plus/Internment Project/Time Series Sample Data/IPUMS census sample data/usa_00017.xml")
df = read_ipums_micro(ddi) %>% 
  mutate(JA = ifelse(RACE==5, 1, 0), # racially Japanese
         ISSEI = (RACE==5)*(BPL==501),) %>%  # Japanese born in Japan
  filter(!(STATEICP %in% drop_states), # drop extra states/territories
         YEAR %in% c(1940,1950)) %>%  # get only pre-post int years
  mutate(EZ = ifelse(STATEICP %in% c(61, 71, 72, 73), # define exclusion zone
    #(STATEICP == 71) | # all CA counties
      #((STATEICP == 61) & (COUNTYICP %in% ezAZ)) | # ez county in AZ
      #((STATEICP == 72) & (COUNTYICP %in% ezOR)) | # ez county in OR
      #((STATEICP == 73) & (COUNTYICP %in% ezWA)), # ez county in WA
    1, # 1 is subject to evacuation
    0 # 0 were not (fully) subject to evac
  )) %>% 
  mutate(INCWAGE = ifelse(INCWAGE==999999, NA, INCWAGE)) %>% 
  mutate(INCWAGE = ifelse(INCWAGE==999998, NA, INCWAGE)) %>% 
  mutate(FARMWRK = ifelse((OCC1950 %in% c(100, 810, 820, 830, 840)), 1, 0)) %>% 
  mutate(EMPL = ifelse(EMPSTAT==1, 1, 0))

# get sum stats for Japanese in and out of EZ:
sum_st_data = df %>% 
  group_by(STATEICP, YEAR) %>% 
  summarise(N = n(),
         N_JA = sum(JA==1),
         per_JA = N_JA/N, #percent population Japanese
         SEX = mean(SEX, na.rm=TRUE),
         AGE = mean(AGE, na.rm=TRUE),
         ISSEI = mean(ISSEI, na.rm=TRUE),
         EDUC = mean(EDUC),
         EMPL = mean(EMPL),
         INCWAGE = mean(INCWAGE, na.rm=TRUE),
         FARMWRK = mean(FARMWRK, na.rm=TRUE),
  )

JA_sum_data = df %>%
  filter(RACE==5) %>% 
  group_by(EZ, YEAR) %>% 
  summarise(SEX = mean(SEX, na.rm=TRUE),
            AGE = mean(AGE, na.rm=TRUE),
            ISSEI = mean(ISSEI, na.rm=TRUE),
            EDUC = mean(EDUC),
            EMPL = mean(EMPL),
            INCWAGE = mean(INCWAGE, na.rm=TRUE),
            FARMWRK = mean(FARMWRK, na.rm=TRUE),
            N = n()
  )

p_load(fixest, huxtable)
# triple-diff estimation:
ddd_reg = feols(log(INCWAGE) ~ EZ*YEAR*JA + ## Our key interaction: time × treatment status
                 # SEX + AGE + EDUC + FARMWRK ## Other controls
                 + JA + EZ*YEAR + YEAR*JA + EZ*JA + EZ + YEAR,  ## FEs
                 cluster = 'STATEICP', ## Clustered SEs
                 data = df) 
tidy(ddd_reg)

# collect into state panel:
state_df = df %>% 
  group_by(YEAR, STATEICP) %>% 
  summarise(PCT_JA = mean(JA==1, na.rm=T), # num of Japanese
            N_ISSEI = sum(ISSEI==1), # num of 1st gen JA immigrants
            #PCT_ISSEI = N_ISSEI/N_JA, # rate of 1st gen in JA pop
            SEX = mean(SEX, na.rm=TRUE),
            AGE = mean(AGE, na.rm=TRUE),
            EDUC = mean(EDUC,na.rm=T),
            EMPL = mean(EMPL,na.rm=T),
            INCWAGE = mean(INCWAGE, na.rm=TRUE),
            FARMWRK = mean(FARMWRK, na.rm=TRUE),
            ) #get state-level stats
# Get historical state id codes:
p_load(readxl)
icpsr <- read_excel('/Volumes/Backup Plus/Internment Project/Time Series Sample Data/GIS data/ICPSR1940.xlsx')# historical state ID codes
icpsr = icpsr %>%   filter(!(STATEICP %in% drop_states)) %>% filter(STATEICP!=8)

# expand icpsr codes for each state into panel form:
panel_st = with(icpsr, expand.grid(YEAR = c(1940,1950),
                                   STATEICP = unique(STATEICP))) 
panel_st = left_join(panel_st, icpsr, by="STATEICP")
panel_st_df <- left_join(panel_st, state_df, by=c("STATEICP","YEAR")) 

p_load(ggplot2, ggthemes)

p_load(sf)
camp_shp = st_read("/Volumes/Backup Plus/Internment Project/Time Series Sample Data/GIS data/WRA_Relocation_Centers/WRA_Relocation_Centers.shp")

# Mapping: -----------------------------------------
p_load(maps, stringr)
states <- map_data("state") %>% 
  mutate(STATENAME = str_to_title(region)) 
statesICP = left_join(states, icpsr, by = "STATENAME") %>% filter(STATENAME!="District Of Columbia")

# get data into map form:
state_map_data = left_join(statesICP, panel_st_df, by = "STATENAME")

p_load(ggplot2, ggthemes, paletteer, viridis, gganimate, transformr)
st_income_map = ggplot() + 
  geom_polygon(data=state_map_data, aes(x=long, y=lat, group=group,
                                         fill=INCWAGE),
               color="black", size=.1 ) +
  scale_fill_viridis(
                      #breaks = c(10, 100, 1000, 10000),
                     #trans=scales::pseudo_log_trans(), 
                     option="A") +
  geom_point(data=camp_shp, aes(x=long,y=lat),fill="white") +
  labs(title="State-level Average Incomes",
       subtitle="1940 Census 1% Sample",
       x="",
       y="",
       caption="Mean wage income out of all individual wage incomes reported in each state") +
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
st_income_map
ggsave("st_income_map.png")

# Diff-in-Diff for effect of internment on wages: --------------
p_load(huxtable, fixest)
mod_twfe = feols(INCWAGE ~ i(YEAR, EZ, ref = 1940)  ## Our key interaction: time × treatment status
                 ## Other controls
                 | STATEICP,                             ## FEs
                 #cluster = region,                          ## Clustered SEs
                 data = panel_st_JA_df) 
iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')
tidy(mod_twfe)
