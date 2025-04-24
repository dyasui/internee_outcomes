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
# drop Alaska, Hawaii, overseas military and Wash. D.C. and unclassified:
drop_states = c(81, 82, 83, 96, 97, 98, 99)
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
p_load(impumsr)
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("usa_00012.xml")
pop_data = read_ipums_micro(ddi) 
sum_data = pop_data %>% 
  mutate(JA = ifelse(RACE==5, 1, 0), # racially Japanese
         JAIM = (RACE==5)*(BPL==501)) %>%  # Japanese born in Japan
  select(YEAR, STATEICP, JA, JAIM) %>% # select key vars
  filter(!(STATEICP %in% drop_states)) %>% # drop extra states/territories
  group_by(YEAR, STATEICP) %>% 
  summarise(N = n(),
            N_JA = sum(JA==1),
            N_JAIM = sum(JAIM==1)) %>% #get state-level stats
  mutate(per_JA = N_JA/N, #percent population Japanese
         per_JAIM = N_JAIM/N, #percent population Japanese immigrant
         ) %>% 
  group_by(YEAR) %>% 
  mutate(shr_JA = N_JA/sum(N_JA), # state share of national JA pop
         shr_JAIM = N_JAIM/sum(N_JAIM), # share of JA immigrants
         ) %>% 
  mutate(EZ = ifelse(STATEICP %in% c(61, 71, 72, 73),
                     #(STATEICP == 71) | # all CA counties
                     #((STATEICP == 61) & (COUNTYICP %in% ezAZ)) | # ez county in AZ
                     #((STATEICP == 72) & (COUNTYICP %in% ezOR)) | # ez county in OR
                     #((STATEICP == 73) & (COUNTYICP %in% ezWA)), # ez county in WA
                     1, # 1 is subject to evacuation
                     0 # 0 were not (fully) subject to evac
  ))

# Get historical state id codes:
library("readxl")
icpsr <- read_excel('ICPSR1940.xlsx')# historical state ID codes
icpsr = icpsr %>%   filter(!(STATEICP %in% drop_states)) %>% filter(STATEICP!=8)

# expand icpsr codes for each state into panel form:
panel_st = with(icpsr, expand.grid(YEAR = c(1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000),
                                   STATEICP = unique(STATEICP))) 
panel_st = left_join(panel_st, icpsr, by="STATEICP")
panel_df <- left_join(panel_st, sum_data, by=c("STATEICP","YEAR")) #
panel_df[is.na(panel_df)] = 0 # replace NA with zeros
panel_df = panel_df %>% 
  mutate(region = case_when(STATENAME %in% WC ~ STATENAME,
                            STATENAME %in% CP ~ STATENAME,
                            STATENAME %in% MT ~ "MountainWest",
                            STATENAME %in% MW ~ "Midwest",
                            STATENAME %in% NE ~ "NorthEast",
                            STATENAME %in% SO ~ "South",)
  ) %>% 
  ungroup() %>% 
  group_by(YEAR,region) %>% 
  summarise(N = sum(N),
            N_JA = sum(N_JA),
            per_JA = N_JA/N,
            N_JAIM = sum(N_JAIM),
            per_JAIM = N_JAIM/N,
  ) %>% 
  group_by(YEAR) %>% 
  mutate(shr_JA = N_JA/sum(N_JA), # state share of national JA pop
         shr_JAIM = N_JAIM/sum(N_JAIM), # share of JA immigrants
         )

# Time series plot:---------------------------------
p_load(ggplot2, ggthemes)

# reorder regions for plot:
panel_df$region <- factor(panel_df$region, 
                      levels=c(
                        "Washington","Oregon","California","Arizona",
                        "Arkansas","Colorado","Idaho","Utah","Wyoming",
                        "MountainWest","Midwest","NorthEast","South"
                               ) )

# Time series area plot: Shares ---------------------------------
ggplot(panel_df, aes(x=YEAR, y=shr_JA, fill=region)) + 
  geom_area() + 
  geom_vline(xintercept = 1940, color="grey10", linetype="dashed") +
  geom_vline(xintercept = 1950, color="grey10", linetype="dashed") +
  #geom_rect(aes(ymin=0,ymax=1,xmin=1942,xmax=1946),fill="grey", alpha=0.25) +
  annotate("text", x=1944, y=0.5, label="WWII Internment", 
           angle=90, color="gray10") +
  #scale_fill_viridis_d(direction=-1, option="B") + 
  scale_fill_manual(values = 
                      c(Washington="steelblue",Oregon="skyblue",California="steelblue2",Arizona="lightskyblue1",
                        Arkansas="goldenrod",Colorado="goldenrod1",Idaho="goldenrod2",Utah="goldenrod3",Wyoming="goldenrod4",
                        MountainWest="orange",Midwest="tomato",NorthEast="indianred",South="tomato3"))+
  scale_x_continuous(limits=c(min(panel_df$YEAR), max(panel_df$YEAR)), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0)) +
  theme_stata() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
ggsave("shareareaplot.png")

# region area plot: Counts ----------------------------------
ggplot(panel_df, aes(x=YEAR, y=N_JA, fill=region)) + 
  geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=1942,xmax=1946),
            fill="grey", alpha=0.5) +
  annotate("text", x=1944, y=2500, label="WWII Internment", 
           angle=90, color="gray10") +
  geom_area() +
  scale_fill_manual(values = 
    c(Washington="indianred",Oregon="indianred1",California="indianred2",Arizona="indianred3",
      Arkansas="olivedrab",Colorado="olivedrab1",Idaho="olivedrab2",Utah="olivedrab3",Wyoming="olivedrab4",
      MountainWest="orange",Midwest="blue",NorthEast="yellow",South="purple"))+
  labs(title="Japanese Settlement in the US:",
       subtitle="Sample Counts - 1900 to 1970 decades and by states and regions",
       x="Year",
       y="Number of Sampled Individuals",
       caption="From Decennial Census Sample Data (via IPUMS)") +
  theme_stata()+
  theme(legend.title = element_blank(),
        legend.position = "bottom")
ggsave("countareaplot.png")

# region time series plot:
ggplot(panel_df, aes(x=YEAR, y=per_JA, color=region)) + 
  #geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=1942,xmax=1946),
  #          fill="grey", alpha=0.5) +
  geom_line(size=1.2) +
  scale_color_manual(values = 
                      c(Washington="indianred",Oregon="firebrick",California="orangered",Arizona="coral",
                        Arkansas="olivedrab",Colorado="darkgreen",Idaho="limegreen",Utah="seagreen",Wyoming="lawngreen",
                        MountainWest="gray0",Midwest="gray10",NorthEast="gray20",South="gray30"))+
  labs(title="Japanese Settlement in the US:",
       subtitle="Japanese as Percent of Population",
       x="Year",
       y="Percent Japanese",
       caption="") +
  theme_stata()+
  theme(#axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom")
ggsave("JA_lineplot.png")

ggplot(panel_df, aes(x=YEAR, y=N_JAIM, fill=region)) + 
  geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=1942,xmax=1946),
            fill="grey", alpha=0.5) +
  geom_area() +
  scale_fill_manual(values = 
                      c(Washington="indianred",Oregon="indianred1",California="indianred2",Arizona="indianred3",
                        Arkansas="olivedrab",Colorado="olivedrab1",Idaho="olivedrab2",Utah="olivedrab3",Wyoming="olivedrab4",
                        MountainWest="orange",Midwest="blue",NorthEast="yellow",South="purple"))+
  annotate("text", x=1944, y=700, label="WWII Internment", 
           angle=90, color="gray10") +
  geom_vline(xintercept = 1907, color="grey60",linetype="dashed",size=1.5,alpha=.7)+
  annotate("text", x=1907-.7, y=400, label="Gentleman's Agreement",
           angle=90, color="gray10")+
  geom_vline(xintercept = 1924, color="grey60",linetype="dashed",size=1.5,alpha=.7)+
  annotate("text", x=1924-.7, y=400, label="Immigration Act of 1924",
           angle=90, color="gray10")+
  #geom_vline(xintercept = 1954, color="grey60",linetype="dashed",size=1.5,alpha=.7)+
  #annotate("text", x=1954-.7, y=400, label="McCarren-Walter Act",
  #angle=90, color="gray10")+
  labs(title="Japanese Immigrants in the US:",
       subtitle="Sample Counts - 1900 to 1960 decades and by states and regions",
       x="Year",
       y="Number of Japanese Immigrants",
       caption="") +
  theme_stata()+
  theme(#axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom")
ggsave("immigrantareaplot.png")


# move from long to wide panel:
panel_df_wide = panel_data(panel_df, id=region, wave=YEAR) %>%  # make panel form
  widen_panel

# Mapping: -----------------------------------------
panel_df2 <- left_join(panel_st, sum_data, by=c("STATEICP","YEAR"))
p_load(maps, stringr)
states <- map_data("state") %>% 
  mutate(STATENAME = str_to_title(region)) 
statesICP = left_join(states, icpsr, by = "STATENAME") %>% filter(STATENAME!="District Of Columbia")

# get data into map form:
state_map_data = left_join(statesICP, panel_df2, by = "STATENAME")
state_map_data[is.na(state_map_data)] = 0 # replace NA with zeros

# create map:
p_load(ggplot2, ggthemes, paletteer, viridis, gganimate, transformr)
wrap_maps = ggplot() + 
  geom_polygon(data=state_map_data, aes(x=long, y=lat, group=group,
                                        fill=shr_JA*100),
               color="black", size=.25 ) +
  scale_fill_viridis(option = "C",
                     name= "State share of national Japanese population each year",
                     trans = scales::pseudo_log_trans(),
                     guide = guide_colorbar(
                     direction = "horizontal",
                     barheight = unit(2, units = "mm"),
                     barwidth = unit(100, units = "mm"),
                     draw.ulim = FALSE,
                     title.position = "top",
                     title.hjust = 0.5,
                     title.vjust = 0.5 )) +
  labs(title="Moving Patterns of Japanese Americans:",
       subtitle="Decennial Years 1900 to 1970",
       x="",
       y="",
       caption="") +
  theme_void() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        legend.position = "bottom") +
  facet_wrap(~ YEAR)
  #transition_manual(YEAR)
wrap_maps
ggsave("wrap_maps.png")

# Is there something weird going on with Japanese getting undersampled in 1940?:-----
race_data = read_ipums_micro(ddi) %>% 
  select(YEAR,STATEICP,RACE) %>% 
  filter(!(STATEICP %in% drop_states)) %>% 
  group_by(YEAR, STATEICP) %>% 
  summarise(N = n(),
            N_WH = sum(RACE==1),
            N_BL = sum(RACE==2),
            N_AI = sum(RACE==3),
            N_CH = sum(RACE==4),
            N_JA = sum(RACE==5),
            N_OA = sum(RACE==6))



