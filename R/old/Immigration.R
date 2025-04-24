# Japanese Immigration Data:
library(pacman)
p_load(tidyverse)
drop_states = c(81, 82, 83, 96, 97, 98, 99)
# IPUMS DATA: ------------------------
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("usa_00011.xml")
data = read_ipums_micro(ddi) %>% 
  #filter(RACE==5) %>% #only take Japanese
  mutate(JAIM = (RACE==5)*(NATIVITY==5)) %>% 
  select(YEAR, STATEICP, JAIM) %>% 
  filter(!(STATEICP %in% drop_states)) %>% 
  group_by(YEAR, STATEICP) %>% 
  summarise(N = n(),
            N_JAIM = sum(JAIM==1)) %>% 
  mutate(per_JAIM = N_JAIM/N) %>% 
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
icpsr = icpsr %>%   filter(!(STATEICP %in% drop_states))
# expand icpsr codes for each state into panel form:
panel_st = with(icpsr, expand.grid(YEAR = c(1900,1910,1920,1930,1940,1950,1960),
                                   STATEICP = unique(STATEICP))) 
panel_st = left_join(panel_st, icpsr, by="STATEICP")
panel_df <- left_join(panel_st, data, by=c("STATEICP","YEAR")) #
panel_df[is.na(panel_df)] = 0 # replace NA with zeros
panel_df = panel_data(panel_df, id=STATEICP, wave=YEAR) # make panel form

ggplot(panel_df, aes(x=YEAR, y=N_JAIM, fill=STATENAME)) + 
  geom_area()

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
panel_df2 = panel_df %>%
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
            N_JAIM = sum(N_JAIM),
            per_JAIM = N_JAIM/N
  )
# reorder regions for plot:
panel_df2$region <- factor(panel_df2$region, 
                           levels=c(
                             "Washington","Oregon","California","Arizona",
                             "Arkansas","Colorado","Idaho","Utah","Wyoming",
                             "MountainWest","Midwest","NorthEast","South"
                           ) )

# region area plot:
ggplot(panel_df2, aes(x=YEAR, y=N_JAIM, fill=region)) + 
  geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=1942,xmax=1946),
            fill="grey", alpha=0.5) +
  geom_area() +
  scale_fill_manual(values = 
                      c(Washington="indianred",Oregon="indianred1",California="indianred2",Arizona="indianred3",
                        Arkansas="olivedrab",Colorado="olivedrab1",Idaho="olivedrab2",Utah="olivedrab3",Wyoming="olivedrab4",
                        MountainWest="orange",Midwest="blue",NorthEast="yellow",South="purple"))+
  annotate("text", x=1944, y=400, label="WWII Internment", 
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
  labs(title="Japanese Immigration into the US:",
       subtitle="Sample Counts - 1900 to 1960 decades and by states and regions",
       x="Year",
       y="Number of Sampled Japanese Immigrants",
       caption="") +
  theme_stata()+
  theme(#axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom")
ggsave("immigrantareaplot.png")
