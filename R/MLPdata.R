
# clean MLP data download ----
# # NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
library(pacman)
p_load(dplyr)
if (!require("ipumsr")) stop(
  "Reading IPUMS data into R requires the ipumsr package. 
  It can be installed using the following command: install.packages('ipumsr')")

# load and filter linked 1900 observations
ddi_00 <- read_ipums_ddi("Data/1900/usa_00035.xml")
df_00 <- read_ipums_micro(ddi_00) %>% 
  filter(HIK!="NA")

# load and filter linked 1910 observations
ddi_10 <- read_ipums_ddi("Data/1910/usa_00034.xml")
df_10 <- read_ipums_micro(ddi_10) %>% 
  filter(HIK!="NA")

# load and filter linked 1920 observations
ddi_20 <- read_ipums_ddi("Data/1920/usa_00032.xml")
df_20 <- read_ipums_micro(ddi_20) %>% 
  filter(HIK!="NA")

# load and filter linked 1930 observations
ddi_30 <- read_ipums_ddi("Data/1930/usa_00033.xml")
df_30 <- read_ipums_micro(ddi_30) %>% 
  filter(HIK!="NA")

# load and filter linked 1940 observations
ddi_40 <- read_ipums_ddi("Data/1940/usa_00027.xml")
df_40 <- read_ipums_micro(ddi_40) %>% 
  filter(HIK!="NA")




