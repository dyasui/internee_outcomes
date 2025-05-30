library(tidyverse)

JARVIS_path <- "data/JARVIS_PublicUse/Data_CodeLists/"
history_file <- paste(JARVIS_path,"HISTORY.TXT", sep = "")

history_dt <- read_delim(history_file, delim = "~^~")
