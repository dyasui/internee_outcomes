#------------------------------------------------------------------------------#
#                               README                                         #
# this file was translated from the original stata do file compile_wra.do      #
# from the replication files for "displacement, diversity, and mobility:       #
# careeer impacts of japanese american internment" by jaime arellano-bover     #
# posted here: https://www.jarellanobover.com/research                         #
# translation was helped by the stata to r code translator chatgpt model by    #
# joseph noonan https://chatgpt.com/g/g-ddlktfvbt-stata-to-r-code-translator   #
#------------------------------------------------------------------------------#

library(tidyverse)
library(haven)  # for reading stata data_raw files

# Load dataset
path <- "~/Research/Internment/data/"

# variables defined by column ranges in original file
column_positions <- fwf_widths(
  c(10,             8,                 1,             1,                 1,
    5,              1,                 1,             1,                 1,
    1,              1,                 2,             1,                 1,
    1,              1,                 6,             1,                 1,
    2,              2,                 1,             1,                 1,
    1,              3,                 3,             3,                 3,
    3,              6,                 5),
  col_names =
  c("last_name",    "first_name",      "initial",     "camp",            "assembly",
    "prev_address", "brth_cntry_pnts", "fath_occ_us", "fath_occ_abroad", "school_jap",
    "school_jap2",  "high_educ",       "arrival_us",  "length_jap",      "times_jap",
    "age_jap",      "military",        "ind_number",  "sex_marrge",      "race",
    "birth_yr",     "birthplace",      "japsch_ssn",  "educ",            "lang",
    "rel",          "qual_occ_1",      "qual_occ_2",  "qual_occ_3",      "pot_occ_1",
    "pot_occ_2",    "file_no",         "file_no_2"))

fw_data <- read_fwf(paste0(path, "WRA.FORM26.PU.txt"), column_positions)

data_raw <- fw_data |>
  filter(ind_number != "") |> # Drop records with empty ind_number
  # Handle duplicates
  group_by(ind_number, file_no, first_name, last_name) |>
  mutate(dup_n = row_number(), .after = last_name) |>
  ungroup() |>
  filter(dup_n == 1) |>
  select(-dup_n)

data_labelled <- data_raw |>
  mutate(
    camp = labelled(as.integer(camp),
                    c("Manzanar"       = 1,
                      "Poston"         = 2,
                      "Gila River"     = 3,
                      "Tule Lake"      = 4,
                      "Minidoka"       = 5,
                      "Topaz"          = 6,
                      "Heart Mountain" = 7,
                      "Granada"        = 8,
                      "Rohwer"         = 9,
                      "Jerome"         = 10 )
                    ),
    assembly = case_when(assembly == "-" ~ "10",
                         assembly == "A" ~ "11",
                         assembly == "B" ~ "12",
                         assembly == "C" ~ "13",
                         assembly == "D" ~ "14",
                         assembly == "E" ~ "15",
                         assembly == "F" ~ "16",
                         assembly %in% c("T", "Z") ~ "",
                         .default = assembly),
    assembly = labelled(as.integer(assembly),
                        c("none"        = 0 , 
                          "Manzanar"    = 1 , 
                          "Fresno"      = 2 , 
                          "Marysville"  = 3 , 
                          "Mayor"       = 4 , 
                          "Merced"      = 5 , 
                          "Pinedale"    = 6 , 
                          "Pomona"      = 7 , 
                          "Portland"    = 8 , 
                          "Puyallup"    = 9 , 
                          "Sacramento"  = 10, 
                          "Salinas"     = 11, 
                          "Santa Anita" = 12, 
                          "Stockton"    = 13, 
                          "Tanforan"    = 14, 
                          "Tulare"      = 15, 
                          "Turlock"     = 16 )
                        ),
    sex = case_when(sex_marrge %in% c("1", "2", "3", "4", "5", "0") ~ 1, # men
                    sex_marrge %in% c("6", "7", "8", "9", "-", "&") ~ 2, # women
                    .default = NA),
    sex = labelled(as.integer(sex), c("male" = 1, "female" = 2)),
    marst = case_when(sex_marrge %in% c("1", "6") ~ 1, # single
                      sex_marrge %in% c("2", "9") ~ 2, # married
                      sex_marrge %in% c("3", "8") ~ 3, # widowed
                      sex_marrge %in% c("4", "9") ~ 4, # divorced
                      sex_marrge == "5" ~ 5, # separated
                      .default = NA),
    marst = labelled(as.integer(marst), c("single" = 1,
                                          "married" = 2,
                                          "widowed" = 3,
                                          "divorced" = 4,
                                          "separated" = 5)),
    birthyr = if_else(as.integer(birth_yr) > 42,
                      as.integer(birth_yr) + 1800, # born after 1842
                      as.integer(birth_yr) + 1900), # born before or during 1942
    race = case_when(race %in% c("4", "7", "L", "O", "V", "5", "J", "M", "P", "W", "6", "K", "N", "Q", "X") ~ 5, # japanese including mixed-race
                     race %in% c("8", "S", "T", "U") ~ 1, # white non-japanese 
                     race %in% c("1", "2", NA) ~ NA), # other race or not specified
    race = labelled(race, c("White" = 1, "Japanese" = 5)),
    bpl_pop = case_when(brth_cntry_pnts %in% c("B", "1", "4", "7", "C") ~ 501, # japan
                        brth_cntry_pnts %in% c("K", "2", "5", "8", "L") ~ 99, # US not specified
                        brth_cntry_pnts %in% c("T", "3", "6", "9", "U") ~ 15, # Hawaii
                        .default = NA), # Hawaii
    bpl_pop = labelled(bpl_pop, c("Japan" = 501, "United States, ns" = 99, "Hawaii" = 15)),
    bpl_mom = case_when(brth_cntry_pnts %in% c("A", "1", "2", "3", "D") ~ 501, # japan
                        brth_cntry_pnts %in% c("J", "4", "5", "6", "M") ~ 99, # US not specified
                        brth_cntry_pnts %in% c("S", "7", "8", "9", "V") ~ 15, # Hawaii
                        .default = NA), # Hawaii
    bpl_mom = labelled(bpl_mom, c("Japan" = 501, "United States, ns" = 99, "Hawaii" = 15)),
    yr_immig = if_else(as.integer(birth_yr) > 42,
                       as.integer(birth_yr) + 1800, # arrived after 1842
                       as.integer(birth_yr) + 1900), # arrived before or during 1942
    nativity = case_when(
      bpl_pop == 1 & bpl_mom == 1 ~ 1,  # both parents native-born
      bpl_pop == 2 & bpl_mom == 1 ~ 2,  # foreign father, native mother
      bpl_pop == 1 & bpl_mom == 2 ~ 3,  # native father, foreign mother
      bpl_pop == 2 & bpl_mom == 2 ~ 4,  # both parents foreign-born
      birthplace %in% 0:74 ~ 5,       # is foreign born themselves
      TRUE ~ NA_real_                    # for any other cases, assign NA
    ),
    educ_bach = ifelse(educ %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "4"), 1, 0),
    bpl = case_when(
      birthplace == 31 ~ 001,  # Alabama	
      birthplace == 81 ~ 002,  # Alaska
      birthplace == 26 ~ 004,  # Arizona
      birthplace == 32 ~ 005,  # Arkansas
      birthplace == 13 ~ 006,  # California
      birthplace == 22 ~ 008,  # Colorado
      birthplace == 61 ~ 009,  # Connecticut
      birthplace == 52 ~ 010,  # Delaware
      birthplace == 51 ~ 011,  # District of Columbia
      birthplace == 53 ~ 012,  # Florida
      birthplace == 54 ~ 013,  # Georgia
      birthplace %in% 70:74 ~ 015,  # Hawaii
      birthplace == 23 ~ 016,  # Idaho
      birthplace == 41 ~ 017,  # Illinois
      birthplace == 42 ~ 018,  # Indiana
      birthplace == 43 ~ 019,  # Iowa
      birthplace == 44 ~ 020,  # Kansas
      birthplace == 33 ~ 021,  # Kentucky
      birthplace == 34 ~ 022,  # Louisiana
      birthplace == 62 ~ 023,  # Maine
      birthplace == 55 ~ 024,  # Maryland
      birthplace == 63 ~ 025,  # Massachusetts
      birthplace == 45 ~ 026,  # Michigan
      birthplace == 46 ~ 027,  # Minnesota
      birthplace == 35 ~ 028,  # Mississippi
      birthplace == 47 ~ 029,  # Missouri
      birthplace == 24 ~ 030,  # Montana
      birthplace == 48 ~ 031,  # Nebraska
      birthplace == 25 ~ 032,  # Nevada
      birthplace == 64 ~ 033,  # New Hampshire
      birthplace == 56 ~ 034,  # New Jersey
      birthplace == 21 ~ 035,  # New Mexico
      birthplace == 57 ~ 036,  # New York
      birthplace == 58 ~ 037,  # North Carolin
      birthplace == 49 ~ 038,  # North Dakota
      birthplace == 40 ~ 039,  # Ohio
      birthplace == 36 ~ 040,  # Oklahoma
      birthplace == 12 ~ 041,  # Oregon
      birthplace == 59 ~ 042,  # Pennsylvania
      birthplace == 65 ~ 044,  # Rhode Island
      birthplace == 50 ~ 045,  # South Carolina
      birthplace == "4-" ~ 046,  # South Dakota
      birthplace == 37 ~ 047,  # Tennessee
      birthplace == 38 ~ 048,  # Texas
      birthplace == 27 ~ 049,  # Utah
      birthplace == 66 ~ 050,  # Vermont
      birthplace == "5&" ~ 051,  # Virginia
      birthplace == 11 ~ 053,  # Washington
      birthplace == "5-" ~ 054,  # West Virginia
      birthplace == "4-" ~ 055,  # Wisconsin
      birthplace == 28 ~ 056,  # Wyoming
      birthplace == 0 ~ 099,  # United States, ns
      birthplace %in% 90:99 ~ 501, # Japan
      birthplace == 82 ~ 150,  # Canada
      birthplace == 83 ~ 200, # Mexico
      birthplace == 84 ~ 300, # South America
      .default = NA
    ),
  )

# previous address codes transcribed from WRA form 26 documentation
wra_counties <- read_csv("data/WRA_prev_address_list.csv")

nhgis_codes <- read_csv("data/county_codes_cw.csv") |>
  distinct(STATENAM, NHGISNAM, NHGISST, NHGISCTY)

join_counties <- left_join(wra_counties, nhgis_codes,
                           by = c("state" = "STATENAM", "county" = "NHGISNAM") )

data_int <- data_labelled |>
  left_join(join_counties, by = "prev_address") |>
  select(ind_number, state, county, NHGISST, NHGISCTY,
         camp, assembly, race, sex, birthyr, bpl, bpl_pop, bpl_mom, yr_immig, nativity,
         last_name, first_name)

intrn_grps <- data_int |>
  filter(!is.na(state)) |>
  count(state, county, NHGISST, NHGISCTY, sex, birthyr, bpl) |>
  mutate(p = n / sum(n))

write_csv(intrn_grps, file = "data/internment_groups.csv")

intrn_grps |>
  arrange(desc(n)) |>
  head(n=25) |> 
  knitr::kable(format = "org")
