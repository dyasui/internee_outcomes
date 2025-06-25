#------------------------------------------------------------------------------#
#                               README                                         #
# this file was translated from the original stata do file compile_wra.do      #
# from the replication files for "displacement, diversity, and mobility:       #
# careeer impacts of japanese american internment" by jaime arellano-bover     #
# posted here: https://www.jarellanobover.com/research                         #
# translation was helped by the stata to r code translator chatgpt model by    #
# joseph noonan https://chatgpt.com/g/g-ddlktfvbt-stata-to-r-code-translator   #
#------------------------------------------------------------------------------#

# variables defined by column ranges in original file
read_fw_form26 <- function(data_file = "data/WRA.FORM26.PU.txt") {
  library(readr)
  # read data by specifying the width in character columns of each source variable
  col_positions <- fwf_widths(
    c(10,             8,                 1,             1,                 1,
      5,              1,                 1,             1,                 1,
      1,              1,                 2,             1,                 1,
      1,              1,                 6,             1,                 1,
      2,              2,                 1,             1,                 1,
      1,              3,                 3,             3,                 3,
      3,              6,                 5),
    col_names <- c(
      "last_name",    "first_name",      "initial",     "camp",            "assembly",
      "prev_address", "brth_cntry_pnts", "fath_occ_us", "fath_occ_abroad", "school_jap",
      "school_jap2",  "high_educ",       "arrival_us",  "length_jap",      "times_jap",
      "age_jap",      "military",        "ind_number",  "sex_marrge",      "race",
      "birth_yr",     "birthplace",      "japsch_ssn",  "educ",            "lang",
      "rel",          "qual_occ_1",      "qual_occ_2",  "qual_occ_3",      "pot_occ_1",
      "pot_occ_2",    "file_no",         "file_no_2")
  )

  fw_data <- read_fwf(
    file = data_file, col_positions,
    # there are weird characters like &,- etc so default to chars
    col_types = cols(.default = col_character())
  )
  return(fw_data)
}

form26_drop_dup <- function(fw_data) {
  library(dplyr)
  fw_data |>
    filter(ind_number != "") |> # Drop records with empty ind_number
    # Handle duplicates
    group_by(ind_number, file_no, first_name, last_name) |>
    mutate(dup_n = row_number(), .after = last_name) |>
    ungroup() |>
    filter(dup_n == 1) |>
    select(-dup_n)
}

form26_label <- function(raw_data) {
  library(dplyr)
  library(haven)

  geo_dict <- c("00"="United States Unspecified", "11"="Washington", "12"="Oregon", "13"="California",
                "21"="New Mexico", "22"="Colorado", "23"="Idaho", "24"="Montana", "25"="Nevada", "26"="Arizona", "27"="Utah", "28"="Wyoming",
                "31"="Alabama", "32"="Arkansas", "33"="Kentucky", "34"="Louisiana", "36"="Oklahoma", "37"="Tennessee", "38"="Texas",
                "4&"="Wisconsin", "4-"="South Dakota", "40"="Ohio", "41"="Illinois", "42"="Indiana", "43"="Iowa", "44"="Kansas", "45"="Michigan", "46"="Minnesota", "47"="Missouri", "48"="Nebraska", "49"="North Dakota",
                "5&"="West Virginia", "5-"="Virginia", "51"="District of Columbia", "53"="Florida", "54"="Georgia", "55"="Maryland", "56"="New Jersey", "57"="New York", "59"="Pennsylvania",
                "61"="Connecticut", "62"="Maine", "63"="Massachusetts", "64"="New Hampshire", "65"="Rhode Island", "65"="Vermont",
                "70"="Hawaii, Unspecified", "71"="Hawaii County", "72"="Honolulu County", "73"="Kauai County", "74"="Maui County",
                "81"="Alaska", "82"="Canada", "83"="Mexico", "84"="South America",
                "85"="American Samoa", "86"="Guam", "87"="Puerto Rico", "88"="Virgin Islands", "89"="Wake Island",
                "80"="Poland", "8-"="Germany", "8&"="Ireland",
                "90"="Japan, Unspecified", "91"="Sakhalin or Saghalien or Karafuto Is", "92"="Hokkaido or Yezu Is.", "93"="Honshu Northern Division", "94"="Honshu Central Division", "95"="Honshu Central Division", "96"="Honshu Southern Division", "97"="Urban Prefectures (Kyoto, Osaka, and Tokyo)", "98"="Shikoku", "99"="Kyushu", "9-"="Formosa/Taiwan", "9&"="Chosen/Korea",
                "--"="Other")
  
  raw_data |> 
    mutate(
      camp = case_when(camp==1 ~ "Manzanar",
                       camp==2 ~ "Poston",
                       camp==3 ~ "Gila River",
                       camp==4 ~ "Tule Lake",
                       camp==5 ~ "Minidoka",
                       camp==6 ~ "Topaz",
                       camp==7 ~ "Heart Mountain",
                       camp==8 ~ "Granada",
                       camp==9 ~ "Rohwer",
                       camp==0 ~ "Jerome"),
      assembly = case_when(assembly==0 ~ "none",
                           assembly==1 ~ "Manzanar",
                           assembly==2 ~ "Fresno",
                           assembly==3 ~ "Marysville",
                           assembly==4 ~ "Mayor",
                           assembly==5 ~ "Merced",
                           assembly==6 ~ "Pinedale",
                           assembly==7 ~ "Pomona",
                           assembly==8 ~ "Portland",
                           assembly==9 ~ "Puyallup",
                           assembly=="-"~ "Sacramento",
                           assembly=="A"~ "Salinas",
                           assembly=="B"~ "Santa Anita",
                           assembly=="C"~ "Stockton",
                           assembly=="D"~ "Tanforan",
                           assembly=="E"~ "Tulare",
                           assembly=="F"~ "Turlock",
                           .default = NA ),
      sex = case_when(sex_marrge %in% c("1", "2", "3", "4", "5", "0") ~ 1, # men
                      sex_marrge %in% c("6", "7", "8", "9", "-", "&") ~ 2, # women
                      .default = NA),
      sex = labelled(as.integer(sex), c("Male" = 1, "Female" = 2)),
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
                       birthplace %in% c("81", "82", "83", "84", "80", "8-", "8&",
                          "9-", "90", "92", "93", "94", "95", "96", "97", "98",
                          "99") ~ 5,
                       race %in% c("1", "2", NA) ~ NA), # other race or not specified
      race = labelled(race, c("White" = 1, "Japanese" = 5)),
      bpl_pop = case_when(brth_cntry_pnts %in% c("B", "1", "4", "7", "C") ~ 501, # japan
                          brth_cntry_pnts %in% c("K", "2", "5", "8", "L") ~ 99, # US not specified
                          brth_cntry_pnts %in% c("T", "3", "6", "9", "U") ~ 15, # Hawaii
                          .default = NA),
      bpl_pop = labelled(bpl_pop, c("Japan" = 501, "United States, ns" = 99, "Hawaii" = 15)),
      bpl_mom = case_when(brth_cntry_pnts %in% c("A", "1", "2", "3", "D") ~ 501, # japan
                          brth_cntry_pnts %in% c("J", "4", "5", "6", "M") ~ 99, # US not specified
                          brth_cntry_pnts %in% c("S", "7", "8", "9", "V") ~ 15, # Hawaii
                          .default = NA),
      bpl_mom = labelled(bpl_mom, c("Japan" = 501, "United States, ns" = 99, "Hawaii" = 15)),
      yrimmig = if_else(as.integer(arrival_us) > 42,
                        as.integer(arrival_us) + 1800, # arrived after 1842
                        as.integer(arrival_us) + 1900), # arrived before or during 1942
      birth_country = case_when(
        birthplace %in% c("00", "01", "11", "12", "13", "21", "22", "23",
                          "24", "25", "26", "27", "28", "32", "33", "34",
                          "36", "37", "38", "4&", "4-", "40", "41", "42",
                          "42", "43", "44", "45", "46", "47", "48", "49",
                          "5&", "51", "53", "54", "55", "56", "57", "59",
                          "61", "62", "63", "64", "70", "71", "72", "73",
                          "74", "81", "82", "83", "84") ~ "United States",
        birthplace == "80" ~ "Poland",
        birthplace == "8-" ~ "Germany",
        birthplace == "8&" ~ "Ireland",
        birthplace == "82" ~ "Canada",
        birthplace == "83" ~ "Mexico",
        birthplace == "84" ~ "South America",
        birthplace %in% c("9-", "90", "91", "92", "93", "94", "95", "96",
                          "97", "98", "99") ~ "Japan",
        birthplace == "--" ~ "Other",
      ),
      nativity = case_when(
        (birth_country=="United States" & bpl_pop %in% c(15,99) & bpl_mom %in% c(15,99)) ~ 1,  # both parents native-born
        (birth_country=="United States" & bpl_pop == 501 & bpl_mom %in% c(15,99)) ~ 2,  # foreign father, native mother
        (birth_country=="United States" & bpl_pop %in% c(15,99) & bpl_mom == 501) ~ 3,  # native father, foreign mother
        (birth_country=="United States" & bpl_pop == 501 & bpl_mom == 501) ~ 4,  # both parents foreign-born
        birthplace %in% c("81", "82", "83", "84", "80", "8-", "8&",
                          "9-", "90", "92", "93", "94", "95", "96", "97", "98",
                          "99") ~ 5,       # is foreign born themselves
        .default = NA_real_                    # for any other cases, assign NA
      ),
      nativity = labelled(nativity, c("Native born, native parents"=1,
                                      "Native born, native mother"=2,
                                      "Native born, native father"=3,
                                      "Native born, foreign parents"=4,
                                      "Foreign born"=5)),
      generation = case_when(
        (race==5 & nativity == 5) ~ "Issei",
        (race==5 & nativity %in% 2:4) ~ "Nisei",
        (race==5 & nativity == 1) ~ "Sansei",
      ),
      fath_occ_us = if_else(fath_occ_us %in% c("&", "-"), NA, fath_occ_us),
      fath_occ_us = labelled(as.integer(fath_occ_us),
                             c("Professional & semiprofessional"=1,
                               "Managerial and official (exept farm)"=2,
                               "Clerical and sales"=3,
                               "Service"=4,
                               "Farm operators and managers"=5,
                               "Fishermen"=6,
                               "Skilled craftsmen and foremen"=7,
                               "Semi-skilled operators (except farm)"=8,
                               "Unskilled laborers (except farm)"=9
                               )),
      fath_occ_abroad = if_else(fath_occ_abroad %in% c("&", "-"), NA, fath_occ_abroad),
      fath_occ_abroad = labelled(as.integer(fath_occ_abroad),
                                 c("Professional & semiprofessional"=1,
                                   "Managerial and official (exept farm)"=2,
                                   "Clerical and sales"=3,
                                   "Service"=4,
                                   "Farm operators and managers"=5,
                                   "Fishermen"=6,
                                   "Skilled craftsmen and foremen"=7,
                                   "Semi-skilled operators (except farm)"=8,
                                   "Unskilled laborers (except farm)"=9
                                   )),
      school_jap = case_when(
        school_jap == "&" ~ NA,
        school_jap == "0" ~ 0,
        school_jap == "1" ~ 1,
        school_jap == "2" ~ 2,
        school_jap == "3" ~ 3,
        school_jap == "4" ~ 4,
        school_jap == "5" ~ 5,
        school_jap == "6" ~ 6,
        school_jap == "7" ~ 7,
        school_jap == "8" ~ 8,
        school_jap == "9" ~ 9,
        school_jap == "A" ~ 10,
        school_jap == "B" ~ 11,
        school_jap == "C" ~ 12,
        school_jap == "D" ~ 13,
        school_jap == "E" ~ 14,
        school_jap == "F" ~ 15,
        school_jap == "G" ~ 16,
        ),
      degfield = case_when(
        high_educ %in% c("A", "J", "1") ~ 00, # "Not specified",
        high_educ %in% c("B", "K") ~ 11, # "Agriculture",
        high_educ %in% c("C", "L") ~ 60, # "Arts",
        high_educ %in% c("D", "M") ~ 36, # "Biological Sciences",
        high_educ %in% c("E", "N") ~ 24, # "Engineering",
        high_educ %in% c("F", "O") ~ 29, # "Home Economics",
        high_educ %in% c("G", "P") ~ 50, # "Physical Sciences",
        high_educ %in% c("H", "Q") ~ 54, # "Public Health, Hygiene, Physical Ed, Nursing, and Pre-Med.",
        high_educ %in% c("I", "R") ~ 37, # "Social Sciences and Mathematics",
        high_educ %in% "2" ~ 2, # "Divinity, Law, or Other Doctorate",
        high_educ %in% "3" ~ 3, # "Teaching, Nursing, or other Certification",
        .default = NA
      ),
      degfield = labelled(degfield,
                          c("Not specified" = 00, "Agriculture" = 11,
                            "Arts" = 60, "Biological Sciences" = 36,
                            "Engineering" = 24, "Home Economics" = 29,
                            "Physical Sciences" = 50, "Public Health, Hygiene, Physical Ed, Nursing, and Pre-Med."=54,
                            "Social Sciences and Mathematics" = 37,
                            "Divinity, Law, or Other Doctorate" = 2,
                            "Teaching, Nursing, org other Certification"=3)),
      high_deg = case_when(
        high_educ %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I") ~ "bachelors", # bachelors degree
        high_educ %in% c("J", "K", "L", "M", "N", "O", "P", "Q", "R") ~ "masters", # masters degree
        high_educ %in% c("1", "2") ~ "doctorate", # phd or other doctorate
        high_educ == "3" ~ "certificate", # certificate, credential, etc
        ),
      educ = case_when( # highest grade completed or grade attending
        educ %in% "J" ~ 0,
        educ %in% c("S", "K") ~ 1,
        educ %in% c("T", "L") ~ 2,
        educ %in% c("U", "M") ~ 3,
        educ %in% c("V", "N") ~ 4,
        educ %in% c("W", "O") ~ 5,
        educ %in% c("X", "P") ~ 6,
        educ %in% c("Y", "Q") ~ 7,
        educ %in% c("Z", "F") ~ 8,
        educ %in% c("A", "G") ~ 9,
        educ %in% c("B", "H") ~ 10,
        educ %in% c("C", "I") ~ 11,
        educ %in% c("D", "I", "E") ~ 12,
        educ %in% c("1", "5") ~ 14,
        educ %in% c("2", "6") ~ 15,
        educ %in% c("3", "7") ~ 16,
        educ %in% c("4", "8") ~ 17,
        educ %in% c("_", "9") ~ 18,
        .default = NA
      ),
      bpl = case_when( # match birthplace codes from IPUMS
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
        .default = NA),
      bpl = labelled(bpl, c("Alabama" = 001, "Alaska" = 002, "Arizona"
                            = 004, "Arkansas" = 005, "California" = 006, "Colorado" = 008,
      "Connecticut" = 009, "Delaware" = 010, "District of Columbia" =
      011, "Florida" = 012, "Georgia" = 013, "Hawaii" = 015, "Idaho" =
      016, "Illinois" = 017, "Indiana" = 018, "Iowa" = 019, "Kansas" =
      020, "Kentucky" = 021, "Louisiana" = 022, "Maine" = 023,
      "Maryland" = 024, "Massachusetts" = 025, "Michigan" = 026,
      "Minnesota" = 027, "Mississippi" = 028, "Missouri" = 029,
      "Montana" = 030, "Nebraska" = 031, "Nevada" = 032, "New Hampshire" = 033, "New Jersey" = 034, "New Mexico" = 035, "New York" = 036, "North Carolina" = 037, "North Dakota" = 038, "Ohio"
      = 039, "Oklahoma" = 040, "Oregon" = 041, "Pennsylvania" = 042,
      "Rhode Island" = 044, "South Carolina" = 045, "South Dakota" =
      046, "Tennessee" = 047, "Texas" = 048, "Utah" = 049, "Vermont" =
      050, "Virginia" = 051, "Washington" = 053, "West Virginia" =
      054, "Wisconsin" = 055, "Wyoming" = 056, "United states, ns" =
      099, "Japan" = 501, "Canada" = 150, "Mexico" = 200, "South America" = 300) ),
      birthplace = recode(birthplace, !!! geo_dict),
      )
}

get_nhgis_codes <- function(shp) {
  library(dplyr)
  library(ipumsr)

  nhgis_codes <- read_ipums_sf(shp, file_select = matches("us_county_1950")) |>
    sf::st_drop_geometry() |>
    mutate(STATENAM = case_when(
           STATENAM == "Alaska Territory" ~ "Alaska",
           STATENAM == "Hawaii Territory" ~ "Hawaii",
           .default = STATENAM
           )) |>
    select(STATENAM, NHGISNAM ,NHGISST, NHGISCTY)
  return(nhgis_codes)
}

wra_counties <- function(nhgis, addr) {
  # read nhgiscodes from shapefile
  nhgis_codes <- read_csv(nhgis)
  wra_counties <- read_csv(addr)
  join_counties <- inner_join(wra_counties, nhgis_codes,
                              by = c("state" = "STATENAM", "county" = "NHGISNAM") )
  return(join_counties)
}

compile_WRA <- function(file="data/WRA.FORM26.PU.txt",
                        addr) {
  library(dplyr)
  data_labelled <- read_fw_form26(file) |>
    form26_drop_dup() |>
    form26_label()

  data_int <- data_labelled |>
    left_join(addr, by = "prev_address") |>
    select(ind_number, state, county, NHGISST, NHGISCTY,
           camp, assembly, race, sex, birthyr, bpl, birthplace,
           bpl_pop, bpl_mom, yrimmig, nativity, generation,
           degfield, high_deg, educ, 
           fath_occ_us, fath_occ_abroad, school_jap,
           last_name, first_name)
}

count_internees <- function(data,
                            vars=c("state", "county")) {
  library(tidyverse)
  groups <- data |>
    filter(!is.na(state)) |>
    count(across(all_of(vars))) |>
    mutate(p = n / sum(n))
  return(groups)
}
