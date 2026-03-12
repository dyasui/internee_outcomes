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
      "age_jap",      "military",        "ind_number",  "sex_marrge",      "race_raw",
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
    dplyr::filter(
      ind_number != "", # Drop records with empty ind_number
      !str_detect(last_name, "\\\\")
      ) |> 
    # Handle duplicates
    group_by(ind_number, file_no, first_name, last_name) |>
    mutate(dup_n = row_number(), .after = last_name) |>
    dplyr::filter(dup_n == 1) |>
    ungroup() |>
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
  
  int_data <- raw_data |> 
    mutate(
      camp = case_when(
        camp==0 ~ "Jerome",
        camp==1 ~ "Manzanar",
        camp==2 ~ "Poston",
        camp==3 ~ "Gila River",
        camp==4 ~ "Tule Lake",
        camp==5 ~ "Minidoka",
        camp==6 ~ "Topaz",
        camp==7 ~ "Heart Mountain",
        camp==8 ~ "Granada",
        camp==9 ~ "Rohwer",
        camp=="D" ~ "miscoded",
        camp=="I" ~ "miscoded",
        camp=="R" ~ "miscoded"
      ),
      assembly = case_when(
        assembly=="-"~ "Sacramento",
        assembly==0 ~ "none",
        assembly==1 ~ "Manzanar",
        assembly==2 ~ "Fresno",
        assembly==3 ~ "Marysville",
        assembly==4 ~ "Mayor",
        assembly==5 ~ "Merced",
        assembly==6 ~ "Pinedale",
        assembly==7 ~ "Pomona",
        assembly==8 ~ "Portland",
        assembly==9 ~ "Puyallup",
        assembly=="A"~ "Salinas",
        assembly=="B"~ "Santa Anita",
        assembly=="C"~ "Stockton",
        assembly=="D"~ "Tanforan",
        assembly=="E"~ "Tulare",
        assembly=="F"~ "Turlock",
        assembly=="Z" ~ "miscode",
        .default = NA
      ),
      sex = case_when(sex_marrge %in% c("1", "2", "3", "4", "5", "0") ~ 1, # men
                      sex_marrge %in% c("6", "7", "8", "9", "-", "&") ~ 2, # women
                      .default = NA),
      ## sex = labelled(as.integer(sex), c("Male" = 1, "Female" = 2)),
      marst = case_when(sex_marrge %in% c("1", "6") ~ 1, # single
                        sex_marrge %in% c("2", "9") ~ 2, # married
                        sex_marrge %in% c("3", "8") ~ 3, # widowed
                        sex_marrge %in% c("4", "9") ~ 4, # divorced
                        sex_marrge == "5" ~ 5, # separated
                        .default = NA),
      ## marst = labelled(as.integer(marst), c("single" = 1,
      ##                                       "married" = 2,
      ##                                       "widowed" = 3,
      ##                                       "divorced" = 4,
      ##                                       "separated" = 5)),
      birthyr = case_when(as.integer(birth_yr) > 42 ~
                            as.integer(birth_yr) + 1800, # born after 1842
                          as.integer(birth_yr) <= 42 ~ as.integer(birth_yr) + 1900,
                          .default = NA), # born before or during 1942
      race = case_when(
        race_raw %in% c("4", "7", "L", "O", "V", "5", "J", "M", "P", "W",
                    "6", "K", "N", "Q", "X") ~ 5, # japanese including mixed-race
        race_raw %in% c("8", "S", "T", "U") ~ 1, # white non-japanese
        birthplace %in% c("81", "82", "83", "84", "80", "8-", "8&",
                          "9-", "90", "92", "93", "94", "95", "96",
                          "97", "98", "99") ~ 5,
        race_raw %in% c("1", "2") ~ 0 # other race or not specified
      ), 
      ## race = labelled(race, c("White" = 1, "Japanese" = 5, "Other/ns" = 0)),
      bpl_pop = case_when(brth_cntry_pnts %in% c("B", "1", "4", "7", "C") ~ 501, # japan
                          brth_cntry_pnts %in% c("K", "2", "5", "8", "L") ~ 99, # US
                          brth_cntry_pnts %in% c("T", "3", "6", "9", "U") ~ 15, # Hawaii
                          .default = NA),
      ## bpl_pop = labelled(bpl_pop, c("Japan" = 501, "United States" = 99, "Hawaii" = 15)),
      bpl_mom = case_when(brth_cntry_pnts %in% c("A", "1", "2", "3", "D") ~ 501, # japan
                          brth_cntry_pnts %in% c("J", "4", "5", "6", "M") ~ 99, # US
                          brth_cntry_pnts %in% c("S", "7", "8", "9", "V") ~ 15, # Hawaii
                          .default = NA),
      ## bpl_mom = labelled(bpl_mom, c("Japan" = 501, "United States" = 99, "Hawaii" = 15)),
      yrimmig = suppressWarnings(as.integer(arrival_us)),
      yrimmig = case_when(arrival_us %in% 43:99 ~ yrimmig + 1800, # arrived after 1842
                          arrival_us %in% 00:42 ~ yrimmig + 1900,
                          .default = NA,), # arrived before or during 1942
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
      ## nativity = labelled(nativity, c("Native born, native parents"=1,
      ##                                 "Native born, native mother"=2,
      ##                                 "Native born, native father"=3,
      ##                                 "Native born, foreign parents"=4,
      ##                                 "Foreign born"=5)),
      generation = case_when(
      (race==5 & nativity == 5) ~ "Issei",
      (race==5 & nativity %in% 2:4) ~ "Nisei",
      (race==5 & nativity == 1) ~ "Sansei",
      ),
      ## fath_occ_us = labelled(suppressWarnings(as.integer(fath_occ_us)),
      ##                        c("Professional & semiprofessional"=1,
      ##                          "Managerial and official (exept farm)"=2,
      ##                          "Clerical and sales"=3,
      ##                          "Service"=4,
      ##                          "Farm operators and managers"=5,
      ##                          "Fishermen"=6,
      ##                          "Skilled craftsmen and foremen"=7,
      ##                          "Semi-skilled operators (except farm)"=8,
      ##                          "Unskilled laborers (except farm)"=9
      ##                          )),
      ## fath_occ_abroad = labelled(suppressWarnings(as.integer(fath_occ_abroad)),
      ##                            c("Professional & semiprofessional"=1,
      ##                              "Managerial and official (exept farm)"=2,
      ##                              "Clerical and sales"=3,
      ##                              "Service"=4,
      ##                              "Farm operators and managers"=5,
      ##                              "Fishermen"=6,
      ##                              "Skilled craftsmen and foremen"=7,
      ##                              "Semi-skilled operators (except farm)"=8,
      ##                              "Unskilled laborers (except farm)"=9
      ##                              )),
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
      ## degfield = labelled(degfield,
      ##                     c("Not specified" = 00, "Agriculture" = 11,
      ##                       "Arts" = 60, "Biological Sciences" = 36,
      ##                       "Engineering" = 24, "Home Economics" = 29,
      ##                       "Physical Sciences" = 50, "Public Health, Hygiene, Physical Ed, Nursing, and Pre-Med."=54,
      ##                       "Social Sciences and Mathematics" = 37,
      ##                       "Divinity, Law, or Other Doctorate" = 2,
      ##                       "Teaching, Nursing, org other Certification"=3)),
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
      ## bpl = labelled(bpl, c("Alabama" = 001, "Alaska" = 002, "Arizona"
      ##                       = 004, "Arkansas" = 005, "California" = 006, "Colorado" = 008,
      ##                       "Connecticut" = 009, "Delaware" = 010, "District of Columbia" =
      ##                                                                011, "Florida" = 012, "Georgia" = 013, "Hawaii" = 015, "Idaho" =
      ##                                                                                                                         016, "Illinois" = 017, "Indiana" = 018, "Iowa" = 019, "Kansas" =
      ##                                                                                                                                                                                 020, "Kentucky" = 021, "Louisiana" = 022, "Maine" = 023,
      ##                       "Maryland" = 024, "Massachusetts" = 025, "Michigan" = 026,
      ##                       "Minnesota" = 027, "Mississippi" = 028, "Missouri" = 029,
      ##                       "Montana" = 030, "Nebraska" = 031, "Nevada" = 032, "New Hampshire" = 033, "New Jersey" = 034, "New Mexico" = 035, "New York" = 036, "North Carolina" = 037, "North Dakota" = 038, "Ohio"
      ##                       = 039, "Oklahoma" = 040, "Oregon" = 041, "Pennsylvania" = 042,
      ##                       "Rhode Island" = 044, "South Carolina" = 045, "South Dakota" =
      ##                                                                       046, "Tennessee" = 047, "Texas" = 048, "Utah" = 049, "Vermont" =
      ##                                                                                                                              050, "Virginia" = 051, "Washington" = 053, "West Virginia" =
      ##                                                                                                                                                                           054, "Wisconsin" = 055, "Wyoming" = 056, "United states, ns" =
      ##                                                                                                                                                                                                                      099, "Japan" = 501, "Canada" = 150, "Mexico" = 200, "South America" = 300) ),
      birthplace = recode(birthplace, !!! geo_dict),
      )

  occ_codes <- tribble(
    ~Code,  ~occ1950,  ~Description,
    "0-1", 1001, "Artistic work",
    "0-2", 1002, "Musical work",
    "0-3", 1003, "Literary work",
    "0-4", 1004, "Entertainment work",
    "0-6", 1006, "Technical work",
    "0-8", 1008, "Managerial work",
    "1-1", 1011, "Computing work",
    "1-2", 1012, "Recording work",
    "1-4", 1014, "General Clerical work",
    "1-5", 1015, "Public Contact work",
    "2-1", 1021, "Cooking",
    "2-3", 1023, "Child care",
    "2-5", 1025, "Personal Service",
    "3-1", 1031, "Farming",
    "3-8", 1038, "Fishery work",
    "3-9", 1039, "Forestry work",
    "4-2", 1042, "Machine trades",
    "4-6", 1046, "Crafts",
    "6-2", 1062, "Observational work",
    "6-4", 1064, "Manipulative work",
    "6-6", 1066, "Elemental work",
    "001", 000, "Accountants and auditors",
    "002", 001, "Actors and actresses",
    "003", 003, "Architects",
    "004", 004, "Artists, sculptors, and teachers of art",
    "006", 006, "Authors, editors, and reporters",
    "007", 007, "Chemists, assayers, and metallurgists",
    "008", 009, "Clergymen",
    "011", 010, "College presidents, professors, and instructors",
    "012", 1112, "County agents and farm demonstrators",
    "013", 032, "Dentists",
    "015", 042, "Engineers, chemical",
    "016", 043, "Engineers, civil",
    "017", 044, "Engineers, electrical",
    "018", 045, "Engineers, industrial",
    "019", 046, "Engineers, mechanical",
    "020", 048, "Engineers, mining",
    "022", 055, "Lawyers and judges",
    "023", 056, "Librarians",
    "024", 057, "Musicians and teachers of music",
    "025", 073, "Pharmacists",
    "026", 075, "Physicians and surgeons",
    "027", 079, "Social and welfare workers",
    "028", 083, "Statisticians",
    "030", 1130, "Teachers, primary school and kindergarten",
    "031", 1131, "Teachers (secondary school) and principals",
    "032", 093, "Teachers and instructors, n.o.c.",
    "033", 058, "Trained nurses",
    "034", 098, "Veterinarians",
    "038", 099, "Professional occupations, n.o.c.",
    "041", 1141, "Aviators",
    "042", 008,  "Chiropractors",
    "043", 514,  "Decorators and window dressers",
    "044", 1144, "Commercial artists",
    "045", 031,  "Dancers and chorus girls",
    "046", 033, "Designers",
    "048", 035, "Draftsmen",
    "050", 095, "Laboratory technicians and assistants",
    "052", 097, "Healers and medical service occupations, n.e.c.",
    "053", 071, "Optometrists",
    "056", 074, "Photographers",
    "057", 005, "Athletes, sports instructors, and sports officials",
    "061", 076, "Radio operators",
    "062", 1162, "Showmen",
    "064", 092, "Surveyors",
    "065", 054, "Embalmers and undertakers",
    "066", 096, "Technicians, except laboratory",
    "068", 1168, "Semiprofessional occupations, n.e.c.",
    "071", 1171, "Hotel and restaurant managers",
    "072", 290, "Retail managers",
    "073", 1173, "Wholesale managers",
    "074", 200, "Buyers and department heads, stores",
    "075", 205, "Floor men and floor managers, stores",
    "079", 210, "Inspectors, managerial and official",
    "081", 400, "Advertising agents",
    "083", 260, "Officials of lodges, societies, unions, etc.",
    "085", 204, "Credit men",
    "087", 230, "Managers and superintendents, buildings",
    "088", 240, "Ship captains, mates, pilots and engineers",
    "091", 280, "Purchasing agents and buyers, n.o.c.",
    "092", 203, "Conductors, railroad",
    "094", 250, "Public officials, n.o.c.",
    "095", 1195, "Inspectors, public service, n.o.c.",
    "097", 1197, "Judges, court clerks, and auditors",
    "098", 1198, "Managers and officials, n.o.c.",
    "101", 310, "Bookkeepers and cashiers, except bank cashiers",
    "102", 341, "Bookkeeping machine operators",
    "103", 1203, "Clerks",
    "104", 1204, "Clerks, general",
    "105", 1205, "Clerks, general office",
    "106", 1206, "Financial institution clerks, n.o.c.",
    "107", 1207, "Hotel clerks, n.o.c.",
    "108", 1208, "Insurance clerks, n.o.c.",
    "110", 1210, "Printing and publishing clerks, n.o.c.",
    "111", 1211, "Railroad clerks, n.o.c.",
    "112", 1212, "Clerks in trade, n.o.c.",
    "115", 1215, "Collectors, bills and accounts",
    "116", 1216, "Correspondence clerks",
    "117", 1217, "File clerks",
    "118", 1218, "General industry clerks",
    "120", 301, "Library assistants and attendants",
    "123", 340, "Messengers, errand boys, and office boys and girls",
    "124", 360, "Telegraph messengers",
    "125", 1225, "Office machine operators",
    "126", 1226, "Paymasters, payroll clerks, and timekeepers",
    "127", 1227, "Post office clerks",
    "128", 335, "Mail carriers",
    "131", 325, "Express messengers and railway mail clerks",
    "132", 302, "Physicians' and dentists' assistants and attendants",
    "133", 350, "Secretaries",
    "134", 1234, "Shipping and receiving clerks",
    "136", 1236, "Statistical clerks and compilers",
    "137", 1237, "Stenographers and typists",
    "138", 1238, "Stock clerks",
    "141", 365, "Telegraph operators",
    "142", 370, "Telephone operators",
    "143", 1243, "Baggage, transportation",
    "144", 380, "Ticket, station, and express agents, transportation",
    "145", 1245, "Weighers",
    "148", 1248, "Agents and appraisers, n.o.c.",
    "149", 1249, "Clerks and kindred occupations, n.o.c.",
    "151", 410, "Auctioneers",
    "152", 1252, "Salesmen, brokerage and commission firms, n.e.c.",
    "155", 1255, "Canvassers and solicitors",
    "156", 420, "Demonstrators",
    "157", 450, "Salesmen, insurance",
    "158", 460, "Newsboys",
    "161", 430, "Hucksters and peddlers",
    "163", 470, "Salesmen, real estate",
    "165", 480, "Salesmen, stock and bond",
    "170", 490, "Sales clerks",
    "175", 1275, "Salespersons",
    "180", 1280,  "Salesmen, to consumers",
    "185", 1285,  "Salesmen, n.o.c.",
    "186", 1286,  "Salesmen and sales agents, except to consumers",
    "187", 1287,  "Sales clerks, dry cleaning and laundry",
    "197", 1297, "Shoppers",
    "201", 1301, "Day workers",
    "202", 710, "Laundresses, private family",
    "203", 1303, "Housewomen, private family",
    "204", 1304, "Housesman and yardman",
    "205", 1305, "Cooks; domestic",
    "206", 1306, "Maids, general",
    "207", 1307, "Nursomaids",
    "208", 1308, "Parlormaids",
    "209", 1309, "Miscellaneous servants, private family",
    "221", 750,  "Bartenders",
    "222", 1322, "Bellmen and related occupations",
    "223", 752, "Boarding-house and lodging-house keepers",
    "224", 1324, "Maids and housesman, hotels, restaurants, etc.",
    "225", 764, "Housewomen, stewards and hostesses",
    "226", 754, "Cooks, except private family",
    "227", 784, "Waiters and waitresses, except private family",
    "228", 1328, "Ship Stewards",
    "229", 1329, "Kitchen workers in hotels, restaurants, railroads, steamships, etc., n.e.c.",
    "232", 740, "Barbers, beauticians, and manicurists",
    "234", 751, "Bootblacks",
    "236", 1336, "Guides, except hunting and trapping",
    "238", 772, "Midwives and practical nurses",
    "240", 732, "Attendants, recreation and amusement, n.e.c.",
    "242", 730, "Attendants, hospitals and other institutions, n.e.c.",
    "243", 731, "Attendants, professional and personal service, n.o.c.",
    "244", 1344, "Camp attendants",
    "245", 1345, "Doormen",
    "247", 1347, "Apprentices to service occupations",
    "248", 783, "Ushers",
    "261", 763, "Guards and watchmen, except crossing watchmen",
    "262", 1362, "Crossing watchmen and bridge tenders",
    "263", 762, "Firemen, fire department",
    "265", 773, "Policemen and doctivos, except in public service",
    "266", 782, "Sheriffs and bailiffs",
    "268", 1368, "Soldiers, sailors, marines, and coast guards, n.o.c.",
    "282", 753, "Charwomen and cleaners",
    "284", 770, "Janitors and sextons",
    "286", 780, "Porters, n.o.c.",
    "291", 1391, "Pullman porters",
    "292", 1392, "Baggage porters",
    "295", 761, "Elevator operators",
    "301", 1401, "Cash grain farmers",
    "302", 1402, "Cotton farmers",
    "303", 1403, "Crop specialty farmers",
    "304", 1404, "Dairy farmers",
    "305", 1405, "Fruit farmers",
    "306", 100, "General farmers",
    "307", 1407, "Animal and livestock farmers",
    "308", 1408, "Poultry farmers",
    "309", 1409, "Truck farmers",
    "311", 1411, "Farm hands, grain",
    "312", 1412, "Farm hands, cotton",
    "313", 1413, "Farm hands, crop specialty",
    "314", 1414, "Farm hands, dairy",
    "315", 1415, "Farm hands, fruit",
    "316", 1416, "Farm hands, general farms",
    "317", 1417, "Farm hands, animal and livestock",
    "318", 1418, "Farm hands, poultry",
    "319", 1419, "Farm hands, vegetables",
    "330", 1430, "Fruit and vegetable graders and packers",
    "331", 1431, "Blight control laborers and bindwood eradicators",
    "332", 1432, "Irrigation occupations",
    "335", 1435, "Farm mechanics",
    "336", 1436, "Farm couples",
    "337", 123, "Farm managers and foremen",
    "338", 1438, "Nursery operators and florist growers",
    "339", 1439, "Nursery and landscaping laborers",
    "340", 930, "Gardeners and groundskeepers, cemeteries, etc.",
    "341", 1441, "Hatchery men",
    "342", 1442, "Laborers, hatchery",
    "343", 1443, "Stablemen",
    "344", 1444, "Barn bosses",
    "347", 1447, "Cotton ginnors",
    "348", 1448, "Technical agricultural occupations, n.o.c.",
    "349", 1449, "Agricultural occupations, n.o.c.",
    "387", 910, "Fishermen and oystermen",
    "388", 1488, "Sponge and seaweed gatherers",
    "389", 1489, "Fishing (occupations), n.o.c.",
    "391", 1491, "Forestry occupations, n.o.c., logging",
    "396", 1492, "Hunting and trapping, n.o.c.",
    "397", 1497, "Hunters and trappers",
    "401", 500, "Bakers",
    "402", 1502, "Occupations in production of bakery products, n.o.c.",
    "602", 1602, "(semiskilled) Occupations in production of bakery products, n.o.c.",
    "802", 1702, "(unskilled) Occupations in production of bakery products, n.o.c.",
    "403", 1503, "Occupations in production of beverages",
    "603", 1603, "(semiskilled) Occupations in production of beverages",
    "803", 1703, "(unskilled) Occupations in production of beverages",
    "404", 1504, "Occupations in canning and preserving of food",
    "604", 1604, "(semiskilled) Occupations in canning and preserving of food",
    "804", 1704, "(unskilled) Occupations in canning and preserving of food",
    "405", 1505, "Occupations in production of confections",
    "605", 1605, "(semiskilled) Occupations in production of confections",
    "805", 1705, "(unskilled) Occupations in production of confections",
    "406", 1506, "Occupations in processing of dairy products",
    "606", 1606, "(semiskilled) Occupations in processing of dairy products",
    "806", 1706, "(unskilled) Occupations in processing of dairy products",
    "407", 1507, "Millers, grain, flour, food, etc.",
    "607", 1607, "(semiskilled) Millers, grain, flour, food, etc.",
    "408", 1508, "Occupations in production of grain-mill products, n.o.c.",
    "608", 1608, "(semiskilled) Occupations in production of grain-mill products, n.o.c.",
    "808", 1708, "(unskilled) Occupations in production of grain-mill products, n.o.c.",
    "409", 1509, "Occupations in slaughtering and in preparation of meat products",
    "609", 1609, "(semiskilled) Occupations in slaughtering and in preparation of meat products",
    "809", 1809, "(unskilled) Occupations in slaughtering and in preparation of meat products",
    "410", 1510, "Occupations in production of miscellaneous food products",
    "610", 1610, "(semiskilled) Occupations in production of miscellaneous food products",
    "810", 1710, "(unskilled) Occupations in production of miscellaneous food products",
    "412", 1512, "Occupations in manufacture of tobacco products",
    "612", 1612, "(semiskilled) Occupations in manufacture of tobacco products",
    "812", 1712, "(unskilled) Occupations in manufacture of tobacco products",
    "415", 684, "Weavers, textile",
    "416", 543, "Loom fixers",
    "618", 1618, "(semiskilled) Nonprocess occupations, in manufacture of textiles, n.o.c.",
    "818", 1718, "(unskilled) Nonprocess occupations, in manufacture of textiles, n.o.c.",
    "419", 1519, "Occupations in manufacture of textiles, n.o.c.",
    "619", 1619, "(semiskilled) Occupations in manufacture of textiles, n.o.c.",
    "819", 1719, "(unskilled) Occupations in manufacture of textiles, n.o.c.",
    "423", 625,  "Milliners",
    "425", 633,  "Dressmakers and seamstresses",
    "625", 1625, "(semiskilled) Dressmakers and seamstresses",
    "426", 590,  "Tailors and tailoresses",
    "427", 1527, "Occupations in fabrication of textile products, n.o.c.",
    "627", 1627, "(semiskilled) Occupations in fabrication of textile products, n.o.c.",
    "827", 1727, "(unskilled) Occupations in fabrication of textile products, n.o.c.",
    "429", 532,  "Inspectors, scalers, and graders, log and lumber",
    "629", 1632,  "(semikilled) Inspectors, scalers, and graders, log and lumber",
    "430", 950,  "Lumbermen, raftsmen, and woodchoppers",
    "630", 1630,  "(semiskilled) Lumbermen, raftsmen, and woodchoppers",
    "830", 1730,  "(unskilled) Lumbermen, raftsmen, and woodchoppers",
    "431", 1531,  "Sawmill occupations, n.o.c.",
    "631", 1631,  "(semiskilled) Sawmill occupations, n.o.c.",
    "831", 1731,  "(unskilled) Sawmill occupations, n.o.c.",
    "439", 1539,  "Occupations in manufacture of miscellaneous finished lumber products, n.o.c.",
    "639", 1639,  "(semiskilled) Occupations in manufacture of miscellaneous finished lumber products, n.o.c.",
    "839", 1739,  "(unskilled) Occupations in manufacture of miscellaneous finished lumber products, n.o.c.",
    "432", 505,  "Cabinetmakers",
    "444", 512, "Compositors and typesetters",
    "644", 1644, "(semiskilled) Compositors and typesetters",
    "445", 520, "Electrotypers and stereographers",
    "447", 571, "Phtoengravers",
    "448", 575, "Pressmen and plate printers, printing",
    "449", 1529, "Occupations in printing and publishing, n.o.c.",
    "649", 1629, "(semiskilled) Occupations in printing and publishing, n.o.c.",
    "452", 1552, "Occupations in production of paint and varnish",
    "459", 1559, "Occupations in manufacture of leather",
    "659", 1659, "(semiskilled) Occupations in manufacture of leather",
    "460", 582, "Shoemakers and shoe repairmen, not factory",
    "471", 534, "Jewelers, watchmakers, goldsmiths, and silversmiths",
    "475", 544, "Machinists",
    "476", 592, "Toolmakers and die sinkers and setters",
    "480", 591, "Tinsmiths, coppersmiths, and sheet metal workers",
    "481", 561, "Molders",
    "483", 503, "Boilermakers",
    "485", 685, "Welders and flame cutters",
    "486", 501, "Blacksmiths, forgemen, and hammermen",
    "491", 641, "Furnacemen, smeltermen, and pourers",
    "497", 515, "Electricians",
    "508", 563, "Opticians, lens grinders, and polishers",
    "509", 1709, "Occupations in manufacture of fabricated plastic products",
    "512", 572, "Piano and organ tuners",
    "516", 670, "Painters, except construction and maintenence",
    "517", 570, "Pattern and model makers, except paper",
    "518", 634, "Dyers",
    "521", 650, "Miners, and mining-machine operators",
    "525", 510, "Carpenters",
    "526", 511, "Cement and concrete finishers",
    "527", 564, "Painters, construction and maintenance",
    "528", 565, "Paperhangers",
    "530", 574, "Plumbers, gas fitters, and storm fitters",
    "735", 632, "Routemen",
    "536", 1536, "Chauffers and drivers, bus, taxi; truck; and tractor",
    "736", 682, "(semiskilled) Chauffers and drivers, bus, taxi; truck; and tractor",
    "537", 960, "Teamsters",
    "541", 541, "Locomotive engineers",
    "542", 542, "Locomotive firemen",
    "543", 661, "Motormen, street, subway, and elevated railway",
    "555", 562, "Motion picture projectionists",
    "557", 643, "Ocupations in laundering, cleaning, dying, and processing apparel and other articles",
    "558", 1558, "Meatcutters, except in slaughtering and packing houses",
    "760", 621, "Attendants, filling stations and parking lots",
    "572", 583, "Engineers, stationary",
    "580", 545, "Mechanics and repairmen, airplane",
    "581", 550, "Mechanics and repairmen, motor vehicle",
    "583", 554, "Mechanics and repairmen, n.o.c.",
    "785", 920, "Garage laborers and car washers and greasers",
    "586", 671, "Photographic process occupations",
    "591", 1691, "Foremen, manufacturing",
    "594", 1694, "Foremen, construction",
    "595", 1695, "Foremen, transportation, communication, and utilities",
    "597", 1697, "Foremen, services, amusements",
    "599", 1699, "Foremen, n.o.c.",
    )

                                        # Vector for recoding (named vector: names = old codes, values = new codes)
  recode_vec <- setNames(occ_codes$occ1950, occ_codes$Code)
  
                                        # Vector for value labels (names = descriptions, values = new numeric codes)
  label_vec <- setNames(occ_codes$occ1950, occ_codes$Description)

  occ_vars <- names(int_data)[str_detect(names(int_data), "^(qual|pot)_occ_")]

  lbl_data <- int_data |>
    mutate(qual_occ_1 = suppressWarnings(as.integer(qual_occ_1))) |> 
    mutate(across(all_of(occ_vars), ~ recode(.x, !!!recode_vec))) # Recode character codes to new numeric
  ## mutate(across(all_of(occ_vars), ~ labelled(as.integer(.x), labels = label_vec)))  # Apply labels

  return(lbl_data)
}

compile_WRA <- function(wra_file="data/WRA.FORM26.PU.txt",
                        adr_file = "data/WRA_prev_address.csv") {
  library(dplyr)
  # read, clean and label pipeline for wra microdata
  data_labelled <- read_fw_form26(wra_file) |>
    form26_drop_dup() |>
    form26_label() |>
    mutate(
      bpl_grp = group_bpl(bpl),
      byr_grp = group_birthyr(birthyr)
    ) 

  # geography codes to join to prev_addr codes
  addr <- read_csv(adr_file)

  data_int <- data_labelled |>
    left_join(addr, by = "prev_address") |>
    select(
      ind_number, state, county, city,
      camp, assembly, race, sex, birthyr, bpl, birthplace,
      bpl_pop, bpl_mom, yrimmig, nativity, generation,
      degfield, high_deg, educ, 
      bpl_grp, byr_grp, 
      fath_occ_us, fath_occ_abroad, school_jap,
      NHGISST, NHGISCTY, STATEFIP, COUNTYICP,
      last_name, first_name, qual_occ_1, qual_occ_2
    ) |>
    rename_with(toupper,
                c("race", "sex", "birthyr", "bpl",
                  "yrimmig",
                  "nativity", "degfield", "educ")
                )

  # I have to get rid of the haven labels so it can be stored as part of the targets workflow
  final_data <- data_int |>
    mutate(across(where(is.factor), as.character)) |> 
    purrr::modify_if(haven::is.labelled, as.numeric)

  return(final_data)
}
