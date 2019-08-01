

# Outcomes on a page, Journey through the lifecourse ###

######################
# Author: Rich Tyler #
# Date: July 2019    #
######################

rm(list = ls()) #  I have an R profile that loads some functions which you may not have so lets clear the workspace

comp_area <- "England"

library(easypackages)
libraries("png", "grid", "tidyverse", "gridExtra", "fingertipsR", "PHEindicatormethods", "readODS", "readxl")

# library(png)
# library(grid)
# library(plyr)
# library(readr)
# library(readxl)
# library(gridExtra)
# library(tidyverse)
# library(fingertipsR)
# library(readODS)

options(scipen = 999)

# This will be the coordinate system for placing our objects in grid later
vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)

# This capitalises words in a string
capwords = function(s, strict = FALSE) {
  cap = function(s) paste(toupper(substring(s, 1, 1)),{s = substring(s, 2); if(strict) tolower(s) else s},sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}

# We can define some hex colours to use later in our comparisons
better <- "#3ECC26"
no_diff <- "#E7AF27"
worse <- "#CC2629"
not_applic <- "#8E8E8E"
higher = "#BED2FF"
lower = "#5555E6"

# This checks to see if 'Journey through indicators' folder exists in the working directory and if not it will be created.
if (!file.exists("./Journey through indicators")) {
  dir.create("./Journey through indicators")} 

# Read in JSNA logo (only download it if it is not available in your working directory)
if(!file.exists("./Journey through indicators/Research Unit.png")){download.file("https://github.com/psychty/la-ph-walkthrough/raw/master/Research%20Unit.png", "./Journey through indicators/Research Unit.png", mode = 'wb')} # This downloads a png image from the West Sussex JSNA website and saves it to your working directory. The if(!file.exists()) only runs the command if the file does not exist (because we have included an ! at the beginning)
unit_logo = readPNG("./Journey through indicators/Research Unit.png")

# I do not think I can legitimately publically host and make available the png images downloaded from flaticon (contact me and I can email something).

# Read in infographics icons ####
ind_2a_icon = readPNG("./Journey through indicators/png/ind_2a_icon_people.png")
ind_2b_icon = readPNG("./Journey through indicators/png/ind_2b_icon_scales.png")
ind_3_icon = readPNG("./Journey through indicators/png/ind_3_icon_feeding_bottle.png")
ind_5_icon = readPNG("./Journey through indicators/png/ind_5_icon_blocks.png")
ind_6_icon = readPNG("./Journey through indicators/png/ind_6_icon_food_1.png")
ind_7_icon = readPNG("./Journey through indicators/png/ind_7_icon_food_2.png")
ind_8_icon = readPNG("./Journey through indicators/png/ind_8_icon_pencil_rotated.png")
ind_9_icon = readPNG("./Journey through indicators/png/ind_9_icon_piggy_bank.png")
ind_10_icon = readPNG("./Journey through indicators/png/ind_10_icon_transport.png")
ind_11_icon = readPNG("./Journey through indicators/png/ind_11_icon_baby_stroller.png")
ind_12_icon = readPNG("./Journey through indicators/png/ind_12_icon_education.png")
ind_13_icon = readPNG("./Journey through indicators/png/ind_13_icon_wrench.png")
ind_14_icon = readPNG("./Journey through indicators/png/ind_14_icon_money_1.png")
ind_15_icon = readPNG("./Journey through indicators/png/ind_15_icon_money_1.png")
ind_16_icon = readPNG("./Journey through indicators/png/ind_16_icon_for_sale.png")
ind_17_icon = readPNG("./Journey through indicators/png/ind_17_icon_overturned_car.png")
ind_19_icon = readPNG("./Journey through indicators/png/ind_19_icon_glass_bottle.png")
ind_20_icon = readPNG("./Journey through indicators/png/ind_20_icon_running.png")
ind_21_icon = readPNG("./Journey through indicators/png/ind_21_icon_sofa.png")
ind_22_icon = readPNG("./Journey through indicators/png/ind_22_icon_cigarette.png")
ind_23_icon = readPNG("./Journey through indicators/png/ind_23_icon_fast_food.png")
ind_24_icon = readPNG("./Journey through indicators/png/ind_24_icon_bra.png")
ind_25_icon = readPNG("./Journey through indicators/png/ind_25_icon_underwear.png")
ind_26_icon = readPNG("./Journey through indicators/png/ind_26_icon_microscope.png")
# ind_27_icon = readPNG("./Journey through indicators/png/ind_27_icon_blood_sugar.png")
ind_27_icon = readPNG("./Journey through indicators/png/ind_28_icon_heartbeat.png")
ind_28_icon = readPNG("./Journey through indicators/png/ind_29_icon_awareness_ribbon.png")
ind_29_icon = readPNG("./Journey through indicators/png/ind_30_icon_coins_1.png")
ind_30_icon = readPNG("./Journey through indicators/png/ind_31_icon_flame.png")
ind_31_icon = readPNG("./Journey through indicators/png/ind_32_icon_hip.png")
ind_32_icon = readPNG("./Journey through indicators/png/ind_33_icon_glove.png")
ind_33_icon = readPNG("./Journey through indicators/png/ind_34_icon_tombstone.png")
ind_34_icon = readPNG("./Journey through indicators/png/ind_35_icon_tombstone.png")
arrow_left = readPNG("./Journey through indicators/png/arrow_left.png")
arrow_right = readPNG("./Journey through indicators/png/arrow_right.png")
arrow_down = readPNG("./Journey through indicators/png/arrow_down.png")

# Indicator 1 - Infant Mortality ####
indicator_1 <- fingertips_data(IndicatorID = 92196, AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_1_comp <- indicator_1 %>% 
  filter(AreaName == comp_area)

# Indicator 2 - Low birth weight ####
indicator_2 <- fingertips_data(IndicatorID = 20101,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_2_comp <- indicator_2 %>% 
  filter(AreaName == comp_area)

# Indicator 3 - Breastfeeding initiation ####
indicator_3 <- fingertips_data(IndicatorID = 20201, AreaTypeID = 101) %>%  
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_3_comp <- indicator_3 %>% 
  filter(AreaName == comp_area) 

# Indicator 4 - 0-4 in hh with out-of-work- benefits ####

# This is updated once a year in december

if(!file.exists("./Journey through indicators/children-in-out-of-work-households-by-la-may-2017.ods")){
  download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/761934/children-in-out-of-work-households-by-la-may-2017.ods", "./Journey through indicators/children-in-out-of-work-households-by-la-may-2017.ods", mode = "wb")}

if(!file.exists("./Journey through indicators/children-in-out-of-work-households-by-country-may-2017.ods")){
  download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/761937/children-in-out-of-work-households-by-country-may-2017.ods", "./Journey through indicators/children-in-out-of-work-households-by-country-may-2017.ods", mode = "wb")}

indicator_4 <- read_ods("./Journey through indicators/children-in-out-of-work-households-by-la-may-2017.ods", sheet = "Table_1", skip = 10, formula_as_formula = TRUE, col_names = FALSE) ; indicator_4 <- indicator_4[,2:4]
colnames(indicator_4) <- c("Name", "Code", "Children_aged_0-4_poverty")

indicator_4$`Children_aged_0-4_poverty` <- gsub(",", "",indicator_4$`Children_aged_0-4_poverty`)
indicator_4 <- subset(indicator_4, !(is.na(Code)))
indicator_4$`Children_aged_0-4_poverty` <- as.numeric(indicator_4$`Children_aged_0-4_poverty`)
indicator_4$Name <- gsub(" UA", "", indicator_4$Name)

indicator_4_comp <- read_ods("./Journey through indicators/children-in-out-of-work-households-by-country-may-2017.ods", sheet = "Table_1", skip = 8, formula_as_formula = TRUE, col_names = FALSE)
indicator_4_comp <- indicator_4_comp[,2:3]
colnames(indicator_4_comp) <- c("Name", "Children_aged_0-4_poverty")

indicator_4_comp <- indicator_4_comp %>% 
  filter(!(is.na(`Children_aged_0-4_poverty`))) %>% 
  filter(Name == "England") %>% 
  mutate(`Children_aged_0-4_poverty` = as.numeric(`Children_aged_0-4_poverty`))

if (!file.exists("./Journey through indicators/mye_2017.xls")) {
  download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2017/ukmidyearestimates2017finalversion.xls", "./Journey through indicators/mye_2017.xls", mode = "wb")}

mye_2017 <- read_excel("./Journey through indicators/mye_2017.xls",sheet = "MYE2 - All", skip = 4) %>% 
  select(Code, Name, `0`, `1`, `2`, `3`, `4`) %>% 
  mutate(`0_4` = rowSums(.[3:7], na.rm = TRUE)) %>% 
  mutate(Name = capwords(Name, strict = TRUE))

indicator_4 <- indicator_4 %>% 
  left_join(mye_2017[c("Code", "0_4")], by = "Code") %>% 
  mutate(Percentage_children_poverty = `Children_aged_0-4_poverty` / `0_4` * 100) %>% 
  mutate(LCI = PHEindicatormethods:::wilson_lower(`Children_aged_0-4_poverty`, `0_4`, confidence = .95) * 100,
         UCI = PHEindicatormethods:::wilson_upper(`Children_aged_0-4_poverty`, `0_4`, confidence = .95) * 100)

indicator_4_comp <- indicator_4_comp %>% 
  left_join(mye_2017[c("Name", "0_4")], by = "Name") %>% 
  mutate(Percentage_children_poverty = `Children_aged_0-4_poverty` / `0_4` * 100) %>% 
  mutate(LCI = PHEindicatormethods:::wilson_lower(`Children_aged_0-4_poverty`, `0_4`, confidence = .95) * 100,
         UCI = PHEindicatormethods:::wilson_upper(`Children_aged_0-4_poverty`, `0_4`, confidence = .95) * 100)

# Indicator 5 - school Readiness by pupil residency ###
# 2018
download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/759613/EYFSP_2018_additional_tables_underlying_data.zip",  "./Journey through indicators/EYFSP_2018_Tables.zip")
unzip("./Journey through indicators/EYFSP_2018_Tables.zip", exdir = "./Journey through indicators")

indicator_5 <- read_csv("./Journey through indicators/EYFSP_LAD_ pr_additional_tables_2018.csv", col_types = cols(year = col_double(),level = col_character(),country_code = col_character(), country_name = col_character(),  region_code_pr = col_character(),  region_name_pr = col_character(), lad_code_pr = col_character(),lad_name_pr = col_character(),number_of_pupils = col_double(),  elg_number = col_double(),elg_percent = col_double(),gld_number = col_double(), gld_percent = col_double(), point_score = col_double(), average_point_score = col_double()
)) %>%
  mutate(lad_name_pr = ifelse(is.na(region_name_pr), country_name_pr, ifelse(is.na(lad_name_pr), region_name_pr, lad_name_pr))) %>% 
  mutate(lad_code_pr = ifelse(is.na(region_code_pr), country_code_pr, ifelse(is.na(lad_code_pr), region_code_pr, lad_code_pr))) %>% 
  mutate(lad_name_pr = gsub(" UA", "", lad_name_pr)) %>% 
  rename(Area_name = lad_name_pr,
         Area_code = lad_code_pr) %>% 
  select(year, level, Area_code, Area_name, number_of_pupils, gld_number, gld_percent) %>% 
  mutate(gld_percent = gld_number / number_of_pupils * 100,
         gld_lci = PHEindicatormethods:::wilson_lower(gld_number, number_of_pupils, confidence = .95) * 100,
         gld_uci = PHEindicatormethods:::wilson_upper(gld_number, number_of_pupils, confidence = .95) * 100) %>% 
  filter(year == "201718")
  
indicator_5_comp <- indicator_5 %>% 
  filter(level == "Regional") %>% 
  summarise(number_of_pupils = sum(number_of_pupils, na.rm = TRUE),
            gld_number = sum(gld_number, na.rm = TRUE)) %>% 
  mutate(year = "201718",
         level = "England",
         Area_name = "England",
         Area_code = "E92000001") %>%
  mutate(gld_percent = gld_number / number_of_pupils * 100,
         gld_lci = PHEindicatormethods:::wilson_lower(gld_number, number_of_pupils, confidence = .95) * 100,
         gld_uci = PHEindicatormethods:::wilson_upper(gld_number, number_of_pupils, confidence = .95) * 100) %>% 
  select(year, level, Area_code, Area_name, number_of_pupils, gld_number, gld_percent, gld_lci, gld_uci)

# Indicators 6 - Excess weight reception ####
indicator_6 <- fingertips_data(IndicatorID = 20601, AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1] & Sex == "Persons") # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_6_comp <- indicator_6 %>% 
  filter(AreaName == comp_area) 

# Indicator 7 ####
indicator_7 <- fingertips_data(IndicatorID = 20602, AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1] & Sex == "Persons") # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_7_comp <- indicator_7 %>% 
  filter(AreaName == comp_area) 

# Indicator 8 Key stage 2 expected level for reading, writing and mathematics #### 
if (!file.exists("./Journey through indicators/Key_stage_2_underlying_data.zip")) {
  download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/774449/Key_stage_2_underlying_data.zip", "./Journey through indicators/Key_stage_2_underlying_data.zip", mode = "wb")
  unzip("./Journey through indicators/Key_stage_2_underlying_data.zip", exdir = "./Journey through indicators")}

indicator_8 <- read_csv("./Journey through indicators/2018_KS2_PUPRES_CHAR_UD.csv", col_types = c(.default = col_character(), KS2_RWM_ELIG_18 = col_number(), KS2_RWM_EXP_18 = col_number())) %>% 
  filter(is.na(RURALITY)) %>% 
  mutate(LAD_NAME = ifelse(is.na(REGION_NAME), COUNTRY_NAME, ifelse(is.na(LA_NAME), REGION_NAME, ifelse(is.na(LAD_NAME), LA_NAME, LAD_NAME)))) %>% 
  mutate(LAD_CODE = ifelse(is.na(REGION_CODE), COUNTRY_CODE, ifelse(is.na(LA_CODE), REGION_CODE, ifelse(is.na(LAD_CODE), LA_CODE, LAD_CODE)))) %>% 
  rename(Area_Name = LAD_NAME,
         Area_Code = LAD_CODE) %>% 
  select(Area_Code, Area_Name, KS2_RWM_ELIG_18, KS2_RWM_EXP_18) %>%
  mutate(KS2_RWM_ELIG_18 = as.numeric(KS2_RWM_ELIG_18),
         KS2_RWM_EXP_18 = as.numeric(KS2_RWM_EXP_18),
         Percentage_EL = KS2_RWM_EXP_18 / KS2_RWM_ELIG_18 * 100,
         GLD_LCI =  PHEindicatormethods:::wilson_lower(KS2_RWM_EXP_18, KS2_RWM_ELIG_18, confidence = .95) * 100,
         GLD_UCI =  PHEindicatormethods:::wilson_upper(KS2_RWM_EXP_18, KS2_RWM_ELIG_18, confidence = .95) * 100)

indicator_8_comp <- indicator_8 %>% 
  filter(Area_Name == comp_area)

# Indicator 9 - under 16s living in poverty ####
indicator_9 <- fingertips_data(IndicatorID = 10101,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_9_comp <- indicator_9 %>% 
  filter(AreaName == comp_area)

# Indicator 10 - Emergency admissions for intentional self-harm ####
indicator_10 <- fingertips_data(IndicatorID = 21001,  AreaTypeID = 101) %>% 
  filter(Sex == "Persons") %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_10_comp <- indicator_10 %>% 
  filter(AreaName == comp_area) 

# Indicator 11 - Under 18s conceptions ####
indicator_11 <- fingertips_data(IndicatorID = 20401,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_11_comp <- indicator_11 %>% 
  filter(AreaName == comp_area) 

# Indicator 12 - Pupils attaining GCSEs ####
indicator_12 <- fingertips_data(IndicatorID = 92199,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_12_comp <- indicator_12 %>% 
  filter(AreaName == comp_area) 

# Indicator 13 - Youth unemployment rate ####
indicator_13 <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=2092957699,2092957698,2092957697,1879048193...1879048573,1879048583,1879048574...1879048582&date=latest&gender=0&age=11&measure=1&measures=20100&select=date_name,geography_name,gender_name,age_name,geography_code,measure_name,obs_value", col_types = cols(  DATE_NAME = col_character(),  GEOGRAPHY_NAME = col_character(),  GENDER_NAME = col_character(),  AGE_NAME = col_character(),  GEOGRAPHY_CODE = col_character(),  MEASURE_NAME = col_character(),  OBS_VALUE = col_double()))

if (!file.exists("./Journey through indicators/mye_2017.xls")) {
  download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2017/ukmidyearestimates2017finalversion.xls", "./Journey through indicators/mye_2017.xls", mode = "wb")}

mye_2017 <- read_excel("./Journey through indicators/mye_2017.xls",sheet = "MYE2 - All", skip = 4) %>% 
  select(Code,Name,`18`,`19`,`20`,`21`,`22`,`23`,`24`) %>% 
  mutate(`18_24` = rowSums(.[3:ncol(mye_2017)], na.rm = TRUE))

indicator_13 <- indicator_13 %>% 
  left_join(mye_2017[c("Code", "Name","18_24")], by = c("GEOGRAPHY_CODE" = "Code")) %>% 
  mutate(Percentage_claimants = OBS_VALUE / `18_24` * 100,
         claimants_LCI = PHEindicatormethods:::wilson_lower(OBS_VALUE, `18_24`, confidence = .95) * 100,
         claimants_UCI = PHEindicatormethods:::wilson_upper(OBS_VALUE, `18_24`, confidence = .95) * 100)

indicator_13_comp <- indicator_13 %>% 
  filter(GEOGRAPHY_NAME == comp_area) 

# Indicator 14 - Hourly earnings (male) ####
if (!file.exists("./Journey through indicators/PROV - Home Geography Table 8.6a   Hourly pay - Excluding overtime 2018.xls")){
  download.file("https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/placeofresidencebylocalauthorityashetable8/2018provisional/table82018provisional.zip", "./Journey through indicators/Earnings.zip")
  unzip("./Journey through indicators/Earnings.zip", exdir = "./Journey through indicators")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.3b   Basic Pay - Including other pay 2018 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.1a   Weekly pay - Gross 2018.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.1b   Weekly pay - Gross 2018 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.2a   Weekly pay - Excluding overtime 2018.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.2b   Weekly pay - Excluding overtime 2018 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.3a   Basic Pay - Including other pay 2018.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.5b   Hourly pay - Gross 2018 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.4a   Overtime pay 2018.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.5a   Hourly pay - Gross 2018.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.12  Gender pay gap 2018.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.6b   Hourly pay - Excluding overtime 2018 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.7a   Annual pay - Gross 2018.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.7b   Annual pay - Gross 2018 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.8a   Annual pay - Incentive 2018.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.8b   Annual pay - Incentive 2018 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.9a   Paid hours worked - Total 2018.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.9b   Paid hours worked - Total 2018 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.10a   Paid hours worked - Basic 2018.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.10b   Paid hours worked - Basic 2018 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.11a   Paid hours worked - Overtime 2018.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.11b   Paid hours worked - Overtime 2018 CV.xls")
  file.remove("./Journey through indicators/Earnings.zip")}

# Estimates of the change from the previous year are provided for the median and mean. It is important to note that these are not adjusted to account for changes in the composition of the labour market during that period. Such factors can influence the apparent change in medians or means independently of changes in individuals' earnings. For example, when there are more low-paying jobs in the labour market in one year compared to the previous year, this acts to decrease the median. Consequently, care should be taken when drawing conclusions about changes in pay for individuals over time.				

indicator_14 <- read_excel("./Journey through indicators/PROV - Home Geography Table 8.6a   Hourly pay - Excluding overtime 2018.xls", sheet = "Male", col_types = c("text", "text", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "text"), skip = 4) %>% 
  select(1:5) %>% 
  mutate(Timeperiod = "2018")

colnames(indicator_14) <- c("Name", "Code", "N_jobs_thousands", "Median", "Change", "Timeperiod")

indicator_14_SE <- indicator_14 %>% 
  filter(Name == "South East")

indicator_14_England <- indicator_14 %>% 
  filter(Name == "England")

# Indicator 15 - Hourly earnings (female) ####

indicator_15 <- read_excel("./Journey through indicators/PROV - Home Geography Table 8.6a   Hourly pay - Excluding overtime 2018.xls", sheet = "Female", col_types = c("text", "text", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "text"), skip = 4) %>% 
  select(1:5) %>% 
  mutate(Timeperiod = "2018")

colnames(indicator_15) <- c("Name", "Code", "N_jobs_thousands", "Median", "Change", "Timeperiod")

indicator_15_SE <- indicator_15 %>% 
  filter(Name == "South East")

indicator_15_England <- indicator_15 %>% 
  filter(Name == "England")

# Indicator 16 - Ratio of lower quartile house price to lower quartile earnings ####
  download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian/current/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian.xls", "./Journey through indicators/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian.xls", mode = "wb")

indicator_16a <- read_excel("./Journey through indicators/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian.xls",  sheet = "6a", skip = 5) %>% 
  filter(!(is.na(Name)))

indicator_16b <- read_excel("./Journey through indicators/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian.xls",  sheet = "6b", skip = 5) %>% 
  filter(!(is.na(Name)))

indicator_16 <- indicator_16a %>% 
  select(Code,Name,`Year ending Sep 2018`) %>% 
  left_join(indicator_16b[c("Code","Name","2018")], by = c("Code", "Name")) %>% 
  rename(LQ_price = `Year ending Sep 2018`,
         Gross_LQ_Annual_Earnings = `2018`) %>% 
  mutate(Gross_LQ_Annual_Earnings = as.numeric(Gross_LQ_Annual_Earnings)) %>% 
  mutate(Affordability_ratio = LQ_price / Gross_LQ_Annual_Earnings)

rm(indicator_16a,indicator_16b)

indicator_16a_comp <- read_excel("./Journey through indicators/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian.xls", sheet = "1a", skip = 5) %>% 
  filter(!(is.na(Name)))

indicator_16b_comp <- read_excel("./Journey through indicators/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian.xls", sheet = "1b", skip = 5) %>% 
  filter(!(is.na(Name)))

indicator_16_comp <- indicator_16a_comp %>% 
  select(Code,Name,`Year ending Sep 2018`) %>% 
  left_join(indicator_16b_comp[c("Code","Name","2018")], by = c("Code", "Name")) %>% 
  rename(LQ_price = `Year ending Sep 2018`,
         Gross_LQ_Annual_Earnings = `2018`) %>% 
  mutate(Gross_LQ_Annual_Earnings = as.numeric(Gross_LQ_Annual_Earnings)) %>% 
  mutate(Affordability_ratio = LQ_price / Gross_LQ_Annual_Earnings)

indicator_16_SE <- indicator_16_comp %>% 
  filter(Name == "South East")

indicator_16_comp <- indicator_16_comp %>% 
  filter(Name == comp_area)

# This indicator is on fingertips but it is not as up to date as on ONS and also the figure for 2015 has been revised.
# ator_16 <- fingertips_data(IndicatorID = 92620,  AreaTypeID = 101) 
# indicaindictor_16 <- arrange(indicator_16, desc(Timeperiod)) # Order by descending year (latest data on top)
# indicator_16 <- subset(indicator_16, Timeperiod == unique(indicator_16$Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
# 
# indicator_16_comp <- subset(indicator_16, AreaName == comp_area) 
# indicator_16 <- subset(indicator_16, AreaName == ch_area) 

# Indicator 17 - KSI roads ####
indicator_17 <- fingertips_data(IndicatorID = 11001,  AreaTypeID = 101) %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_17_comp <- indicator_17 %>% 
  filter(AreaName == comp_area) 

# Indicator 18 - Violent crime ####
indicator_18 <- fingertips_data(IndicatorID = 11202,  AreaTypeID = 101) %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_18_comp <- indicator_18 %>% 
  filter(AreaName == comp_area) 

# Indicator 19 Hospital admissions for alcohol-related conditions (Narrow), all ages ####
indicator_19 <- fingertips_data(IndicatorID = 91414,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons") %>% 
 arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_19_comp <- indicator_19 %>% 
  filter(AreaName == comp_area) 

# Indicator 20 - Physically active adults (current method) ####
indicator_20 <- fingertips_data(IndicatorID = 93014,  AreaTypeID = 101) %>% 
  filter(Sex == "Persons" & Age == "19+ yrs") %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_20_comp <- indicator_20 %>% 
  filter(AreaName == comp_area) 

# Indicator 21 - Physically inactive adults (current method) ####
indicator_21 <- fingertips_data(IndicatorID = 93015,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons" & Age == "19+ yrs") %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_21_comp <- indicator_21 %>% 
  filter(AreaName == comp_area) 

# Indicator 22 - Adult smoking prevalence ####
indicator_22 <- fingertips_data(IndicatorID = 92443,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons" & Age == "18+ yrs") %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_22_comp <- indicator_22 %>% 
  filter(AreaName == comp_area) 

# Indicator 23 - Excess weight ####
indicator_23 <- fingertips_data(IndicatorID = 93088,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons" & Age == "18+ yrs") %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_23_comp <- indicator_23 %>% 
  filter(AreaName == comp_area) 

# Indicator 24 - Breast screening ####
indicator_24 <- fingertips_data(IndicatorID = 22001,  AreaTypeID = 101) %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_24_comp <- indicator_24 %>% 
  filter(AreaName == comp_area) 

# Indicator 25 - Bowel screening ####
indicator_25 <- fingertips_data(IndicatorID = 91720,  AreaTypeID = 101) %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_25_comp <- indicator_25 %>% 
  filter(AreaName == comp_area) 

# Indicator 26 - Cervical screening ####
indicator_26 <- fingertips_data(IndicatorID = 22002,  AreaTypeID = 101) %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_26_comp <- indicator_26 %>% 
  filter(AreaName == comp_area) 


# download.file("https://files.digital.nhs.uk/72/CB2869/qof-1718-csv.zip", "./Journey through indicators/QOF_diabetes_prevalence.zip", mode = "wb")
# unzip("./Journey through indicators/QOF_diabetes_prevalence.zip", exdir = "./Journey through indicators")
# file.remove("./Journey through indicators/QOF_diabetes_prevalence.zip")
# file.remove("./Journey through indicators/CCG_EXCEPTIONS_EXCLUSIONS.csv")
# file.remove("./Journey through indicators/ACHIEVEMENT.csv")

# indicator_27 <- read_excel("./Journey through indicators/QOF_diabetes_prevalence.xlsx")
# 
# indicator_27 <- fingertips_data(IndicatorID = 93347,  AreaTypeID = 101) #%>% 
#  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
#  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
# 
# indicator_27_comp <- indicator_27 %>% 
#   filter(AreaName == comp_area) 

# Indicator 27 - Mortality from cardiovascular disease ####
indicator_27 <- fingertips_data(IndicatorID = 40401,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons") %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_27_comp <- indicator_27 %>% 
  filter(AreaName == comp_area) 

# Indicator 28 - Mortality from all cancers ####
indicator_28 <- fingertips_data(IndicatorID = 40501,  AreaTypeID = 101) %>% 
  filter(Sex == "Persons") %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_28_comp <- indicator_28 %>% 
  filter(AreaName == comp_area) 

# Indicator 29 - Older people in poverty ####
indicator_29 <- fingertips_data(IndicatorID = 340,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

 
ID2015 <- read_csv("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/467774/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv")

indicator_29_comp <- indicator_29 %>% 
  filter(AreaName == comp_area) %>% 
  mutate(LCI = PHEindicatormethods:::wilson_lower((Value / 100) * sum(ID2015$`Older population aged 60 and over: mid 2012 (excluding prisoners)`), sum(ID2015$`Older population aged 60 and over: mid 2012 (excluding prisoners)`), confidence = .95) * 100,
         UCI = PHEindicatormethods:::wilson_upper((Value/100) * sum(ID2015$`Older population aged 60 and over: mid 2012 (excluding prisoners)`),sum(ID2015$`Older population aged 60 and over: mid 2012 (excluding prisoners)`), confidence = .95) * 100) 

# Indicator 30 - Percentage of households in fuel poverty ####
indicator_30 <- fingertips_data(IndicatorID = 90356,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_30_comp <- indicator_30 %>% 
  filter(AreaName == comp_area) 

# Indicator 31 - Emergency admissions for hip fractures ####
indicator_31 <- fingertips_data(IndicatorID = 41401,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons") %>% 
 arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_31_comp <- indicator_31 %>% 
  filter(AreaName == comp_area) 

# Indicator 32 - Excess winter deaths index ####
indicator_32 <- fingertips_data(IndicatorID = 90641,  AreaTypeID = 101) %>% 
  filter(Sex == "Persons") %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_32_comp <- indicator_32 %>% 
   filter(AreaName == comp_area) 

# Indicator 33 - Male Life Expectancy ####
indicator_33 <- fingertips_data(IndicatorID = 90366,  AreaTypeID = 101) %>% 
  filter(Sex == "Male") %>% 
  arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_33_comp <- indicator_33 %>% 
  filter(AreaName == comp_area) 

# Indicator 34 - Female Life Expectancy ####
indicator_34 <- fingertips_data(IndicatorID = 90366,  AreaTypeID = 101) %>% 
  filter(Sex == "Female") %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value

indicator_34_comp <- indicator_34 %>% 
  filter(AreaName == comp_area) 

# Chosen area ####

#areas <- c("Medway","Bracknell Forest","West Berkshire","Reading","Slough","Windsor and Maidenhead","Wokingham","Milton Keynes","Brighton and Hove","Portsmouth","Southampton","Aylesbury Vale","Chiltern","South Bucks","Wycombe","Eastbourne","Hastings","Lewes","Rother","Wealden","Basingstoke and Deane","East Hampshire","Eastleigh","Fareham","Gosport","Hart","Havant","New Forest","Rushmoor","Test Valley","Winchester","Ashford","Canterbury","Dartford","Dover","Gravesham","Maidstone","Sevenoaks","Shepway","Swale","Thanet","Tonbridge and Malling","Tunbridge Wells","Cherwell","Oxford","South Oxfordshire","Vale of White Horse","West Oxfordshire","Elmbridge","Epsom and Ewell","Guildford","Mole Valley","Reigate and Banstead","Runnymede","Spelthorne","Surrey Heath","Tandridge","Waverley","Woking","Adur","Arun","Chichester","Crawley","Horsham","Mid Sussex","Worthing")

LA_name_code <- read_csv("https://opendata.arcgis.com/datasets/a267b55f601a4319a9955b0197e3cb81_0.csv")

areas <- c("Adur", "Arun", "Chichester", "Crawley", "Horsham", "Mid Sussex","Worthing","Brighton and Hove")
 i = 1

for (i in 1:length(areas)){
  ch_area <- areas[i]

ch_code <- as.character(subset(LA_name_code, LAD17NM == ch_area, select = "LAD17CD")) # this will look up the name of the area (ch_area) on the file specified within read_csv (which is a lookup file from ONS) and find the code of the area

ch_label <-  ifelse(nchar(ch_area) > 10,sub('(.{1,10})(\\s|$)', '\\1\n', ch_area),ch_area)
data_show <-  sub('(.{1,15})(\\s|$)', '\\1\n', paste("Data are shown\nfor ", ch_area ," \nand are compared\nwith ", comp_area, sep = ""))

# Infographic ####
pdf(paste("./Journey through indicators/PH_Outcomes_walkthrough_", ch_area,".pdf" , sep = ""), width = 11.69, height = 8.27)

grid.newpage() # Create a blank page
pushViewport(viewport(layout = grid.layout(30, 20))) # Define grid layout for page (e.g. think like excel, 30 cells by 20 cells)

# Header
grid.rect(x = unit(0.5, "npc"), y = unit(0.95, "npc"), width = unit(1.1, "npc"), height = unit(0.12, "npc"), just = "centre", hjust = NULL, vjust = NULL,default.units = "npc", name = NULL, gp=gpar(fill = "#333333", col = "#333333"), draw = TRUE, vp = NULL)
grid.text(paste("West Sussex Public Health Outcomes (Data correct as of ", format(Sys.Date(), "%B %Y"), ")", sep = ""), just = "left",  x = unit(0.025, "npc"), y = unit(.97, "npc"), gp = gpar(col = "#ffffff", fontsize = "12", fontface = "bold"))

grid.text(paste(ch_label), just = "left", x = unit(0.025, "npc"), y = unit(ifelse(nchar(ch_area) > 10,.93,.94), "npc"), gp = gpar(col = "#ffffff", fontsize = ifelse(nchar(ch_area) > 10,"13","18"), fontface = "bold"))

grid.text("Note: At Lower Tier Authority Level some outcomes\nare based on small numbers / sample sizes", just = "left", x = unit(0.15, "npc"), y = unit(.93, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "italic"))
grid.text("Some issues affect people of all ages, outcomes have been\nplaced in the life stage where the impact may be greatest.", just = "left", x = unit(0.38, "npc"), y = unit(.93, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "italic"))
grid.text(data_show, just = "left", y = unit(.94, "npc"), x = unit(0.65, "npc"), gp = gpar(col = "#ffffff", fontsize = "8"))

grid.clip(x = 0.775,y = 0.94, width = 0.03, just = "left")
grid.circle(x = 0.775, y = 0.94  , r = 0.04, default.units = "npc", name = NULL, gp = gpar(fill = higher, col = "#333333"), draw = TRUE, vp = NULL)
grid.clip(x = 0.775,y = 0.94, width = 0.03, just = "right")
grid.circle(x = 0.775, y = 0.94  , r = 0.04, default.units = "npc", name = NULL, gp = gpar(fill = better, col = "#333333"), draw = TRUE, vp = NULL)
pushViewport(viewport(width = 1, height = 1, clip=TRUE))
grid.text("Better/\nHigher", just = "centre", x = unit(0.775, "npc"), y = unit(.94, "npc"), gp = gpar(col = "#ffffff", fontsize = "8"))
grid.circle(x = 0.835, y = 0.94  , r = 0.04, default.units = "npc", name = NULL, gp = gpar(fill = no_diff, col = "#333333"), draw = TRUE, vp = NULL)
grid.text("No\ndiff.", just = "centre", x = unit(0.835, "npc"), y = unit(.94, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))

grid.clip(x = 0.895, y = 0.94, width = 0.03, just = "left")
grid.circle(x = 0.895, y = 0.94  , r = 0.04, default.units = "npc", name = NULL, gp = gpar(fill = lower, col = "#333333"), draw = TRUE, vp = NULL)
grid.clip(x = 0.895, y = 0.94, width = 0.03, just = "right")
grid.circle(x = 0.895, y = 0.94  , r = 0.04, default.units = "npc", name = NULL, gp = gpar(fill = worse, col = "#333333"), draw = TRUE, vp = NULL)
pushViewport(viewport(width = 1, height = 1, clip=TRUE))
grid.text("Worse/\nLower", just = "centre", x = unit(0.895, "npc"), y = unit(.94, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))
grid.circle(x = 0.955, y = 0.94  , r = 0.04, default.units = "npc", name = NULL, gp = gpar(fill = not_applic, col = "#333333"), draw = TRUE, vp = NULL)
grid.text("Not\napplic.", just = "centre", x = unit(0.955, "npc"),y = unit(.94, "npc"),  gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))

# Dotted lines ##
grid.lines(x = c(0.1,0.962), y = 0.79, default.units = "npc", name =  NULL, gp = gpar(col = "#333333", lty = "dotted", lwd = 2.5))
grid.lines(x = 0.962, y = c(0.6,0.79), default.units = "npc", name =  NULL, gp = gpar(col = "#333333", lty = "dotted", lwd = 2.5))
grid.lines(x = c(0.05,0.962), y = 0.6, default.units = "npc", name =  NULL, gp = gpar(col = "#333333", lty = "dotted", lwd = 2.5))
grid.lines(x = 0.05, y = c(0.4,0.6), default.units = "npc", name =  NULL, gp = gpar(col = "#333333", lty = "dotted", lwd = 2.5))
grid.lines(x = c(0.05,0.962), y = 0.4, default.units = "npc", name =  NULL, gp = gpar(col = "#333333", lty = "dotted", lwd = 2.5))
grid.lines(x = 0.962, y = c(0.2,0.4), default.units = "npc", name =  NULL, gp = gpar(col = "#333333", lty = "dotted", lwd = 2.5))
grid.lines(x = c(0.05,0.962), y = 0.2, default.units = "npc", name =  NULL, gp = gpar(col = "#333333", lty = "dotted", lwd = 2.5))

# Circles and arrows
grid.circle(x = 0.11, y = 0.79  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_right, x = unit(0.11, "npc"), y = unit(0.79, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.375, y = 0.79  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_right, x = unit(0.375, "npc"), y = unit(0.79, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.685, y = 0.79  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_right, x = unit(0.685, "npc"), y = unit(0.79, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.962, y = 0.79  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_down, x = unit(0.962, "npc"), y = unit(0.79, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.962, y = 0.6  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_left, x = unit(0.962, "npc"), y = unit(0.6, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.665, y = 0.6  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_left, x = unit(0.665, "npc"), y = unit(0.6, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.31, y = 0.6  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_left, x = unit(0.31, "npc"), y = unit(0.6, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.05, y = 0.6  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_down, x = unit(0.05, "npc"), y = unit(0.6, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.05, y = 0.4  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_right, x = unit(0.05, "npc"), y = unit(0.4, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.33, y = 0.4  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_right, x = unit(0.33, "npc"), y = unit(0.4, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.68, y = 0.4  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_right, x = unit(0.68, "npc"), y = unit(0.4, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.962, y = 0.4  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_down, x = unit(0.962, "npc"), y = unit(0.4, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.962, y = 0.2  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_left, x = unit(0.962, "npc"), y = unit(0.2, "npc"),  just = "centre", width = .0175)
grid.circle(x = 0.315, y = 0.2  , r = 0.02, default.units = "npc", name = NULL, gp = gpar(fill = "#333333", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(arrow_left, x = unit(0.315, "npc"), y = unit(0.2, "npc"),  just = "centre", width = .0175)

# Stages ##

#  Pre-birth to Early years ##
grid.circle(x = 0.05, y = 0.79  , r = 0.06, default.units = "npc", name = NULL, gp = gpar(fill = "#000000"), draw = TRUE, vp = NULL)
grid.text("Pre-birth\nto Early\nYears", just = "centre", x = unit(0.05, "npc"), y = unit(.79, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))

# indicator 1 - infant mortality
indicator_1_ch <- subset(indicator_1, AreaName == ch_area) 
# Lower is better
indicator_1_colour <- ifelse(is.na(indicator_1_ch$LowerCI95.0limit), not_applic, ifelse(indicator_1_ch$LowerCI95.0limit > indicator_1_comp$UpperCI95.0limit, worse, ifelse(indicator_1_ch$UpperCI95.0limit < indicator_1_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.15, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_1_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.text("Infant\nMortality", just = "centre", x = unit(0.15, "npc"), y = unit(.835, "npc"), gp = gpar(col = "#ffffff", fontsize = "7"))
grid.text(ifelse(is.na(indicator_1_ch$Value),"-",paste(round(indicator_1_ch$Value,0), " per 1,000", sep = "")), just = "left", x = unit(0.13, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_1_ch$Value),"Data on\ninfant mortality\nunavailable",paste("Rate of deaths in\ninfants aged under\n1 year per 1,000 live\nbirths (", as.character(indicator_1_ch$Timeperiod), ")\n(", indicator_1_ch$Count, " deaths)", sep = "")), just = "left", x = unit(0.13, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 2 - Low birth weight babies
indicator_2_ch <- subset(indicator_2, AreaName == ch_area) 
# Lower is better
indicator_2_colour <- ifelse(is.na(indicator_2_ch$LowerCI95.0limit), not_applic, ifelse(indicator_2_ch$LowerCI95.0limit > indicator_2_comp$UpperCI95.0limit, worse, ifelse(indicator_2_ch$UpperCI95.0limit < indicator_2_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.24, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_2_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_2a_icon, x = unit(0.233, "npc"), y = unit(0.835, "npc"),  just = "centre", width = .025)
grid.raster(ind_2b_icon, x = unit(0.247, "npc"), y = unit(0.845, "npc"),  just = "centre", width = .0125)
grid.text(ifelse(is.na(indicator_2_ch$Value),"-",paste(indicator_2_ch$Count, " (",round(indicator_2_ch$Value,1),"%)", sep = "")), just = "left", x = unit(0.22, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_2_ch$Value),"Data on\nlow birth weight\nunavailable",paste("babies born\nin ", as.character(indicator_2_ch$Timeperiod), " had a\nLOW BIRTHWEIGHT\n(< 2500g)", sep = "")), just = "left", x = unit(0.22, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 3 - Breast feeding initiation
indicator_3_ch <- subset(indicator_3, AreaName == ch_area) 
# Higher is better
indicator_3_colour <- ifelse(is.na(indicator_3_ch$LowerCI95.0limit), not_applic, ifelse(indicator_3_ch$LowerCI95.0limit > indicator_3_comp$UpperCI95.0limit, better, ifelse(indicator_3_ch$UpperCI95.0limit < indicator_3_comp$LowerCI95.0limit, worse, no_diff)))

grid.circle(x = 0.33, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_3_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_3_icon, x = unit(0.33, "npc"), y = unit(0.835, "npc"),  just = "centre", width = .025)

grid.text(ifelse(is.na(indicator_3_ch$Value),"-",paste(round(indicator_3_ch$Value,1), "%", sep = "")), just = "left", x = unit(0.31, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_3_ch$Value),"Data on\nbreast feeding\ninitiation\nunavailable",paste("of mothers\nbreastfeed their\nbabies in the first\n48hrs after delivery\nin ", as.character(indicator_3_ch$Timeperiod), sep = "")), just = "left", x = unit(0.31, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 4 - 0 to 4 year olds in households with an adult of out of work benefits #
indicator_4_ch <- subset(indicator_4, Name == ch_area)
indicator_4_colour <- ifelse(is.na(indicator_4_ch$LCI), not_applic, ifelse(indicator_4_ch$LCI > indicator_4_comp$UCI, worse, ifelse(indicator_4_ch$UCI < indicator_4_comp$LCI, better, no_diff)))

grid.circle(x = 0.42, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_4_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.text("Out of\nwork\nBenefits", just = "centre", x = unit(0.42, "npc"), y = unit(.835, "npc"), gp = gpar(col = "#ffffff", fontsize = "6"))
grid.text(ifelse(is.na(indicator_4_ch$Percentage_children_poverty),"-",paste(round(indicator_4_ch$Percentage_children_poverty,1), "%", sep = "")), just = "left", x = unit(0.4, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_4_ch$Percentage_children_poverty),"Data\nunavailable","of 0-4 year olds in\nhouseholds with an\nadult of out-of-work\nbenefits in May 2017."), just = "left", x = unit(0.4, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 5 - School readiness at the end of reception
indicator_5_ch <- subset(indicator_5, Area_name == ch_area)
indicator_5_colour <- ifelse(is.na(indicator_5_ch$gld_lci), not_applic, ifelse(indicator_5_ch$gld_lci > indicator_5_comp$gld_uci, worse, ifelse(indicator_5_ch$gld_uci < indicator_5_comp$gld_lci, better, no_diff)))

grid.circle(x = 0.51, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_5_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_5_icon, x = unit(0.51, "npc"), y = unit(0.835, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_5_ch$gld_percent),"-",paste(round(indicator_5_ch$gld_percent,1), "%", sep = "")), just = "left", x = unit(0.49, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_5_ch$gld_percent), "Data\nunavailable",paste("of children assessed\nas achieving a good\nlevel of development\n(being 'School Ready')\nat the end of reception\nin 2017/18")), just = "left", x = unit(0.49, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# School years ##
grid.circle(x = 0.625, y = 0.79  , r = 0.06, default.units = "npc", name = NULL, gp = gpar(fill = "#000000"), draw = TRUE, vp = NULL)
grid.text("School\nYears", just = "centre", x = unit(0.625, "npc"), y = unit(.79, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))

# indicator 6 - Excess weight reception
indicator_6_ch <- subset(indicator_6, AreaName == ch_area) 
indicator_6_colour <- ifelse(is.na(indicator_6_ch$LowerCI95.0limit), not_applic, ifelse(indicator_6_ch$LowerCI95.0limit > indicator_6_comp$UpperCI95.0limit, worse, ifelse(indicator_6_ch$UpperCI95.0limit < indicator_6_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.725, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_6_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_6_icon, x = unit(0.725, "npc"), y = unit(0.835, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_6_ch$Value),"-",paste(round(indicator_6_ch$Value,1), "%", sep = "")), just = "left", x = unit(0.705, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_6_ch$Value), "Data\nunavailable",paste("of reception aged\npupils (4/5 years)\nmeasured as having\nExcess Weight\nin ", indicator_6_ch$Timeperiod, sep = "")), just = "left", x = unit(0.705, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 7 - Excess weight year six
indicator_7_ch <- subset(indicator_7, AreaName == ch_area) 
indicator_7_colour <- ifelse(is.na(indicator_7_ch$LowerCI95.0limit), not_applic, ifelse(indicator_7_ch$LowerCI95.0limit > indicator_7_comp$UpperCI95.0limit, worse, ifelse(indicator_7_ch$UpperCI95.0limit < indicator_7_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.815, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_7_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_7_icon, x = unit(0.815, "npc"), y = unit(0.835, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_7_ch$Value),"-",paste(round(indicator_7_ch$Value,1), "%", sep = "")), just = "left", x = unit(0.795, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_7_ch$Value), "Data\nunavailable",paste("of Year 6 pupils\n(aged 10/11 years)\nmeasured as having\nExcess Weight\nin ", indicator_7_ch$Timeperiod, sep = "")), just = "left", x = unit(0.795, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 8 - KS2 attainment reading, writing and maths
indicator_8_ch <- subset(indicator_8, Area_Name == ch_area)
# Higher is better
indicator_8_colour <- ifelse(is.na(indicator_8_ch$GLD_LCI), not_applic, ifelse(indicator_8_ch$GLD_LCI > indicator_8_comp$GLD_UCI, better, ifelse(indicator_8_ch$GLD_UCI < indicator_8_comp$GLD_LCI, worse, no_diff)))

grid.circle(x = 0.905, y = 0.84  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_8_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_8_icon, x = unit(0.905, "npc"), y = unit(0.845, "npc"),  just = "centre", width = .03)
grid.text("KS2", just = "centre", x = unit(0.905, "npc"), y = unit(.825, "npc"), gp = gpar(col = "#ffffff", fontsize = "7", fontface = "bold"))
grid.text(ifelse(is.na(indicator_8_ch$Percentage_EL),"-",paste(round(indicator_8_ch$Percentage_EL,1), "%", sep = "")), just = "left", x = unit(0.88, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_8_ch$Percentage_EL), "Data\nunavailable",paste("of pupils attain the\nexpected levels at\nKey stage 2 for\nReading, Writing and\nMathematics in 2018")), just = "left", x = unit(0.88, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 9 - Children in poverty (under 16s)
indicator_9_ch <- subset(indicator_9, AreaName == ch_area) 
# Lower is better
indicator_9_colour <- ifelse(is.na(indicator_9_ch$LowerCI95.0limit), not_applic, ifelse(indicator_9_ch$LowerCI95.0limit > indicator_9_comp$UpperCI95.0limit, worse, ifelse(indicator_9_ch$UpperCI95.0limit < indicator_9_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.89, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_9_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_9_icon, x = unit(0.89, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_9_ch$Value),"-",paste(round(indicator_9_ch$Value,1), "%", sep = "")), just = "left", x = unit(0.87, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_9_ch$Value), "Data\nunavailable",paste("of children aged\nunder 16 years\nlived in poverty\nin ", indicator_9_ch$Timeperiod, sep = "")), just = "left", x = unit(0.87, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 10 - Emergency admissions for self harm
indicator_10_ch <- subset(indicator_10, AreaName == ch_area) 
# Lower is better
indicator_10_colour <- ifelse(is.na(indicator_10_ch$LowerCI95.0limit), not_applic, ifelse(indicator_10_ch$LowerCI95.0limit > indicator_10_comp$UpperCI95.0limit, worse, ifelse(indicator_10_ch$UpperCI95.0limit < indicator_10_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.8, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_10_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_10_icon, x = unit(0.8, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_10_ch$Value),"-",paste(round(indicator_10_ch$Value,0), " per\n100,000", sep = "")), just = "left", x = unit(0.78, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_10_ch$Value), "Data\nunavailable",paste("Emergency Hospital\nAdmissions for\nIntentional Self-\nHarm in ", indicator_10_ch$Timeperiod)), just = "left", x = unit(0.78, "npc"), y = unit(.5475, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 11 - Teenage pregnancy
indicator_11_ch <- subset(indicator_11, AreaName == ch_area) 
# Lower is better
indicator_11_colour <- ifelse(is.na(indicator_11_ch$LowerCI95.0limit), not_applic, ifelse(indicator_11_ch$LowerCI95.0limit > indicator_11_comp$UpperCI95.0limit, worse, ifelse(indicator_11_ch$UpperCI95.0limit < indicator_11_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.705, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_11_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_11_icon, x = unit(0.705, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_11_ch$Value),"-",paste(round(indicator_11_ch$Value,0), " per\n1,000", sep = "")), just = "left", x = unit(0.69, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_11_ch$Value), "Data\nunavailable",paste("In ", indicator_11_ch$Timeperiod, ", ", round(indicator_11_ch$Count,0)," young\nwomen under 18 years\nbecame pregnant.", sep = "")), just = "left", x = unit(0.69, "npc"), y = unit(.5475, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 12 - GCSE five A*-Cs
indicator_12_ch <- subset(indicator_12, AreaName == ch_area) 
# Higher is better
indicator_12_colour <- ifelse(is.na(indicator_12_ch$LowerCI95.0limit), not_applic, ifelse(indicator_12_ch$LowerCI95.0limit > indicator_12_comp$UpperCI95.0limit, better, ifelse(indicator_12_ch$UpperCI95.0limit < indicator_12_comp$LowerCI95.0limit, worse, no_diff)))

grid.circle(x = 0.61, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_12_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_12_icon, x = unit(0.61, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_12_ch$Value),"-",paste(round(indicator_12_ch$Value ,1), "%", sep = "")), just = "left", x = unit(0.592, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_12_ch$Value), "Data\nunavailable",paste("of pupils attained\nat least five GCSE\ngrades at A*-C including\nEngland  and maths\nin ", indicator_12_ch$Timeperiod, sep = "")), just = "left", x = unit(0.592, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# Early working life ##
grid.circle(x = 0.53, y = 0.6  , r = 0.06, default.units = "npc", name = NULL, gp = gpar(fill = "#000000"), draw = TRUE, vp = NULL)
grid.text("Early\nWorking\nLife", just = "centre", x = unit(0.53, "npc"), y = unit(.6, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))

# indicator 13 - Youth claimant rate
indicator_13_ch <- subset(indicator_13, GEOGRAPHY_NAME == ch_area) 
# Lower is better
indicator_13_colour <- ifelse(is.na(indicator_13_ch$claimants_LCI), not_applic, ifelse(indicator_13_ch$claimants_LCI > indicator_13_comp$claimants_UCI, worse, ifelse(indicator_13_ch$claimants_UCI < indicator_13_comp$claimants_LCI, better, no_diff)))

grid.circle(x = 0.445, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_13_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_13_icon, x = unit(0.445, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_13_ch$Percentage_claimants),"-",paste(round(indicator_13_ch$Percentage_claimants ,1), "%", sep = "")), just = "left", x = unit(0.42, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_13_ch$Percentage_claimants), "Data\nunavailable",paste("Youth claimant rate\n(", indicator_13_ch$OBS_VALUE," young people\naged 18-24 years)\non out-of-work benefits\nin ",indicator_13_ch$DATE_NAME, sep = "")), just = "left", x = unit(0.42, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 14 - Median hourly earnings males
indicator_14_ch <- subset(indicator_14, Name == ch_area)

grid.circle(x = 0.355, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = "#8E8E8E", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_14_icon, x = unit(0.355, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_14_ch$Median),"-",paste("?", str_pad(indicator_14_ch$Median, width = 5, side="right", pad="0"), sep = "")), just = "left", x = unit(0.331, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_14_ch$Median), "Data\nunavailable",paste("Full time hourly gross\nearnings for males\n(median rate) excluding\novertime in ", indicator_14_ch$Timeperiod, "\nEngland: ?", str_pad(indicator_14_England$Median, width = 5, side="right", pad="0"), sep = "")), just = "left", x = unit(0.331, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 15 - median hourly earnings females
indicator_15_ch <- subset(indicator_15, Name == ch_area)

grid.circle(x = 0.265, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = "#8E8E8E", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_15_icon, x = unit(0.265, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_15_ch$Median),"-",paste("?", str_pad(indicator_15_ch$Median,width = 5, side="right", pad="0"), sep = "")), just = "left", x = unit(0.24, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_15_ch$Median), "Data\nunavailable",paste("Full time hourly gross\nearnings for females\n(median rate) excluding\novertime in ", indicator_15_ch$Timeperiod, "\nEngland: ?", str_pad(indicator_15_England$Median, width = 5, side = "right", pad = "0"), sep = "")), just = "left", x = unit(0.24, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 16 - Housing affordability index
indicator_16_ch <- subset(indicator_16, Name == ch_area)

grid.circle(x = 0.175, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = "#8E8E8E", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_16_icon, x = unit(0.175, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_16_ch$Affordability_ratio),"-",paste(round(indicator_16_ch$Affordability_ratio,2), sep = "")), just = "left", x = unit(0.14, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_16_ch$Affordability_ratio), "Data\nunavailable",paste("Housing Affordability\nRatio of lower quartile\nhouse price to lower\nquartile earnings in 2018\n",comp_area, ": ", round(indicator_16_comp$Affordability_ratio,2), sep = "")), just = "left", x = unit(0.14, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 17 KSI
indicator_17_ch <- subset(indicator_17, AreaName == ch_area) 
# Lower is better
indicator_17_colour <- ifelse(is.na(indicator_17_ch$LowerCI95.0limit), not_applic, ifelse(indicator_17_ch$LowerCI95.0limit > indicator_17_comp$UpperCI95.0limit, worse, ifelse(indicator_17_ch$UpperCI95.0limit < indicator_17_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.085, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_17_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_17_icon, x = unit(0.085, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_17_ch$Value),"-",paste(round(indicator_17_ch$Value, 0), " per\n100,000", sep = "")), just = "left", x = unit(0.065, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_17_ch$Value), "Data\nunavailable",paste("Rate of people\nkilled or seriously\ninjured on the roads\nin ",indicator_17_ch$Timeperiod, sep = "")), just = "left", x = unit(0.065, "npc"), y = unit(.5475, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 18 - Violent crime
indicator_18_ch <- subset(indicator_18, AreaName == ch_area) 
# Lower is better
indicator_18_colour <- ifelse(is.na(indicator_18_ch$LowerCI95.0limit), not_applic, ifelse(indicator_18_ch$LowerCI95.0limit > indicator_18_comp$UpperCI95.0limit, worse, ifelse(indicator_18_ch$UpperCI95.0limit < indicator_18_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.1, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_18_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.text("Violent\nCrime", just = "centre", x = unit(0.1, "npc"), y = unit(.45, "npc"), gp = gpar(col = "#ffffff", fontsize = "7"))
grid.text(ifelse(is.na(indicator_18_ch$Value),"-",paste(round(indicator_18_ch$Value, 0), " per 1,000", sep = "")), just = "left", x = unit(0.081, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_18_ch$Value), "Data\nunavailable",paste("Violence against the\nperson (recorded\ncrime data)\n",format(indicator_18_ch$Count, big.mark = ",")," offences\nin ",indicator_18_ch$Timeperiod, sep = "")), just = "left", x = unit(0.081, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 19 - Admission episodes for alcohol related conditions
indicator_19_ch <- subset(indicator_19, AreaName == ch_area) 
# Lower is better
indicator_19_colour <- ifelse(is.na(indicator_19_ch$LowerCI95.0limit), not_applic, ifelse(indicator_19_ch$LowerCI95.0limit > indicator_19_comp$UpperCI95.0limit, worse, ifelse(indicator_19_ch$UpperCI95.0limit < indicator_19_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.19, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_19_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_19_icon, x = unit(0.19, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_19_ch$Value),"-",paste(round(indicator_19_ch$Value, 0), " per\n100,000", sep = "")), just = "left", x = unit(0.16, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_19_ch$Value), "Data\nunavailable",paste("Admission episodes for\nalcohol related conditions\n(narrow definition) ",format(round(indicator_19_ch$Count,0), big.mark = ","),"\nadmissions in ",indicator_19_ch$Timeperiod, sep = "")), just = "left", x = unit(0.16, "npc"), y = unit(.3475, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 20 - Adults undertaking 150 minutes of physical activity
indicator_20_ch <- subset(indicator_20, AreaName == ch_area) 
# Higher is better
indicator_20_colour <- ifelse(is.na(indicator_20_ch$LowerCI95.0limit), not_applic, ifelse(indicator_20_ch$LowerCI95.0limit > indicator_20_comp$UpperCI95.0limit, better, ifelse(indicator_20_ch$UpperCI95.0limit < indicator_20_comp$LowerCI95.0limit, worse, no_diff)))

grid.circle(x = 0.28, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_20_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_20_icon, x = unit(0.28, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_20_ch$Value),"-",paste(round(indicator_20_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.26, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_20_ch$Value), "Data\nunavailable",paste("of adults undertaking\nat least 150 minutes\nof physical activity\nper week in ",indicator_20_ch$Timeperiod, sep = "")), just = "left", x = unit(0.26, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 21 - Less than 30 equivalent minutes of physical activity
indicator_21_ch <- subset(indicator_21, AreaName == ch_area) 
# Lower is better
indicator_21_colour <- ifelse(is.na(indicator_21_ch$LowerCI95.0limit), not_applic, ifelse(indicator_21_ch$LowerCI95.0limit > indicator_21_comp$UpperCI95.0limit, worse, ifelse(indicator_21_ch$UpperCI95.0limit < indicator_21_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.37, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_21_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_21_icon, x = unit(0.37, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_21_ch$Value),"-",paste(round(indicator_21_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.35, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_21_ch$Value), "Data\nunavailable",paste("of adults\nundertaking less than\n30 equivalent minutes\nof physical activity\nper week in ",indicator_21_ch$Timeperiod, sep = "")), just = "left", x = unit(0.35, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# Mid working life to retirement
grid.circle(x = 0.465, y = 0.4  , r = 0.06, default.units = "npc", name = NULL, gp = gpar(fill = "#000000"), draw = TRUE, vp = NULL)
grid.text("Mid Working\nlife to\nretirement", just = "centre", x = unit(0.465, "npc"), y = unit(.4, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))

# indicator 22 - Adult smoking prevalence
indicator_22_ch <- subset(indicator_22, AreaName == ch_area) 
# Lower is better
indicator_22_colour <- ifelse(is.na(indicator_22_ch$LowerCI95.0limit), not_applic, ifelse(indicator_22_ch$LowerCI95.0limit > indicator_22_comp$UpperCI95.0limit, worse, ifelse(indicator_22_ch$UpperCI95.0limit < indicator_22_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.55, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_22_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_22_icon, x = unit(0.55, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_22_ch$Value),"-",paste(round(indicator_22_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.53, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_22_ch$Value), "Data\nunavailable",paste("adult smoking\nprevalence\n(",indicator_22_ch$Timeperiod,")", sep = "")), just = "left", x = unit(0.53, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 23 - Overweight or Obese adults
indicator_23_ch <- subset(indicator_23, AreaName == ch_area) 
# Lower is better
indicator_23_colour <- ifelse(is.na(indicator_23_ch$LowerCI95.0limit), not_applic, ifelse(indicator_23_ch$LowerCI95.0limit > indicator_23_comp$UpperCI95.0limit, worse, ifelse(indicator_23_ch$UpperCI95.0limit < indicator_23_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.64, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_23_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_23_icon, x = unit(0.64, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_23_ch$Value),"-",paste(round(indicator_23_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.62, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_23_ch$Value), "Data\nunavailable",paste("of adults (aged\n18+ years) classified\nas overweight or\nobese (",indicator_23_ch$Timeperiod,")", sep = "")), just = "left", x = unit(0.62, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 24 - Breast cancer screening
indicator_24_ch <- subset(indicator_24, AreaName == ch_area) 
# Higher is better
indicator_24_colour <- ifelse(is.na(indicator_24_ch$LowerCI95.0limit), not_applic, ifelse(indicator_24_ch$LowerCI95.0limit > indicator_24_comp$UpperCI95.0limit, better, ifelse(indicator_24_ch$UpperCI95.0limit < indicator_24_comp$LowerCI95.0limit, worse, no_diff)))

grid.circle(x = 0.73, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_24_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_24_icon, x = unit(0.73, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_24_ch$Value),"-",paste(round(indicator_24_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.71, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_24_ch$Value), "Data\nunavailable",paste("breast cancer\nscreening coverage\nwomen  aged\n53-70 years\n(",indicator_24_ch$Timeperiod,")", sep = "")), just = "left", x = unit(0.71, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 25 - Bowel cancer screening
indicator_25_ch <- subset(indicator_25, AreaName == ch_area) 
# Higher is better
indicator_25_colour <- ifelse(is.na(indicator_25_ch$LowerCI95.0limit), not_applic, ifelse(indicator_25_ch$LowerCI95.0limit > indicator_25_comp$UpperCI95.0limit, better, ifelse(indicator_25_ch$UpperCI95.0limit < indicator_25_comp$LowerCI95.0limit, worse, no_diff)))

grid.circle(x = 0.82, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_25_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_25_icon, x = unit(0.82, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_25_ch$Value),"-",paste(round(indicator_25_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.8, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_25_ch$Value), "Data\nunavailable",paste("bowel cancer\nscreening coverage\nadults aged\n60-74 years\n(",indicator_25_ch$Timeperiod,")", sep = "")), just = "left", x = unit(0.8, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 26 - Cervical cancer screening
indicator_26_ch <- subset(indicator_26, AreaName == ch_area) 
# Higher is better
indicator_26_colour <- ifelse(is.na(indicator_26_ch$LowerCI95.0limit), not_applic, ifelse(indicator_26_ch$LowerCI95.0limit > indicator_26_comp$UpperCI95.0limit, better, ifelse(indicator_26_ch$UpperCI95.0limit < indicator_26_comp$LowerCI95.0limit, worse, no_diff)))

grid.circle(x = 0.91, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_26_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_26_icon, x = unit(0.91, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_26_ch$Value),"-",paste(round(indicator_26_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.88, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_26_ch$Value), "Data\nunavailable",paste("cervical cancer\nscreening coverage\nwomen aged\n25-64 years\n(",indicator_26_ch$Timeperiod,")", sep = "")), just = "left", x = unit(0.88, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 27 Cardiovascular disease premature mortality
indicator_27_ch <- subset(indicator_27, AreaName == ch_area) 
indicator_27_colour <- ifelse(is.na(indicator_27_ch$LowerCI95.0limit), not_applic, ifelse(indicator_27_ch$LowerCI95.0limit > indicator_27_comp$UpperCI95.0limit, higher, ifelse(indicator_27_ch$UpperCI95.0limit < indicator_27_comp$LowerCI95.0limit, lower, no_diff)))

grid.circle(x = 0.89, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_27_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_27_icon, x = unit(0.89, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_27_ch$Value),"-",paste(round(indicator_27_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.87, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_27_ch$Value), "Data\nunavailable",paste("Mortality from all\nCardiovascular diseases\namong those aged\nunder 75 years in ",indicator_27_ch$Timeperiod, sep = "")), just = "left", x = unit(0.87, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 28 - Cancer premature mortality
indicator_28_ch <- subset(indicator_28, AreaName == ch_area) 
# Lower is better
indicator_28_colour <- ifelse(is.na(indicator_28_ch$LowerCI95.0limit), not_applic, ifelse(indicator_28_ch$LowerCI95.0limit > indicator_28_comp$UpperCI95.0limit, worse, ifelse(indicator_28_ch$UpperCI95.0limit < indicator_28_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.8, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_28_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_28_icon, x = unit(0.8, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_28_ch$Value),"-",paste(round(indicator_28_ch$Value, 0), " per 100,000", sep = "")), just = "left", x = unit(0.77, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_28_ch$Value), "Data\nunavailable",paste("Mortality from all\nCancers among those\naged under 75 years in\n",indicator_28_ch$Timeperiod, sep = "")), just = "left", x = unit(0.77, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 29 - 
# indicator_29_ch <- subset(indicator_29, AreaName == ch_area) 
# # Lower is better
# indicator_29_colour <- ifelse(is.na(indicator_29_ch$LowerCI95.0limit), not_applic, ifelse(indicator_29_ch$LowerCI95.0limit > indicator_29_comp$UpperCI95.0limit, worse, ifelse(indicator_29_ch$UpperCI95.0limit < indicator_29_comp$LowerCI95.0limit, better, no_diff)))
# 
# grid.circle(x = 0.71, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_29_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
# grid.raster(ind_29_icon, x = unit(0.71, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
# grid.text(ifelse(is.na(indicator_29_ch$Value),"-",paste(round(indicator_29_ch$Value, 0), " per 100,000", sep = "")), just = "left", x = unit(0.675, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
# grid.text(ifelse(is.na(indicator_29_ch$Value), "Data\nunavailable",paste("Mortality from all\nCancers among those\naged under 75 years in\n",indicator_29_ch$Timeperiod, sep = "")), just = "left", x = unit(0.675, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# Retirement to older age ##
grid.circle(x = 0.625, y = 0.2  , r = 0.06, default.units = "npc", name = NULL, gp = gpar(fill = "#000000"), draw = TRUE, vp = NULL)
grid.text("Retirement\nto older age", just = "centre", x = unit(0.625, "npc"), y = unit(.2, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))

# indicator 29 - ID 2015 - older people income deprivation
indicator_29_ch <- subset(indicator_29, AreaName == ch_area)

ID2015_ch <- subset(ID2015, `Local Authority District name (2013)` == ch_area)

indicator_29_ch$LCI <- PHEindicatormethods:::wilson_lower((indicator_29_ch$Value/100) * sum(ID2015_ch$`Older population aged 60 and over: mid 2012 (excluding prisoners)`),sum(ID2015_ch$`Older population aged 60 and over: mid 2012 (excluding prisoners)`), .95)  * 100
indicator_29_ch$UCI <- PHEindicatormethods:::wilson_upper((indicator_29_ch$Value/100) * sum(ID2015_ch$`Older population aged 60 and over: mid 2012 (excluding prisoners)`),sum(ID2015_ch$`Older population aged 60 and over: mid 2012 (excluding prisoners)`), .95) * 100

indicator_29_colour <- ifelse(is.na(indicator_29_ch$LCI), not_applic, ifelse(indicator_29_ch$LCI > indicator_29_comp$UCI, higher, ifelse(indicator_29_ch$UCI < indicator_29_comp$LCI, lower, no_diff)))

grid.circle(x = 0.54, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_29_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_29_icon, x = unit(0.54, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_29_ch$Value),"-",paste(round(indicator_29_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.515, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_29_ch$Value), "Data\nunavailable",paste("of older people\naged 60+ years living\non low incomes\n(English indices of\ndeprivation, ",indicator_29_ch$Timeperiod, ")\nEngland: ", round(indicator_29_comp$Value,1), "%", sep = "")), just = "left", x = unit(0.515, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 31 - Fuel poverty
indicator_30_ch <- subset(indicator_30, AreaName == ch_area)

grid.circle(x = 0.45, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = "#8E8E8E", col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_30_icon, x = unit(0.45, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_30_ch$Value),"-",paste(round(indicator_30_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.43, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_30_ch$Value), "Data\nunavailable",paste("of households\nexperiencing fuel\npoverty (Low income\nhigh cost method) in\n",indicator_30_ch$Timeperiod, sep = "")), just = "left", x = unit(0.43, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 31 - Emergency admissions for hip fractures
indicator_31_ch <- subset(indicator_31, AreaName == ch_area) 
# Lower is better
indicator_31_colour <- ifelse(is.na(indicator_31_ch$LowerCI95.0limit), not_applic, ifelse(indicator_31_ch$LowerCI95.0limit > indicator_31_comp$UpperCI95.0limit, worse, ifelse(indicator_31_ch$UpperCI95.0limit < indicator_31_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.36, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_31_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_31_icon, x = unit(0.36, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)

grid.text(ifelse(is.na(indicator_31_ch$Value),"-",paste(round(indicator_31_ch$Value, 0), " per 100,000", sep = "")), just = "left", x = unit(0.335, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_31_ch$Value), "Data\nunavailable",paste("Emergency admissions\nfor hip fractures among\nthose aged 65 years\nand over in ",indicator_31_ch$Timeperiod, "\n(", indicator_31_ch$Count ," admissions)", sep = "")), just = "left", x = unit(0.335, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 32 - Excess winter deaths
indicator_32_ch <- subset(indicator_32, AreaName == ch_area) 
# Lower is better
indicator_32_colour <- ifelse(is.na(indicator_32_ch$LowerCI95.0limit), not_applic, ifelse(indicator_32_ch$LowerCI95.0limit > indicator_32_comp$UpperCI95.0limit, worse, ifelse(indicator_32_ch$UpperCI95.0limit < indicator_32_comp$LowerCI95.0limit, better, no_diff)))

grid.circle(x = 0.27, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_32_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_32_icon, x = unit(0.27, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_32_ch$Value),"-",paste(round(indicator_32_ch$Value, 1), sep = "")), just = "left", x = unit(0.25, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_32_ch$Value), "Data\nunavailable",paste("Excess winter\ndeaths index\n(three years pooled)\nall ages from\n",indicator_32_ch$Timeperiod, sep = "")), just = "left", x = unit(0.25, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 33 - Male life expectancy at birth
indicator_33_ch <- subset(indicator_33, AreaName == ch_area) 
# Higher is better
indicator_33_colour <- ifelse(is.na(indicator_33_ch$LowerCI95.0limit), not_applic, ifelse(indicator_33_ch$LowerCI95.0limit > indicator_33_comp$UpperCI95.0limit, better, ifelse(indicator_33_ch$UpperCI95.0limit < indicator_33_comp$LowerCI95.0limit, worse, no_diff)))

grid.circle(x = 0.18, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_33_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_33_icon, x = unit(0.18, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_33_ch$Value),"-",paste(round(indicator_33_ch$Value, 1), " years", sep = "")), just = "left", x = unit(0.16, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_33_ch$Value),"Data on male\nlife expectancy\nunavailable",paste("Male life\nexpectancy\nat birth in\n",indicator_33_ch$Timeperiod, sep = "")), just = "left", x = unit(0.16, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

# indicator 34 - Female life expectancy at birth
indicator_34_ch <- subset(indicator_34, AreaName == ch_area) 
# Higher is better
indicator_34_colour <- ifelse(is.na(indicator_34_ch$LowerCI95.0limit), not_applic, ifelse(indicator_34_ch$LowerCI95.0limit > indicator_34_comp$UpperCI95.0limit, better, ifelse(indicator_34_ch$UpperCI95.0limit < indicator_34_comp$LowerCI95.0limit, worse, no_diff)))

grid.circle(x = 0.09, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_34_colour, col = "#ffffff"), draw = TRUE, vp = NULL)
grid.raster(ind_34_icon, x = unit(0.09, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
grid.text(ifelse(is.na(indicator_34_ch$Value),"-",paste(round(indicator_34_ch$Value, 1), " years", sep = "")), just = "left", x = unit(0.07, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
grid.text(ifelse(is.na(indicator_34_ch$Value),"Data on female\nlife expectancy\nunavailable",paste("Female life\nexpectancy\nat birth in\n",indicator_34_ch$Timeperiod, sep = "")), just = "left", x = unit(0.07, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))

grid.raster(unit_logo, y = unit(0.1, "npc"), x = unit(0.815, "npc"), vjust = 1, hjust = 0, width = .17)
grid.text("http://jsna.westsussex.gov.uk",just = "left", x = unit(0.05, "npc"), y = unit(.05, "npc"), gp = gpar(col = "#1c8ccd", fontsize = "11", fontface = "bold"))

grid.text("Infographic images designed by Freepik and OCHA from Flaticon",just = "centre", x = unit(0.5, "npc"), y = unit(.05, "npc"), gp = gpar(col = "#333333", fontsize = "8"))

dev.off()
}

