
# Outcomes on a page, Journey through the lifecourse ###

# This version is for lower tier local authorities, there is a separate file for upper tier authorities,

######################
# Author: Rich Tyler #
# Date: January 2020 #
######################

comp_area <- "England"

github_repo_dir <- "~/Documents/Repositories/la-ph-walkthrough"

library(easypackages)
libraries("png", "grid", "tidyverse", "gridExtra", "fingertipsR", "PHEindicatormethods", "readODS", "readxl", 'jsonlite')

options(scipen = 999)

# This capitalises words in a string
capwords = function(s, strict = FALSE) {
  cap = function(s) paste(toupper(substring(s, 1, 1)),{s = substring(s, 2); if(strict) tolower(s) else s},sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}

# Exporting a single data file for further use in an exported document or for interactive viewing.
# To achieve this we need to make consistent field names and descriptions that can easily be filtered later. It also means putting it information about comparators. The more work we do here the easier it will be to create the interactive data viz.

# This checks to see if 'Journey through indicators' folder exists in the working directory and if not it will be created.
if (!file.exists("./Journey through indicators")) {
  dir.create("./Journey through indicators")} 

# Indicator 1 - Infant Mortality ####
indicator_1 <- fingertips_data(IndicatorID = 92196, AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the rate of deaths in infants aged under 1 year per 1,000 live births.'),
         Unit = 'Rate',
         Label = paste0(round(Value,0), ' per 1,000 live births (95% CI ', round(Lower_CI,0), '-', round(Upper_CI,0), ', ', Numerator, ' deaths)'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>% 
  mutate(ID = as.character(ID))

# indicator_1_comp <- indicator_1 %>% 
#   filter(AreaName == comp_area)

# Indicator 2 - Low birth weight ####
indicator_2 <- fingertips_data(IndicatorID = 20101,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the number and proportion of babies born with a birth weight below 2.5kgs'),
         Unit = 'Proportion',
         Label = paste0(format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' (', round(Value,1), '%)'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_2_comp <- indicator_2 %>% 
#   filter(AreaName == comp_area)

# Indicator 3 - Breastfeeding initiation ####
indicator_3 <- fingertips_data(IndicatorID = 20201, AreaTypeID = 101) %>%  
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) %>%  # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of mothers who breastfeed their babies in the first 48 hours after delivery.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_3_comp <- indicator_3 %>% 
#   filter(AreaName == comp_area) 

# Indicator 4 - 0-4 in hh with out-of-work- benefits ####

# This is updated once a year in december

if(!file.exists("./Journey through indicators/children-in-out-of-work-households-by-la-may-2017.ods")){
  download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/761934/children-in-out-of-work-households-by-la-may-2017.ods", "./Journey through indicators/children-in-out-of-work-households-by-la-may-2017.ods", mode = "wb")}

if(!file.exists("./Journey through indicators/children-in-out-of-work-households-by-country-may-2017.ods")){
  download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/761937/children-in-out-of-work-households-by-country-may-2017.ods", "./Journey through indicators/children-in-out-of-work-households-by-country-may-2017.ods", mode = "wb")}

indicator_4 <- read_ods("./Journey through indicators/children-in-out-of-work-households-by-la-may-2017.ods", sheet = "Table_1", skip = 10, formula_as_formula = TRUE, col_names = FALSE) ; indicator_4 <- indicator_4[,2:4]
colnames(indicator_4) <- c("Area_name", "Area_code", "Children_aged_0-4_poverty")

indicator_4$`Children_aged_0-4_poverty` <- gsub(",", "",indicator_4$`Children_aged_0-4_poverty`)
indicator_4 <- subset(indicator_4, !(is.na(Area_code)))
indicator_4$`Children_aged_0-4_poverty` <- as.numeric(indicator_4$`Children_aged_0-4_poverty`)
indicator_4$Area_name <- gsub(" UA", "", indicator_4$Area_name)

indicator_4_comp <- read_ods("./Journey through indicators/children-in-out-of-work-households-by-country-may-2017.ods", sheet = "Table_1", skip = 8, formula_as_formula = TRUE, col_names = FALSE)
indicator_4_comp <- indicator_4_comp[,2:3]
colnames(indicator_4_comp) <- c("Area_name", "Children_aged_0-4_poverty")

indicator_4_comp <- indicator_4_comp %>% 
  filter(!(is.na(`Children_aged_0-4_poverty`))) %>% 
  filter(Area_name == "England") %>% 
  mutate(`Children_aged_0-4_poverty` = as.numeric(`Children_aged_0-4_poverty`)) %>% 
  mutate(Area_code = 'E92000001')

mye_2017_04 <- read_csv(url(paste0("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=&date=2017&gender=0&c_age=101...105&measures=20100&select=date_name,geography,geography_name,geography_code,gender_name,c_age,c_age_name,measures_name,obs_value,obs_status_name,record_count"))) %>% 
  filter(C_AGE_NAME %in% c('Age 0', 'Age 1', 'Age 2', 'Age 3', 'Age 4')) %>% 
  group_by(GEOGRAPHY_CODE, GEOGRAPHY_NAME) %>% 
  summarise(`0_4` = sum(OBS_VALUE, na.rm = TRUE)) %>% 
  rename(Area_code = GEOGRAPHY_CODE,
         Area_name = GEOGRAPHY_NAME) %>% 
  select(Area_code, `0_4`)

indicator_4 <- indicator_4 %>% 
  bind_rows(indicator_4_comp) %>% 
  left_join(mye_2017_04, by = "Area_code") %>% 
  mutate(Percentage_children_poverty = `Children_aged_0-4_poverty` / `0_4` * 100) %>% 
  mutate(Lower_CI = PHEindicatormethods:::wilson_lower(`Children_aged_0-4_poverty`, `0_4`, confidence = .95) * 100,
         Upper_CI = PHEindicatormethods:::wilson_upper(`Children_aged_0-4_poverty`, `0_4`, confidence = .95) * 100) %>% 
    rename(Value = Percentage_children_poverty) %>% 
  mutate(ID = '004',
         Name = 'Children in out of work households',
         Numerator = `Children_aged_0-4_poverty`,
         Timeperiod = 'May 2017',
         Denominator = `0_4`,
         Description = paste0('This is the proportion of 0-4 year olds in households with an adult claiming out-of-work benefits.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    
  mutate(ID = as.character(ID)) %>% 
  filter(substr(Area_code, 0,1) == 'E')

rm(indicator_4_comp)

# Indicator 5 - school Readiness by pupil residency ####

# 2018
download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/849261/EYFSP_pupil_characteristics_2019_-_underlying_data.zip",  "./Journey through indicators/EYFSP_2019_Tables.zip")
unzip("./Journey through indicators/EYFSP_2019_Tables.zip", exdir = "./Journey through indicators")

indicator_5 <-  read_excel("./Journey through indicators/EYFSP_LAD_pr_additional_tables_2018_2019.xlsx", col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  mutate(lad_name = ifelse(is.na(region_name), country_name, ifelse(is.na(lad_name), region_name, lad_name))) %>% 
  mutate(lad_code = ifelse(is.na(region_code_pupil), country_code_pupil, ifelse(is.na(lad_code_pupil), region_code_pupil, lad_code_pupil))) %>% 
  mutate(lad_name = gsub(" UA", "", lad_name)) %>% 
  rename(Area_name = lad_name,
         Area_code = lad_code,
         number_of_pupils = all_pupils,
         level = geographic_level,
         year = time_period) %>% 
  select(year, level, Area_code, Area_name, number_of_pupils, gld_number, gld_percent) %>% 
  mutate(gld_percent = gld_number / number_of_pupils * 100,
         gld_lci = PHEindicatormethods:::wilson_lower(gld_number, number_of_pupils, confidence = .95) * 100,
         gld_uci = PHEindicatormethods:::wilson_upper(gld_number, number_of_pupils, confidence = .95) * 100) %>% 
  filter(year == "201819")
  
indicator_5_comp <- indicator_5 %>% 
  filter(level == "Regional") %>% 
  summarise(number_of_pupils = sum(number_of_pupils, na.rm = TRUE),
            gld_number = sum(gld_number, na.rm = TRUE)) %>% 
  mutate(year = "201819",
         level = "England",
         Area_name = "England",
         Area_code = "E92000001") %>%
  mutate(gld_percent = gld_number / number_of_pupils * 100,
         gld_lci = PHEindicatormethods:::wilson_lower(gld_number, number_of_pupils, confidence = .95) * 100,
         gld_uci = PHEindicatormethods:::wilson_upper(gld_number, number_of_pupils, confidence = .95) * 100) %>% 
  select(year, level, Area_code, Area_name, number_of_pupils, gld_number, gld_percent, gld_lci, gld_uci)

indicator_5 <- indicator_5 %>% 
  bind_rows(indicator_5_comp) %>% 
  rename(Timeperiod = year,
         Numerator = gld_number,
         Denominator = number_of_pupils,
         Value = gld_percent,
         Lower_CI = gld_lci,
         Upper_CI = gld_uci) %>% 
  mutate(ID = '005',
         Name = 'Children achieving good level of development at the end of reception',
         Timeperiod = paste0(substr(Timeperiod, 1,4), '/', substr(Timeperiod, 5, 6)),
         Description = paste0('This is the proportion of children assessed as achieving a good level of development (e.g. being "school ready") at the end of reception.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

rm(indicator_5_comp)

# Indicators 6 - Excess weight reception ####
indicator_6 <- fingertips_data(IndicatorID = 20601, AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1] & Sex == "Persons") %>%  # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of reception aged pupils (aged 4/5 years) measured as having excess weight.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_6_comp <- indicator_6 %>% 
#   filter(AreaName == comp_area) 

# Indicator 7 ####
indicator_7 <- fingertips_data(IndicatorID = 20602, AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1] & Sex == "Persons") %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of Year 6 pupils (aged 10/11 years) measured as having excess weight.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

#indicator_7_comp <- indicator_7 %>% 
#  filter(AreaName == comp_area) 

# Indicator 8 Key stage 2 expected level for reading, writing and mathematics #### 
if (!file.exists("./Journey through indicators/Key_stage_2_underlying_data.zip")) {
  download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/829253/KS2_provisional_underlying_data.zip", "./Journey through indicators/Key_stage_2_underlying_data.zip", mode = "wb")
  unzip("./Journey through indicators/Key_stage_2_underlying_data.zip", exdir = "./Journey through indicators")}

indicator_8 <- read_csv("./Journey through indicators/KS2_provisional_underlying_data/2019_KS2_LAD_UD_TYPE_B.csv") %>% 
  filter(source_methodology == 'PupilResidency',
         breakdown == 'LADTotals') %>% 
  mutate(Area_name = ifelse(region_name == 'AllData', country_name, ifelse(LAD_name == 'AllData', region_name, LAD_name))) %>% 
  mutate(Area_code = ifelse(region_name == 'AllData', country_code, ifelse(LAD_name == 'AllData', region_code, LAD_code))) %>% 
  select(Area_code, Area_name, t_rwm_elig, t_rwm_exp, time_period) %>%
  mutate(t_rwm_elig = as.numeric(t_rwm_elig),
         t_rwm_exp = as.numeric(t_rwm_exp),
         Percentage_EL = t_rwm_exp / t_rwm_elig * 100,
         Lower_CI =  PHEindicatormethods:::wilson_lower(t_rwm_exp, t_rwm_elig, confidence = .95) * 100,
         Upper_CI =  PHEindicatormethods:::wilson_upper(t_rwm_exp, t_rwm_elig, confidence = .95) * 100) %>% 
  mutate(ID = '008',
         Name = 'Expected levels RWM KS2',
         Timeperiod = '2019') %>% 
  rename(Numerator = t_rwm_exp,
         Denominator = t_rwm_elig,
         Value = Percentage_EL) %>% 
  mutate(Description = paste0('This is the proportion of pupils who attain the expected levels at Key Stage 2 for Reading, Writing and Mathematics.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = 'This data is provisional.') %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%   
  mutate(ID = as.character(ID))

# indicator_8_comp <- indicator_8 %>% 
#   filter(Area_Name == comp_area)

# Indicator 9 - under 16s living in poverty ####
indicator_9 <- fingertips_data(IndicatorID = 10101,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1] & Sex == "Persons") %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of children aged 16 years estimated to be living in poverty.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

#indicator_9_comp <- indicator_9 %>% 
#  filter(AreaName == comp_area)

# Indicator 10 - Emergency admissions for intentional self-harm ####
indicator_10 <- fingertips_data(IndicatorID = 21001,  AreaTypeID = 101) %>% 
  filter(Sex == "Persons") %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1] & Sex == "Persons") %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the rate of emergency hospital admissions for intentional self-harm per 100,000 population (all ages).'),
         Unit = 'Rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' admissions)'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_10_comp <- indicator_10 %>% 
#   filter(AreaName == comp_area) 

# Indicator 11 - Under 18s conceptions ####
indicator_11 <- fingertips_data(IndicatorID = 20401,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the rate of pregnancies that occur among women aged 15-17 years, per 1,000 women aged 15-17.'),
         Unit = 'Rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 1,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' pregnancies)'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_11_comp <- indicator_11 %>% 
#   filter(AreaName == comp_area) 

# Indicator 12 - Pupils attaining GCSEs ####
indicator_12 <- fingertips_data(IndicatorID = 92199,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of pupils attaining at least five GCSEs.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_12_comp <- indicator_12 %>% 
#   filter(AreaName == comp_area) 

# Indicator 13 - Youth unemployment rate ####
mye_2018_2124 <- read_csv(url(paste0("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=&date=latest&gender=0&c_age=119...125&measures=20100&select=date_name,geography,geography_name,geography_code,gender_name,c_age,c_age_name,measures_name,obs_value,obs_status_name,record_count"))) %>% 
  filter(C_AGE_NAME %in% c('Age 18', 'Age 19', 'Age 20', 'Age 21', 'Age 22', 'Age 23', 'Age 24')) %>% 
  group_by(GEOGRAPHY_CODE, GEOGRAPHY_NAME) %>% 
  summarise(`18_24` = sum(OBS_VALUE, na.rm = TRUE)) %>% 
  # filter(substr(GEOGRAPHY_CODE, 0,1) == 'E') %>% 
  rename(Code = GEOGRAPHY_CODE,
         Name = GEOGRAPHY_NAME)

indicator_13 <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=2092957699,2092957698,2092957697,1879048193...1879048573,1879048583,1879048574...1879048582&date=latest&gender=0&age=11&measure=1&measures=20100&select=date_name,geography_name,gender_name,age_name,geography_code,measure_name,obs_value", col_types = cols(  DATE_NAME = col_character(),  GEOGRAPHY_NAME = col_character(),  GENDER_NAME = col_character(),  AGE_NAME = col_character(),  GEOGRAPHY_CODE = col_character(),  MEASURE_NAME = col_character(),  OBS_VALUE = col_double())) %>% 
  left_join(mye_2018_2124, by = c("GEOGRAPHY_CODE" = "Code")) %>% 
  mutate(Value = OBS_VALUE / `18_24` * 100,
         Lower_CI = PHEindicatormethods:::wilson_lower(OBS_VALUE, `18_24`, confidence = .95) * 100,
         Upper_CI = PHEindicatormethods:::wilson_upper(OBS_VALUE, `18_24`, confidence = .95) * 100) %>% 
  rename(Area_code = GEOGRAPHY_CODE,
         Area_name = GEOGRAPHY_NAME,
         Numerator = OBS_VALUE,
         Denominator = `18_24`,
         Timeperiod = DATE_NAME) %>% 
  mutate(ID = '013',
         Name = 'Youth unemployment claimaints',
         Description = paste0('This is the proportion of 18-24 year olds claiming out-of-work benefits.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  filter(substr(Area_code, 0,1) == 'E')

# indicator_13_comp <- indicator_13 %>% 
#   filter(GEOGRAPHY_NAME == comp_area) 

# Indicator 14 - Hourly earnings (male) ####
if (!file.exists("./Journey through indicators/PROV - Home Geography Table 8.6a   Hourly pay - Excluding overtime 2019.xls")){
  download.file("https://www.ons.gov.uk/file?uri=%2femploymentandlabourmarket%2fpeopleinwork%2fearningsandworkinghours%2fdatasets%2fplaceofresidencebylocalauthorityashetable8%2f2019provisional/table82019provisional.zip", "./Journey through indicators/Earnings.zip")
  unzip("./Journey through indicators/Earnings.zip", exdir = "./Journey through indicators")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.3b   Basic Pay - Including other pay 2019 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.1a   Weekly pay - Gross 2019.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.1b   Weekly pay - Gross 2019 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.2a   Weekly pay - Excluding overtime 2019.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.2b   Weekly pay - Excluding overtime 2019 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.3a   Basic Pay - Including other pay 2019.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.5b   Hourly pay - Gross 2019 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.4a   Overtime pay 2019.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.5a   Hourly pay - Gross 2019.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.12  Gender pay gap 2019.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.6b   Hourly pay - Excluding overtime 2019 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.7a   Annual pay - Gross 2019.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.7b   Annual pay - Gross 2019 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.8a   Annual pay - Incentive 2019.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.8b   Annual pay - Incentive 2019 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.9a   Paid hours worked - Total 2019.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.9b   Paid hours worked - Total 2019 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.10a   Paid hours worked - Basic 2019.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.10b   Paid hours worked - Basic 2019 CV.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.11a   Paid hours worked - Overtime 2019.xls")
  file.remove("./Journey through indicators/PROV - Home Geography Table 8.11b   Paid hours worked - Overtime 2019 CV.xls")
  file.remove("./Journey through indicators/Earnings.zip")}

# Estimates of the change from the previous year are provided for the median and mean. It is important to note that these are not adjusted to account for changes in the composition of the labour market during that period. Such factors can influence the apparent change in medians or means independently of changes in individuals' earnings. For example, when there are more low-paying jobs in the labour market in one year compared to the previous year, this acts to decrease the median. Consequently, care should be taken when drawing conclusions about changes in pay for individuals over time.				

indicator_14 <- read_excel("./Journey through indicators/PROV - Home Geography Table 8.6a   Hourly pay - Excluding overtime 2019.xls", sheet = "Male", col_types = c("text", "text", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "text"), skip = 4) %>%
  select(1:5) %>% 
  mutate(Timeperiod = "2018") %>% 
  filter(substr(Code, 0,1) == 'E')

colnames(indicator_14) <- c("Name", "Code", "N_jobs_thousands", "Median", "Change", "Timeperiod")

# indicator_14_SE <- indicator_14 %>% 
#   filter(Name == "South East")
# 
# indicator_14_England <- indicator_14 %>% 
#   filter(Name == "England")

indicator_14 <- indicator_14 %>% 
  rename(Area_code = Code,
         Area_name = Name,
         Value = Median) %>% 
  mutate(Numerator = NA,
         Denominator = NA,
         Lower_CI = NA,
         Upper_CI = NA,
         ID = '014',
         Name = 'Full time hourly gross earnings males',
         Description = paste0('This is the full time hourly gross earnings for males (median rate) excluding overtime.'),
         Unit = 'Median',
         Label = paste0('£', Value),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# rm(indicator_14_England, indicator_14_SE)

# Indicator 15 - Hourly earnings (female) ####

indicator_15 <- read_excel("./Journey through indicators/PROV - Home Geography Table 8.6a   Hourly pay - Excluding overtime 2019.xls", sheet = "Female", col_types = c("text", "text", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "text"), skip = 4) %>% 
  select(1:5) %>% 
  mutate(Timeperiod = "2018") %>% 
  filter(substr(Code, 0,1) == 'E')

colnames(indicator_15) <- c("Name", "Code", "N_jobs_thousands", "Median", "Change", "Timeperiod")

# indicator_15_SE <- indicator_15 %>% 
#   filter(Name == "South East")
# 
# indicator_15_England <- indicator_15 %>% 
#   filter(Name == "England")

indicator_15 <- indicator_15 %>% 
  # bind_rows(indicator_15_SE) %>% 
  # bind_rows(indicator_15_England) %>% 
  rename(Area_code = Code,
         Area_name = Name,
         Value = Median) %>% 
  mutate(Numerator = NA,
         Denominator = NA,
         Lower_CI = NA,
         Upper_CI = NA,
         ID = '015',
         Name = 'Full time hourly gross earnings females',
         Description = paste0('This is the full time hourly gross earnings for females (median rate) excluding overtime.'),
         Unit = 'Median',
         Label = paste0('£', Value),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# rm(indicator_15_England, indicator_15_SE)

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

# indicator_16_SE <- indicator_16_comp %>% 
#   filter(Name == "South East")
# 
# indicator_16_comp <- indicator_16_comp %>% 
#   filter(Name == comp_area)

indicator_16 <- indicator_16 %>% 
  bind_rows(indicator_16_comp) %>% 
  rename(Area_code = Code,
         Area_name = Name,
         Value = Affordability_ratio) %>% 
  mutate(Numerator = LQ_price,
         Denominator = Gross_LQ_Annual_Earnings,
         Lower_CI = NA,
         Upper_CI = NA,
         Timeperiod = '2018',
         ID = '016',
         Name = 'Housing Affordability ratio',
         Description = paste0('This is the ratio of lower quartile house price to lower quartile earnings.'),
         Unit = 'Ratio',
         Label = paste0(round(Value, 2)),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  filter(substr(Area_code,0,1) == 'E')

rm(indicator_16_comp, indicator_16_SE, indicator_161, indicator_16a_comp, indicator_16b_comp)

# Indicator 17 - KSI roads ####
indicator_17 <- fingertips_data(IndicatorID = 11001,  AreaTypeID = 101) %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) %>%  # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the rate of people killed or seriously injured on the roads in the area per 100,000 population (all ages).'),
         Unit = 'Rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' people)'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_17_comp <- indicator_17 %>% 
#   filter(AreaName == comp_area) 

# Indicator 18 - Violent crime ####
indicator_18 <- fingertips_data(IndicatorID = 11202,  AreaTypeID = 101) %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) %>%  # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the rate of crimes recorded as violence against the person per 1,000 population (all ages).'),
         Unit = 'Rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 1,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' offences)'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_18_comp <- indicator_18 %>% 
#   filter(AreaName == comp_area) 

# Indicator 19 Hospital admissions for alcohol-related conditions (Narrow), all ages ####
indicator_19 <- fingertips_data(IndicatorID = 91414,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons") %>% 
 arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the rate of admission episodes for alcohol related conditions (narrow definition) per 100,000 population (all ages).'),
         Unit = 'Rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' admissions)'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_19_comp <- indicator_19 %>% 
#   filter(AreaName == comp_area) 

# Indicator 20 - Physically active adults (current method) ####
indicator_20 <- fingertips_data(IndicatorID = 93014,  AreaTypeID = 101) %>% 
  filter(Sex == "Persons" & Age == "19+ yrs") %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>%  # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of adults aged 19+ undertaking at least 150 minutes of physical activity per week.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_20_comp <- indicator_20 %>% 
#   filter(AreaName == comp_area) 

# Indicator 21 - Physically inactive adults (current method) ####
indicator_21 <- fingertips_data(IndicatorID = 93015,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons" & Age == "19+ yrs") %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1])%>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of adults aged 19+ undertaking less than 30 equivalence minutes of physical activity per week.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_21_comp <- indicator_21 %>% 
#   filter(AreaName == comp_area) 

# Indicator 22 - Adult smoking prevalence ####
indicator_22 <- fingertips_data(IndicatorID = 92443,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons" & Age == "18+ yrs") %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) %>%  # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of adults aged 18+ estimated to be smokers.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_22_comp <- indicator_22 %>% 
#   filter(AreaName == comp_area) 

# Indicator 23 - Excess weight ####
indicator_23 <- fingertips_data(IndicatorID = 93088,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons" & Age == "18+ yrs") %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of adults aged 18+ estimated to be overweight or obese.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_23_comp <- indicator_23 %>% 
#   filter(AreaName == comp_area) 

# Indicator 24 - Breast screening ####
indicator_24 <- fingertips_data(IndicatorID = 22001,  AreaTypeID = 101) %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of females aged 53-70 years attending breast cancer screening in the previous 36 months.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_24_comp <- indicator_24 %>% 
#   filter(AreaName == comp_area) 

# Indicator 25 - Bowel screening ####
indicator_25 <- fingertips_data(IndicatorID = 91720,  AreaTypeID = 101) %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of eligible persons aged 60-74 years attending bowel cancer screening (Faecal occult blood test) in the previous 30 months.'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_25_comp <- indicator_25 %>% 
#   filter(AreaName == comp_area) 

# Indicator 26 - Cervical screening ####
indicator_26_a <- fingertips_data(IndicatorID = 93560,  AreaTypeID = 101) %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) %>%  # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of females aged 25-49 years attending cervical cancer screening in the previous 42 months (3.5 years).'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_26_a_comp <- indicator_26_a %>% 
  # filter(AreaName == comp_area) 

# Indicator 26 - Cervical screening ####
indicator_26_b <- fingertips_data(IndicatorID = 93561,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of females aged 50-64 years attending cervical cancer screening in the previous 66 months (5.5 years).'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_26_b_comp <- indicator_26_b %>% 
#   filter(AreaName == comp_area) 

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

# Indicator 27 - Mortality from all cancers ####
indicator_27 <- fingertips_data(IndicatorID = 40501,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons") %>% 
 arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the rate of mortality (deaths) from all cancers in persons aged under 75 years per 100,000 population.'),
         Unit = 'Rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' deaths)'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_27_comp <- indicator_27 %>% 
#   filter(AreaName == comp_area) 

# Indicator 28 - Mortality from cvd ####
indicator_28 <- fingertips_data(IndicatorID = 40401,  AreaTypeID = 101) %>% 
  filter(Sex == "Persons") %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>%  # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the rate of mortality (deaths) from all cardiovascular diseases in persons aged under 75 years per 100,000 population.'),
         Unit = 'Rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' deaths)'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_28_comp <- indicator_28 %>% 
#   filter(AreaName == comp_area) 

# Indicator 29 - Older people in poverty ####
indicator_29 <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv") %>% 
  select(`LSOA code (2011)`,`Local Authority District code (2019)`, `Local Authority District name (2019)`, `Income Deprivation Affecting Older People (IDAOPI) Score (rate)`, `Older population aged 60 and over: mid 2015 (excluding prisoners)`) %>% 
  rename(LSOA = 'LSOA code (2011)',
         Area_code = 'Local Authority District code (2019)', 
         Area_name = 'Local Authority District name (2019)', 
         IDAOPI_score = 'Income Deprivation Affecting Older People (IDAOPI) Score (rate)', 
         Denominator = 'Older population aged 60 and over: mid 2015 (excluding prisoners)') %>% 
  mutate(Older_people_IDOAPI = IDAOPI_score * Denominator) %>% 
  group_by(Area_code, Area_name) %>% 
  summarise(Older_people_IDOAPI = sum(Older_people_IDOAPI, na.rm = TRUE),
            Denominator = sum(Denominator, na.rm = TRUE)) %>% 
  mutate(recreated_IDAOPI = Older_people_IDOAPI/ Denominator * 100) %>% 
  ungroup()

indicator_29_Eng <- indicator_29 %>% 
  summarise(Older_people_IDOAPI = sum(Older_people_IDOAPI, na.rm = TRUE),
            Denominator = sum(Denominator, na.rm = TRUE)) %>% 
  mutate(recreated_IDAOPI = Older_people_IDOAPI/ Denominator * 100) %>% 
  mutate(Area_code = 'E92000001',
         Area_name = 'England')
  
indicator_29 <- indicator_29 %>% 
  bind_rows(indicator_29_Eng) %>% 
  mutate(ID = '029',
         Name = 'Estimated proportion older people living in poverty',
         Lower_CI = NA,
         Upper_CI = NA,
         Timeperiod = '2019',
         Description = paste0('This is the estimated proportion of older people (aged 60+) living on low incomes. It is calculated by applying the Lower-layer super output area (LSOA) level Income Deprivation Affective Older People Index (IDAOPI) to the population denominator for that LSOA and aggregating the result up to lower tier local authority level. The estimated number is rounded to the nearest 100.'),
         Unit = 'Proportion') %>% 
  rename(Value = recreated_IDAOPI,
         Numerator = Older_people_IDOAPI) %>% 
  mutate(Label = paste0(round(Value, 1), '% (', format(round(Numerator, -2), big.mark = ',', trim = TRUE), ' older people)'),
         Notes = 'This indicator is from the 2019 Index of multiple deprivation although the population used in calculations is based on 2015 estimates.') %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

rm(indicator_29_Eng)

# Indicator 30 - Percentage of households in fuel poverty ####
indicator_30 <- fingertips_data(IndicatorID = 90356,  AreaTypeID = 101) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of households experienceing fuel poverty (low income, high costs method).'),
         Unit = 'Proportion',
         Label = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_30_comp <- indicator_30 %>% 
#   filter(AreaName == comp_area) 

# Indicator 31 - Emergency admissions for hip fractures ####
indicator_31 <- fingertips_data(IndicatorID = 41401,  AreaTypeID = 101) %>% 
 filter(Sex == "Persons") %>% 
 arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
 filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the rate of emergency admissions for hip fractures among those aged 65 years and older per 100,000 population.'),
         Unit = 'Rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' admissions)'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_31_comp <- indicator_31 %>% 
#   filter(AreaName == comp_area) 

# Indicator 32 - Excess winter deaths index ####
indicator_32 <- fingertips_data(IndicatorID = 90360,  AreaTypeID = 101) %>% 
  filter(Sex == "Persons") %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>%  # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the ratio of extra deaths from all causes that occur in the winter months (December to March) compared with the expected number based on the average of the number of non-winter deaths. The result is presented as a percentage and for example an EWM index of 20 shows that there were 20 per cent more deaths in winter compared with the non-winter period.'),
         Unit = 'Index',
         Label = paste0(round(Value, 1), '%'),
         Notes = 'The number of winter deaths does not translate directly to being x more than in non-winter as the non-winter period is based on eight months of data compared with a four month winter period. Therefore we would expect half this number to take place in the eight months of winter (Dec-March) assuming identical mortality in winter and non-winter.') %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    mutate(ID = as.character(ID))

# indicator_32_comp <- indicator_32 %>% 
#    filter(AreaName == comp_area) 

# Indicator 33 - Male Life Expectancy ####
indicator_33 <- fingertips_data(IndicatorID = 90366,  AreaTypeID = 101) %>% 
  filter(Sex == "Male") %>% 
  arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the average number of years a male would expect to live if they experienced the age-specific mortality rates for the local area and time period throughout his life.'),
         Unit = 'Years',
         Label = paste0(round(Value, 1), ' years'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    
  mutate(ID = as.character(ID)) %>% 
  mutate(Name = 'Male life expectancy at birth')

# indicator_33_comp <- indicator_33 %>% 
#   filter(AreaName == comp_area) 

# Indicator 34 - Female Life Expectancy ####
indicator_34 <- fingertips_data(IndicatorID = 90366,  AreaTypeID = 101) %>% 
  filter(Sex == "Female") %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the average number of years a female would expect to live if they experienced the age-specific mortality rates for the local area and time period throughout her life.'),
         Unit = 'Years',
         Label = paste0(round(Value, 1), ' years'),
         Notes = NA) %>% 
select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Notes) %>%    
  mutate(ID = as.character(ID)) %>% 
  mutate(Name = 'Female life expectancy at birth')

# indicator_34_comp <- indicator_34 %>% 
  # filter(AreaName == comp_area) 

# Compile single dataframe ####

main_df <- indicator_1 %>% 
  bind_rows(indicator_2) %>% 
  bind_rows(indicator_3) %>% 
  bind_rows(indicator_4) %>% 
  bind_rows(indicator_5) %>% 
  bind_rows(indicator_6) %>% 
  bind_rows(indicator_7) %>% 
  bind_rows(indicator_8) %>% 
  bind_rows(indicator_9) %>% 
  bind_rows(indicator_10) %>% 
  bind_rows(indicator_11) %>% 
  bind_rows(indicator_12) %>% 
  bind_rows(indicator_13) %>% 
  bind_rows(indicator_14) %>% 
  bind_rows(indicator_15) %>% 
  bind_rows(indicator_16) %>% 
  bind_rows(indicator_17) %>% 
  bind_rows(indicator_18) %>% 
  bind_rows(indicator_19) %>% 
  bind_rows(indicator_20) %>% 
  bind_rows(indicator_21) %>% 
  bind_rows(indicator_22) %>% 
  bind_rows(indicator_23) %>% 
  bind_rows(indicator_24) %>% 
  bind_rows(indicator_25) %>% 
  bind_rows(indicator_26_a) %>% 
  bind_rows(indicator_26_b) %>% 
  bind_rows(indicator_27) %>% 
  bind_rows(indicator_28) %>% 
  bind_rows(indicator_29) %>% 
  bind_rows(indicator_30) %>% 
  bind_rows(indicator_31) %>% 
  bind_rows(indicator_32) %>% 
  bind_rows(indicator_33) %>% 
  bind_rows(indicator_34) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000058', 'Bournemouth, Christchurch and Poole', ifelse(Area_code == 'E06000023', 'Bristol', ifelse(Area_code == 'E12000006', 'East of England region', ifelse(Area_code == 'E12000004', 'East Midlands region', ifelse(Area_code == 'E06000019', 'Herefordshire', ifelse(Area_code == 'E07000146',	'King’s Lynn and West Norfolk', ifelse(Area_code == 'E07000112', 'Folkestone and Hythe', ifelse(Area_code == 'E06000010', 'Kingston upon Hull', ifelse(Area_code == 'E06000033',	'Southend-on-Sea', ifelse(Area_code == 'E07000204',	'St Edmundsbury', ifelse(Area_code == 'E12000001',	'North East region', ifelse(Area_code == 'E12000002','North West region', ifelse(Area_code == 'E12000003',	'Yorkshire and the Humber region', ifelse(Area_code == 'E12000005','West Midlands region', ifelse(Area_code == 'E12000007', 'London region', ifelse(Area_code == 'E12000008',	'South East region', ifelse(Area_code == 'E12000009',	'South West region',  ifelse(Area_code == 'E08000013',	'St Helens', Area_name))))))))))))))))))) %>% 
  mutate(Polarity = ifelse(ID %in% c('92196','20101','004','20601','20602','10101','21001','20401','013','11001','11202','91414','93015','92443','93088','40501','40401','41401','90360'), 'Lower is better', ifelse(ID %in% c('20201','005','008','92199','93014','22001','91720','93560','93561','90366','90366'), 'Higher is better', ifelse(ID %in% c('014','015','016','029','90356'), 'Not applicable', NA)))) %>% 
  mutate(Label = ifelse(is.na(Value), 'There is no data for this indicator', Label))

meta_areas <- main_df %>% 
  select(Area_code, Area_name) %>% 
  unique()

LA_name_code <- read_csv("https://opendata.arcgis.com/datasets/17eb563791b648f9a7025ca408bb09c6_0.csv") %>% 
  filter(substr(LAD18CD, 0,1) == 'E') %>% 
  rename(Code = LAD18CD) %>% 
  select(Code)

Region_name_code <- read_csv('https://opendata.arcgis.com/datasets/18b9e771acb84451a64d3bcdb3f3145c_0.csv') %>% 
  rename(Code = RGN19CD) %>% 
  select(Code)

Codes_to_keep <- LA_name_code %>% 
 bind_rows(Region_name_code) %>% 
  add_row(Code = 'E92000001')

main_df <- main_df %>% 
  filter(Area_code %in% Codes_to_keep$Code)

areas <- c("Adur", "Arun", "Chichester", "Crawley", "Horsham", "Mid Sussex","Worthing","Brighton and Hove", "Eastbourne","Hastings","Lewes","Rother","Wealden", 'South East region', 'England')

# i = 1

rm(indicator_1, indicator_2, indicator_3, indicator_4, indicator_5, indicator_6, indicator_7, indicator_8, indicator_9, indicator_10, indicator_11, indicator_12, indicator_13, indicator_14, indicator_15, indicator_16, indicator_17, indicator_18, indicator_19, indicator_20, indicator_21, indicator_22, indicator_23, indicator_24, indicator_25, indicator_26_a, indicator_26_b, indicator_27, indicator_28, indicator_29, indicator_30, indicator_31, indicator_32, indicator_33, indicator_34, mye_2017_04, mye_2018_2124, Region_name_code, LA_name_code)

# Read in JSNA logo (only download it if it is not available in your working directory)
if(!file.exists("./Journey through indicators/Research Unit.png")){download.file("https://github.com/psychty/la-ph-walkthrough/raw/master/Research%20Unit.png", "./Journey through indicators/Research Unit.png", mode = 'wb')} # This downloads a png image from the West Sussex JSNA website and saves it to your working directory. The if(!file.exists()) only runs the command if the file does not exist (because we have included an ! at the beginning)
unit_logo = readPNG("./Journey through indicators/Research Unit.png")

# I cannot host and make available the png images downloaded from flaticon (contact me and I can email something).

# Visualising the data ####

# We can define some hex colours to use later in our comparisons
better <- "#3ECC26"
no_diff <- "#E7AF27"
worse <- "#CC2629"
not_applic <- "#8E8E8E"
higher = "#BED2FF"
lower = "#5555E6"

# Icons ####
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
ind_27_icon = readPNG("./Journey through indicators/png/ind_29_icon_awareness_ribbon.png")
ind_28_icon = readPNG("./Journey through indicators/png/ind_28_icon_heartbeat.png")
ind_29_icon = readPNG("./Journey through indicators/png/ind_30_icon_coins_1.png")
ind_30_icon = readPNG("./Journey through indicators/png/ind_31_icon_flame.png")
ind_31_icon = readPNG("./Journey through indicators/png/ind_32_icon_hip.png")
ind_32_icon = readPNG("./Journey through indicators/png/ind_33_icon_glove.png")
ind_33_icon = readPNG("./Journey through indicators/png/ind_34_icon_tombstone.png")
ind_34_icon = readPNG("./Journey through indicators/png/ind_35_icon_tombstone.png")
arrow_left = readPNG("./Journey through indicators/png/arrow_left.png")
arrow_right = readPNG("./Journey through indicators/png/arrow_right.png")
arrow_down = readPNG("./Journey through indicators/png/arrow_down.png")

# Data visualisation - static pdf ####

# This will be the coordinate system for placing our objects in grid later
vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)

areas_wo_comp <- setdiff(areas, c('South East region', 'England')) # this is areas without areas; south east region and england

for (i in 1:length(areas_wo_comp)){
  ch_area <- areas_wo_comp[i]
  
  comp_data <- main_df %>% 
    filter(Area_name == comp_area) %>% 
    select(Name, Value, Lower_CI, Upper_CI, Numerator) %>% 
    rename(Comp_Value = Value,
           Comp_Lower_CI = Lower_CI,
           Comp_Upper_CI = Upper_CI,
           Comp_Numerator = Numerator)
  
  ch_data <- main_df %>% 
    filter(Area_name == ch_area) %>% 
    left_join(comp_data, by = 'Name') %>% 
    mutate(Significance = ifelse(is.na(Lower_CI), 'Not applicable', ifelse(Polarity == 'Not applicable', 'Not applicable', ifelse(Lower_CI > Comp_Upper_CI, 'Significantly higher', ifelse(Upper_CI < Comp_Lower_CI, 'Significantly lower', 'Similar'))))) %>% 
    mutate(Colour = ifelse(Significance == 'Not applicable', not_applic, ifelse(Significance == 'Similar', no_diff, ifelse(Significance == 'Significantly higher' & Polarity == 'Higher is better', better, ifelse(Significance == 'Significantly higher' & Polarity == 'Lower is better', worse, ifelse(Significance == 'Significantly lower' & Polarity == 'Lower is better', better, ifelse(Significance == 'Significantly lower' & Polarity == 'Higher is better', worse, NA)))))))
  
  # ch_code <- as.character(subset(LA_name_code, LAD17NM == ch_area, select = "LAD18CD")) # this will look up the name of the area (ch_area) on the file specified within read_csv (which is a lookup file from ONS) and find the code of the area
  
  ch_label <-  ifelse(nchar(ch_area) > 10,sub('(.{1,10})(\\s|$)', '\\1\n', ch_area),ch_area)
  data_show <-  sub('(.{1,15})(\\s|$)', '\\1\n', paste("Data are shown\nfor ", ch_area ," \nand are compared\nwith ", comp_area, sep = ""))
  
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
  
indicator_1_ch <- ch_data %>% 
  filter(ID == '92196')

  grid.circle(x = 0.15, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_1_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.text("Infant\nMortality", just = "centre", x = unit(0.15, "npc"), y = unit(.835, "npc"), gp = gpar(col = "#ffffff", fontsize = "7"))
  grid.text(ifelse(is.na(indicator_1_ch$Value),"-",paste(round(indicator_1_ch$Value,0), " per 1,000", sep = "")), just = "left", x = unit(0.13, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_1_ch$Value),"Data on\ninfant mortality\nunavailable",paste("Rate of deaths in\ninfants aged under\n1 year per 1,000 live\nbirths (", as.character(indicator_1_ch$Timeperiod), ")\n(", indicator_1_ch$Numerator, " deaths)", sep = "")), just = "left", x = unit(0.13, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  
# indicator 2 - Low birth weight babies
indicator_2_ch <- ch_data %>% 
  filter(ID == '20101')
  
  grid.circle(x = 0.24, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_2_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_2a_icon, x = unit(0.233, "npc"), y = unit(0.835, "npc"),  just = "centre", width = .025)
  grid.raster(ind_2b_icon, x = unit(0.247, "npc"), y = unit(0.845, "npc"),  just = "centre", width = .0125)
  grid.text(ifelse(is.na(indicator_2_ch$Value),"-",paste(indicator_2_ch$Numerator, " (",round(indicator_2_ch$Value,1),"%)", sep = "")), just = "left", x = unit(0.22, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_2_ch$Value),"Data on\nlow birth weight\nunavailable",paste("babies born\nin ", as.character(indicator_2_ch$Timeperiod), " had a\nLOW BIRTHWEIGHT\n(< 2,500g)", sep = "")), just = "left", x = unit(0.22, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # indicator 3 - Breast feeding initiation
  indicator_3_ch <- ch_data %>% 
    filter(ID == '20201')
  
  grid.circle(x = 0.33, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_3_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_3_icon, x = unit(0.33, "npc"), y = unit(0.835, "npc"),  just = "centre", width = .025)
  
  grid.text(ifelse(is.na(indicator_3_ch$Value),"-",paste(round(indicator_3_ch$Value,1), "%", sep = "")), just = "left", x = unit(0.31, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_3_ch$Value),"Data on\nbreast feeding\ninitiation\nunavailable",paste("of mothers\nbreastfeed their\nbabies in the first\n48hrs after delivery\nin ", as.character(indicator_3_ch$Timeperiod), sep = "")), just = "left", x = unit(0.31, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # indicator 4 - 0 to 4 year olds in households with an adult on out of work benefits #
  indicator_4_ch <- ch_data %>% 
    filter(ID == '004')
  
  grid.circle(x = 0.42, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_4_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.text("Out of\nwork\nBenefits", just = "centre", x = unit(0.42, "npc"), y = unit(.835, "npc"), gp = gpar(col = "#ffffff", fontsize = "6"))
  
  grid.text(ifelse(is.na(indicator_4_ch$Value),"-",paste(round(indicator_4_ch$Value,1), "%", sep = "")), just = "left", x = unit(0.4, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_4_ch$Value),"Data\nunavailable","of 0-4 year olds in\nhouseholds with an\nadult on out-of-work\nbenefits in May 2017."), just = "left", x = unit(0.4, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # indicator 5 - School readiness at the end of reception
  indicator_5_ch <- ch_data %>% 
    filter(ID == '005')
  
  grid.circle(x = 0.51, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_5_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_5_icon, x = unit(0.51, "npc"), y = unit(0.835, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_5_ch$Value),"-",paste(round(indicator_5_ch$Value,1), "%", sep = "")), just = "left", x = unit(0.49, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_5_ch$Value), "Data\nunavailable",paste0("of children assessed\nas achieving a good\nlevel of development\n(being 'School Ready')\nat the end of reception\nin ", indicator_5_ch$Timeperiod)), just = "left", x = unit(0.49, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # School years ##
  grid.circle(x = 0.625, y = 0.79  , r = 0.06, default.units = "npc", name = NULL, gp = gpar(fill = "#000000"), draw = TRUE, vp = NULL)
  grid.text("School\nYears", just = "centre", x = unit(0.625, "npc"), y = unit(.79, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))
  
  # indicator 6 - Excess weight reception
  indicator_6_ch <- ch_data %>% 
    filter(ID == '20601')
  
  grid.circle(x = 0.725, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_6_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_6_icon, x = unit(0.725, "npc"), y = unit(0.835, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_6_ch$Value),"-",paste(round(indicator_6_ch$Value,1), "%", sep = "")), just = "left", x = unit(0.705, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_6_ch$Value), "Data\nunavailable",paste("of reception aged\npupils (4/5 years)\nmeasured as having\nExcess Weight\nin ", indicator_6_ch$Timeperiod, sep = "")), just = "left", x = unit(0.705, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # indicator 7 - Excess weight year six
  indicator_7_ch <- ch_data %>% 
    filter(ID == '20602')
  
  grid.circle(x = 0.815, y = 0.835  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_7_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_7_icon, x = unit(0.815, "npc"), y = unit(0.835, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_7_ch$Value),"-",paste(round(indicator_7_ch$Value,1), "%", sep = "")), just = "left", x = unit(0.795, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_7_ch$Value), "Data\nunavailable",paste("of Year 6 pupils\n(aged 10/11 years)\nmeasured as having\nExcess Weight\nin ", indicator_7_ch$Timeperiod, sep = "")), just = "left", x = unit(0.795, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 8 - KS2 attainment reading, writing and maths
  indicator_8_ch <- ch_data %>% 
    filter(ID == '008')

  grid.circle(x = 0.905, y = 0.84  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_8_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_8_icon, x = unit(0.905, "npc"), y = unit(0.845, "npc"),  just = "centre", width = .03)
  grid.text("KS2", just = "centre", x = unit(0.905, "npc"), y = unit(.825, "npc"), gp = gpar(col = "#ffffff", fontsize = "7", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_8_ch$Value),"-",paste(round(indicator_8_ch$Value,1), "%", sep = "")), just = "left", x = unit(0.88, "npc"), y = unit(.78, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_8_ch$Value), "Data\nunavailable",paste("of pupils attain the\nexpected levels at\nKey stage 2 for\nReading, Writing and\nMathematics in 2019")), just = "left", x = unit(0.88, "npc"), y = unit(.76, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 9 - Children in poverty (under 16s)
indicator_9_ch <- ch_data %>% 
  filter(ID == '10101')

  grid.circle(x = 0.89, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_9_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_9_icon, x = unit(0.89, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_9_ch$Value),"-",paste(round(indicator_9_ch$Value,1), "%", sep = "")), just = "left", x = unit(0.87, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_9_ch$Value), "Data\nunavailable",paste("of children aged\nunder 16 years\nlived in poverty\nin ", indicator_9_ch$Timeperiod, sep = "")), just = "left", x = unit(0.87, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 10 - Emergency admissions for self harm
  indicator_10_ch <- ch_data %>% 
    filter(ID == '21001')
  
  grid.circle(x = 0.8, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_10_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_10_icon, x = unit(0.8, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_10_ch$Value),"-",paste(round(indicator_10_ch$Value,0), " per\n100,000", sep = "")), just = "left", x = unit(0.78, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_10_ch$Value), "Data\nunavailable",paste("Emergency Hospital\nAdmissions for\nIntentional Self-\nHarm in ", indicator_10_ch$Timeperiod)), just = "left", x = unit(0.78, "npc"), y = unit(.5475, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 11 - Teenage pregnancy
  indicator_11_ch <- ch_data %>% 
    filter(ID == '20401')
  
  grid.circle(x = 0.705, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_11_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_11_icon, x = unit(0.705, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_11_ch$Value),"-",paste(round(indicator_11_ch$Value,0), " per\n1,000", sep = "")), just = "left", x = unit(0.69, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_11_ch$Value), "Data\nunavailable",paste("In ", indicator_11_ch$Timeperiod, ", ", round(indicator_11_ch$Numerator,0)," young\nwomen under 18 years\nbecame pregnant.", sep = "")), just = "left", x = unit(0.69, "npc"), y = unit(.5475, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # indicator 12 - GCSE five A*-Cs
  indicator_12_ch <- ch_data %>% 
    filter(ID == '92199')
  
  grid.circle(x = 0.61, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_12_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_12_icon, x = unit(0.61, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_12_ch$Value),"-",paste(round(indicator_12_ch$Value ,1), "%", sep = "")), just = "left", x = unit(0.592, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_12_ch$Value), "Data\nunavailable",paste("of pupils attained\nat least five GCSE\ngrades at A*-C including\nEngland  and maths\nin ", indicator_12_ch$Timeperiod, sep = "")), just = "left", x = unit(0.592, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # Early working life ##
  grid.circle(x = 0.53, y = 0.6  , r = 0.06, default.units = "npc", name = NULL, gp = gpar(fill = "#000000"), draw = TRUE, vp = NULL)
  grid.text("Early\nWorking\nLife", just = "centre", x = unit(0.53, "npc"), y = unit(.6, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))
  
  # indicator 13 - Youth claimant rate
  indicator_13_ch <- ch_data %>% 
    filter(ID == '013')
  
  grid.circle(x = 0.445, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_13_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_13_icon, x = unit(0.445, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_13_ch$Value),"-",paste(round(indicator_13_ch$Value ,1), "%", sep = "")), just = "left", x = unit(0.42, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_13_ch$Value), "Data\nunavailable",paste("Youth claimant rate\n(", indicator_13_ch$Numerator," young people\naged 18-24 years)\non out-of-work benefits\nin ",indicator_13_ch$Timeperiod, sep = "")), just = "left", x = unit(0.42, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # indicator 14 - Median hourly earnings males
  indicator_14_ch <- ch_data %>% 
    filter(ID == '014')
    
  grid.circle(x = 0.355, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_14_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_14_icon, x = unit(0.355, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_14_ch$Value),"-",paste("£", str_pad(indicator_14_ch$Value, width = 5, side="right", pad="0"), sep = "")), just = "left", x = unit(0.331, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_14_ch$Value), "Data\nunavailable",paste("Full time hourly gross\nearnings for males\n(median rate) excluding\novertime in ", indicator_14_ch$Timeperiod, "\nEngland: £", str_pad(indicator_14_ch$Comp_Value, width = 5, side="right", pad="0"), sep = "")), just = "left", x = unit(0.331, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # indicator 15 - median hourly earnings females
  indicator_15_ch <- ch_data %>% 
    filter(ID == '015')
  
  grid.circle(x = 0.265, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_15_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_15_icon, x = unit(0.265, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_15_ch$Value),"-",paste("£", str_pad(indicator_15_ch$Value, width = 5, side="right", pad="0"), sep = "")), just = "left", x = unit(0.24, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_15_ch$Value), "Data\nunavailable",paste("Full time hourly gross\nearnings for females\n(median rate) excluding\novertime in ", indicator_15_ch$Timeperiod, "\nEngland: £", str_pad(indicator_15_ch$Comp_Value, width = 5, side = "right", pad = "0"), sep = "")), just = "left", x = unit(0.24, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # indicator 16 - Housing affordability index
  indicator_16_ch <- ch_data %>% 
    filter(ID == '016')
    
    grid.circle(x = 0.175, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_16_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_16_icon, x = unit(0.175, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_16_ch$Value),"-",paste(round(indicator_16_ch$Value,2), sep = "")), just = "left", x = unit(0.14, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_16_ch$Value), "Data\nunavailable",paste("Housing Affordability\nRatio of lower quartile\nhouse price to lower\nquartile earnings in \n",indicator_16_ch$Timeperiod," ",comp_area, ": ", round(indicator_16_ch$Comp_Value,2), sep = "")), just = "left", x = unit(0.14, "npc"), y = unit(.57, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # indicator 17 KSI
  indicator_17_ch <- ch_data %>% 
    filter(ID == '11001')
  
  grid.circle(x = 0.085, y = 0.645  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_17_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_17_icon, x = unit(0.085, "npc"), y = unit(0.645, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_17_ch$Value),"-",paste(round(indicator_17_ch$Value, 0), " per\n100,000", sep = "")), just = "left", x = unit(0.065, "npc"), y = unit(.59, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_17_ch$Value), "Data\nunavailable",paste("Rate of people\nkilled or seriously\ninjured on the roads\nin ",indicator_17_ch$Timeperiod, sep = "")), just = "left", x = unit(0.065, "npc"), y = unit(.5475, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 18 - Violent crime
indicator_18_ch <- ch_data %>% 
  filter(ID == '11202')

  grid.circle(x = 0.1, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_18_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.text("Violent\nCrime", just = "centre", x = unit(0.1, "npc"), y = unit(.45, "npc"), gp = gpar(col = "#ffffff", fontsize = "7"))
  grid.text(ifelse(is.na(indicator_18_ch$Value),"-",paste(round(indicator_18_ch$Value, 0), " per 1,000", sep = "")), just = "left", x = unit(0.081, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_18_ch$Value), "Data\nunavailable",paste("Violence against the\nperson (recorded\ncrime data)\n",format(indicator_18_ch$Numerator, big.mark = ",")," offences\nin ",indicator_18_ch$Timeperiod, sep = "")), just = "left", x = unit(0.081, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # indicator 19 - Admission episodes for alcohol related conditions
  indicator_19_ch <- ch_data %>% 
    filter(ID == '91414')
  
  grid.circle(x = 0.19, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_19_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_19_icon, x = unit(0.19, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_19_ch$Value),"-",paste(round(indicator_19_ch$Value, 0), " per\n100,000", sep = "")), just = "left", x = unit(0.16, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_19_ch$Value), "Data\nunavailable",paste("Admission episodes for\nalcohol related conditions\n(narrow definition) ",format(round(indicator_19_ch$Numerator,0), big.mark = ","),"\nadmissions in ",indicator_19_ch$Timeperiod, sep = "")), just = "left", x = unit(0.16, "npc"), y = unit(.3475, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 20 - Adults undertaking 150 minutes of physical activity
indicator_20_ch <- ch_data %>% 
  filter(ID == '93014')

  grid.circle(x = 0.28, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_20_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_20_icon, x = unit(0.28, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_20_ch$Value),"-",paste(round(indicator_20_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.26, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_20_ch$Value), "Data\nunavailable",paste("of adults undertaking\nat least 150 minutes\nof physical activity\nper week in ",indicator_20_ch$Timeperiod, sep = "")), just = "left", x = unit(0.26, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # indicator 21 - Less than 30 equivalent minutes of physical activity
  indicator_21_ch <- ch_data %>% 
    filter(ID == '93015')
  
  grid.circle(x = 0.37, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_21_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_21_icon, x = unit(0.37, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_21_ch$Value),"-",paste(round(indicator_21_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.35, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_21_ch$Value), "Data\nunavailable",paste("of adults\nundertaking less than\n30 equivalent minutes\nof physical activity\nper week in ",indicator_21_ch$Timeperiod, sep = "")), just = "left", x = unit(0.35, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# Mid working life to retirement
grid.circle(x = 0.465, y = 0.4  , r = 0.06, default.units = "npc", name = NULL, gp = gpar(fill = "#000000"), draw = TRUE, vp = NULL)
grid.text("Mid Working\nlife to\nretirement", just = "centre", x = unit(0.465, "npc"), y = unit(.4, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))
  
# indicator 22 - Adult smoking prevalence
indicator_22_ch <- ch_data %>% 
  filter(ID == '92443')
  
grid.circle(x = 0.55, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_22_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_22_icon, x = unit(0.55, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_22_ch$Value),"-",paste(round(indicator_22_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.53, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_22_ch$Value), "Data\nunavailable",paste("adult smoking\nprevalence\n(",indicator_22_ch$Timeperiod,")", sep = "")), just = "left", x = unit(0.53, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 23 - Overweight or Obese adults
indicator_23_ch <- ch_data %>% 
  filter(ID == '93088')

  grid.circle(x = 0.64, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_23_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_23_icon, x = unit(0.64, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_23_ch$Value),"-",paste(round(indicator_23_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.62, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_23_ch$Value), "Data\nunavailable",paste("of adults (aged\n18+ years) classified\nas overweight or\nobese (",indicator_23_ch$Timeperiod,")", sep = "")), just = "left", x = unit(0.62, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 24 - Breast cancer screening
indicator_24_ch <- ch_data %>% 
  filter(ID == '22001')

  grid.circle(x = 0.73, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_24_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_24_icon, x = unit(0.73, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_24_ch$Value),"-",paste(round(indicator_24_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.71, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_24_ch$Value), "Data\nunavailable",paste("breast cancer\nscreening coverage\nwomen  aged\n53-70 years\n(",indicator_24_ch$Timeperiod,")", sep = "")), just = "left", x = unit(0.71, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 25 - Bowel cancer screening
indicator_25_ch <- ch_data %>% 
  filter(ID == '91720')

grid.circle(x = 0.82, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_25_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_25_icon, x = unit(0.82, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_25_ch$Value),"-",paste(round(indicator_25_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.8, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_25_ch$Value), "Data\nunavailable",paste("bowel cancer\nscreening coverage\nadults aged\n60-74 years\n(",indicator_25_ch$Timeperiod,")", sep = "")), just = "left", x = unit(0.8, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 26_a - Cervical cancer screening 25-49
indicator_26_a_ch <- ch_data %>% 
  filter(ID == '93560')
  
grid.circle(x = 0.91, y = 0.45  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_26_a_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_26_icon, x = unit(0.91, "npc"), y = unit(0.45, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_26_a_ch$Value),"-",paste(round(indicator_26_a_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.88, "npc"), y = unit(.39, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_26_a_ch$Value), "Data\nunavailable",paste("cervical cancer\nscreening coverage\nwomen aged\n25-49 years\n(",indicator_26_a_ch$Timeperiod,")", sep = "")), just = "left", x = unit(0.88, "npc"), y = unit(.37, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 26_b - Cervical cancer screening 50-64
  indicator_26_b_ch <- ch_data %>% 
    filter(ID == '93561')

  grid.circle(x = 0.89, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_26_b_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_26_icon, x = unit(0.89, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_26_b_ch$Value),"-",paste(round(indicator_26_b_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.87, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_26_b_ch$Value), "Data\nunavailable",paste("cervical cancer\nscreening coverage\nwomen aged\n50-64 years (",indicator_26_b_ch$Timeperiod,")", sep = "")), just = "left", x = unit(0.87, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 27 - Cancer premature mortality
indicator_27_ch <- ch_data %>% 
  filter(ID == '40501')
  
  grid.circle(x = 0.79, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_27_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_27_icon, x = unit(0.79, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_27_ch$Value),"-",paste(round(indicator_27_ch$Value, 0), " per 100,000", sep = "")), just = "left", x = unit(0.77, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_27_ch$Value), "Data\nunavailable",paste("Mortality from all\nCancers among those\naged under 75 years in\n",indicator_27_ch$Timeperiod, sep = "")), just = "left", x = unit(0.77, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 28 Cardiovascular disease premature mortality
indicator_28_ch <- ch_data %>% 
  filter(ID == '40401')

  grid.circle(x = 0.7, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_28_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_28_icon, x = unit(0.7, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_28_ch$Value),"-",paste(round(indicator_28_ch$Value, 0), " per 100,000", sep = "")), just = "left", x = unit(0.68, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_28_ch$Value), "Data\nunavailable",paste("Mortality from all\nCardiovascular\ndiseases among\nthose aged under\n75 years in ",indicator_28_ch$Timeperiod, sep = "")), just = "left", x = unit(0.68, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  # Retirement to older age ##
  grid.circle(x = 0.625, y = 0.2  , r = 0.06, default.units = "npc", name = NULL, gp = gpar(fill = "#000000"), draw = TRUE, vp = NULL)
  grid.text("Retirement\nto older age", just = "centre", x = unit(0.625, "npc"), y = unit(.2, "npc"), gp = gpar(col = "#ffffff", fontsize = "8", fontface = "bold"))
  
  # indicator 29 - ID 2015 - older people income deprivation
  indicator_29_ch <- ch_data %>% 
    filter(ID == '029')
  
  grid.circle(x = 0.54, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_29_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_29_icon, x = unit(0.54, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_29_ch$Value),"-",paste(round(indicator_29_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.515, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_29_ch$Value), "Data\nunavailable",paste("of older people\naged 60+ years living\non low incomes\n(English indices of\ndeprivation, ",indicator_29_ch$Timeperiod, ")\nEngland: ", round(indicator_29_ch$Comp_Value,1), "%", sep = "")), just = "left", x = unit(0.515, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 31 - Fuel poverty
  indicator_30_ch <- ch_data %>% 
    filter(ID == '90356')
  
  grid.circle(x = 0.45, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_30_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_30_icon, x = unit(0.45, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_30_ch$Value),"-",paste(round(indicator_30_ch$Value, 1), "%", sep = "")), just = "left", x = unit(0.43, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_30_ch$Value), "Data\nunavailable",paste("of households\nexperiencing fuel\npoverty (Low income\nhigh cost method) in\n",indicator_30_ch$Timeperiod, sep = "")), just = "left", x = unit(0.43, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 31 - Emergency admissions for hip fractures
indicator_31_ch <- ch_data %>% 
  filter(ID == '41401')
  
  grid.circle(x = 0.36, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_31_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_31_icon, x = unit(0.36, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
  
  grid.text(ifelse(is.na(indicator_31_ch$Value),"-",paste(round(indicator_31_ch$Value, 0), " per 100,000", sep = "")), just = "left", x = unit(0.335, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_31_ch$Value), "Data\nunavailable",paste("Emergency admissions\nfor hip fractures among\nthose aged 65 years\nand over in ",indicator_31_ch$Timeperiod, "\n(", format(indicator_31_ch$Numerator, big.mark = ',', trim = TRUE)," admissions)", sep = "")), just = "left", x = unit(0.335, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 32 - Excess winter deaths
indicator_32_ch <- ch_data %>% 
  filter(ID == '90360')

  grid.circle(x = 0.27, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_32_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_32_icon, x = unit(0.27, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_32_ch$Value),"-",paste(round(indicator_32_ch$Value, 1), sep = "")), just = "left", x = unit(0.25, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_32_ch$Value), "Data\nunavailable",paste("Excess winter\ndeaths index\nall ages\n",indicator_32_ch$Timeperiod, sep = "")), just = "left", x = unit(0.25, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 33 - Male life expectancy at birth
indicator_33_ch <- ch_data %>% 
  filter(Name == 'Male life expectancy at birth')

  grid.circle(x = 0.18, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_33_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_33_icon, x = unit(0.18, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_33_ch$Value),"-",paste(round(indicator_33_ch$Value, 1), " years", sep = "")), just = "left", x = unit(0.16, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_33_ch$Value),"Data on male\nlife expectancy\nunavailable",paste("Male life\nexpectancy\nat birth in\n",indicator_33_ch$Timeperiod, sep = "")), just = "left", x = unit(0.16, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
# indicator 34 - Female life expectancy at birth
indicator_34_ch <- ch_data %>% 
  filter(Name == 'Female life expectancy at birth')
  
  grid.circle(x = 0.09, y = 0.25  , r = 0.03, default.units = "npc", name = NULL, gp = gpar(fill = indicator_34_ch$Colour, col = "#ffffff"), draw = TRUE, vp = NULL)
  grid.raster(ind_34_icon, x = unit(0.09, "npc"), y = unit(0.25, "npc"),  just = "centre", width = .025)
  grid.text(ifelse(is.na(indicator_34_ch$Value),"-",paste(round(indicator_34_ch$Value, 1), " years", sep = "")), just = "left", x = unit(0.07, "npc"), y = unit(.19, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "10", fontface = "bold"))
  grid.text(ifelse(is.na(indicator_34_ch$Value),"Data on female\nlife expectancy\nunavailable",paste("Female life\nexpectancy\nat birth in\n",indicator_34_ch$Timeperiod, sep = "")), just = "left", x = unit(0.07, "npc"), y = unit(.17, "npc"), vjust = 1, gp = gpar(col = "#333333", fontsize = "7"))
  
  grid.raster(unit_logo, y = unit(0.1, "npc"), x = unit(0.815, "npc"), vjust = 1, hjust = 0, width = .17)
  grid.text("http://jsna.westsussex.gov.uk",just = "left", x = unit(0.05, "npc"), y = unit(.05, "npc"), gp = gpar(col = "#1c8ccd", fontsize = "11", fontface = "bold"))
  
  grid.text("Infographic images designed by Freepik and OCHA from Flaticon",just = "centre", x = unit(0.5, "npc"), y = unit(.05, "npc"), gp = gpar(col = "#333333", fontsize = "8"))
  
  dev.off()
}

# Compiling data for export to json ####

meta <- main_df %>% 
  select(ID, Name, Description, Unit, Timeperiod, Polarity) %>% 
  unique() %>% 
  mutate(Number = row_number()) %>%
  mutate(x = ifelse(Number == 1, .16, ifelse(Number == 2, .24, ifelse(Number == 3, .32, ifelse(Number == 4, .43, ifelse(Number == 5, .51, ifelse(Number == 6, .73, ifelse(Number == 7, .81, ifelse(Number == 8, .89, ifelse(Number == 9, .88, ifelse(Number == 10, .8, ifelse(Number == 11, .72, ifelse(Number == 12, .6, ifelse(Number == 13, .44, ifelse(Number == 14, .36, ifelse(Number == 15, .25, ifelse(Number == 16, .17, ifelse(Number == 17, .09, ifelse(Number == 18, .1, ifelse(Number == 19, .18, ifelse(Number == 20, .26, ifelse(Number == 21, .38, ifelse(Number == 22, .54, ifelse(Number == 23, .62, ifelse(Number == 24, .74, ifelse(Number == 25, .82, ifelse(Number == 26, .9, ifelse(Number == 27, .87, ifelse(Number == 28, .79, ifelse(Number == 29, .71, ifelse(Number == 30, .52, ifelse(Number == 31, .44, ifelse(Number == 32, .36, ifelse(Number == 34, .26, ifelse(Number == 35, .18, NA))))))))))))))))))))))))))))))))))) %>% 
  mutate(y = ifelse(Number %in% c(1:8), .05, ifelse(Number %in% c(9:17), .3, ifelse(Number %in% c(18:26), .55, ifelse(Number %in% c(27:35), .8, NA))))) 

meta %>% 
  toJSON() %>% 
  write_lines(paste0(github_repo_dir, '/lt_data_meta_extract.json'))

comp_data <- main_df %>% 
  filter(Area_name == comp_area) %>% 
  select(Name, Value, Lower_CI, Upper_CI, Numerator) %>% 
  rename(Comp_Value = Value,
         Comp_Lower_CI = Lower_CI,
         Comp_Upper_CI = Upper_CI,
         Comp_Numerator = Numerator)

main_df %>% 
  filter(Area_name %in% areas_wo_comp) %>% 
  left_join(comp_data, by = 'Name') %>%
  left_join(meta[c('Name','x','y')], by = 'Name') %>% 
  mutate(Significance = ifelse(is.na(Lower_CI), 'Not applicable', ifelse(Polarity == 'Not applicable', 'Not applicable', ifelse(Lower_CI > Comp_Upper_CI, 'Significantly higher', ifelse(Upper_CI < Comp_Lower_CI, 'Significantly lower', 'Similar'))))) %>% 
  mutate(Colour = ifelse(Significance == 'Not applicable', not_applic, ifelse(Significance == 'Similar', no_diff, ifelse(Significance == 'Significantly higher' & Polarity == 'Higher is better', better, ifelse(Significance == 'Significantly higher' & Polarity == 'Lower is better', worse, ifelse(Significance == 'Significantly lower' & Polarity == 'Lower is better', better, ifelse(Significance == 'Significantly lower' & Polarity == 'Higher is better', worse, NA))))))) %>% 
  toJSON() %>% 
  write_lines(paste0(github_repo_dir, '/lt_data_extract_compare_england.json'))


