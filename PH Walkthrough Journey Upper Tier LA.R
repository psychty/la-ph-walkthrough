
# Outcomes on a page, Journey through the lifecourse ###

# This version is for upper tier local authorities, there is a separate file for upper tier authorities,

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

# Indicator 1 - Smoking at time of delivery ####
indicator_1 <- fingertips_data(IndicatorID = 93085, AreaTypeID = 202) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('The number of mothers known to be smokers at the time of delivery as a percentage of all maternities.'),
         Unit = 'proportion',
         Label = paste0(round(Value,1), '%'),
         Label_screen = paste0(round(Value,1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>% 
  mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of mothers'),
         line_2 = paste0('were smokers'),
         line_3 = paste0('at the time'),
         line_4 = paste0('of delivery in ', Timeperiod),
         line_5 = paste0('(', format(round(Numerator,0),big.mark = ',', trim = TRUE), ' mothers)')) %>% 
  mutate(img_path = './images/cigarette-with-smoke.svg')

# Indicator 2 - Low birth weight ####
indicator_2 <- fingertips_data(IndicatorID = 20101,  AreaTypeID = 202) %>% 
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
         Unit = 'proportion',
         Label = paste0(format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' (', round(Value,1), '%)'),
         Label_screen = paste0(format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' (', round(Value,1), '%)'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('babies born'),
         line_2 = paste0('in ', Timeperiod),
         line_3 = paste0('had a low'),
         line_4 = paste0('birthweight'),
         line_5 = paste0('(<2.5kgs)')) %>% 
  mutate(img_path = './images/low_birthweight_icon.svg')

# Indicator 3 - Newborn blood spot ####
# Until more data is published, we cannot use the latest year as it is only for regions (LA data is supressed). But we can find the latest year where county and ua data is available
ind_3_latest_ut_year <- fingertips_data(IndicatorID = 91323, AreaTypeID = 202) %>% 
  filter(AreaType == 'County & UA') %>% 
  filter(!is.na(Value)) %>% 
  arrange(desc(Timeperiod)) %>% 
  filter(Timeperiod == unique(Timeperiod)[1]) %>% 
  select(Timeperiod) %>% 
  unique()

indicator_3 <- fingertips_data(IndicatorID = 91323, AreaTypeID = 102) %>% 
  filter(Timeperiod == ind_3_latest_ut_year$Timeperiod) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('The proportion of babies registered within the relevant clinical commissioning group both at birth and on the last day of the reporting period who are eligible for newborn blood spot screening and have a conclusive result recorded on the child health information system (CHIS) at less than or equal to 17 days of age.'),
         Unit = 'proportion',
         Label = paste0(round(Value,1), '%'),
         Label_screen = paste0(round(Value,1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>% 
  mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of eligible babies'),
         line_2 = paste0('with a newborn'),
         line_3 = paste0('bloodspot screening'),
         line_4 = paste0('result in ', Timeperiod),
         line_5 = paste0('(', format(round(Numerator,0),big.mark = ',', trim = TRUE), ' babies)')) %>% 
  mutate(img_path = './images/laboratory-microscope.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 4 - Infant Mortality ####
indicator_4 <- fingertips_data(IndicatorID = 92196, AreaTypeID = 202) %>% 
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
         Unit = 'rate',
         Label = paste0(round(Value,0), ' per 1,000 live births (95% CI ', round(Lower_CI,0), '-', round(Upper_CI,0), ', ', format(Numerator, big.mark = ',', trim = TRUE), ' deaths)'),
         Label_screen = paste0(round(Value,0), ' per 1,000'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>% 
  mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('rate of deaths in'),
         line_2 = paste0('infants aged under'),
         line_3 = paste0('1 year per 1,000 live'),
         line_4 = paste0('births (', Timeperiod, ')'),
         line_5 = paste0('(', format(round(Numerator,0),big.mark = ',', trim = TRUE), ' deaths)')) %>% 
  mutate(img_path = NA)

# Indicator 5 - Breastfeeding initiation ####
indicator_5 <- fingertips_data(IndicatorID = 20201, AreaTypeID = 202) %>%  
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
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of mothers'),
         line_2 = paste0('breastfeed'),
         line_3 = paste0('their babies'),
         line_4 = paste0('within 48hrs'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/breastfeeding.svg')

# Indicator 6 - school Readiness by pupil residency ####

# 2018

#download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/849261/EYFSP_pupil_characteristics_2019_-_underlying_data.zip",  "./Journey through indicators/EYFSP_2019_Tables.zip")
#unzip("./Journey through indicators/EYFSP_2019_Tables.zip", exdir = "./Journey through indicators")

indicator_6 <-  read_excel("./Journey through indicators/EYFSP_LA_1_key_measures_additional_tables_2018_2019.xlsx", col_types = c("text", "numeric", "text", "text", "text", "text", "text", "numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  mutate(la_name = ifelse(is.na(region_name), country_name, ifelse(is.na(la_name), region_name, la_name))) %>% 
  mutate(new_la_code = ifelse(is.na(region_code), country_code, ifelse(is.na(new_la_code), region_code, new_la_code))) %>% 
  mutate(la_name = gsub(" UA", "", la_name)) %>% 
  rename(Area_name = la_name,
         Area_code = new_la_code,
         level = geographic_level,
         Timeperiod = time_period) %>% 
  filter(Timeperiod == "201819") %>% 
  filter(gender == 'Total') %>% 
  filter(characteristic_type == 'Total') %>% 
  select(Timeperiod, level, Area_code, Area_name, number_of_pupils, gld_number, gld_percent) %>% 
  unique() %>% 
  mutate(gld_percent = gld_number / number_of_pupils * 100,
         gld_lci = PHEindicatormethods:::wilson_lower(gld_number, number_of_pupils, confidence = .95) * 100,
         gld_uci = PHEindicatormethods:::wilson_upper(gld_number, number_of_pupils, confidence = .95) * 100) %>% 
  rename(Numerator = gld_number,
         Denominator = number_of_pupils,
         Value = gld_percent,
         Lower_CI = gld_lci,
         Upper_CI = gld_uci) %>% 
  mutate(ID = '006',
         Name = 'Children achieving a good level of development at the end of reception',
         Timeperiod = paste0(substr(Timeperiod, 1,4), '/', substr(Timeperiod, 5, 6)),
         Description = paste0('This is the proportion of children assessed as achieving a good level of development (e.g. being "school ready") at the end of reception.'),
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of children assessed'),
         line_2 = paste0('as achieving a good'),
         line_3 = paste0('level of development'),
         line_4 = paste0('at the end of reception'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/letter-block-toy.svg')

# Indicators 7 - Excess weight reception ####
indicator_7 <- fingertips_data(IndicatorID = 20601, AreaTypeID = 202) %>% 
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
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of reception'),
         line_2 = paste0('pupils (4-5'),
         line_3 = paste0('years) with'),
         line_4 = paste0('excess weight'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/fried-potatoes.svg')

# Indicator 8 ####
indicator_8 <- fingertips_data(IndicatorID = 20602, AreaTypeID = 202) %>% 
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
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of Year 6'),
         line_2 = paste0('pupils (10-11'),
         line_3 = paste0('years) with'),
         line_4 = paste0('excess weight'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/cup-cake.svg')

# Indicator 9 Key stage 2 expected level for reading, writing and mathematics #### 
if (!file.exists("./Journey through indicators/Key_stage_2_underlying_data.zip")) {
  download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/829253/KS2_provisional_underlying_data.zip", "./Journey through indicators/Key_stage_2_underlying_data.zip", mode = "wb")
  unzip("./Journey through indicators/Key_stage_2_underlying_data.zip", exdir = "./Journey through indicators")}

indicator_9 <- fingertips_data(IndicatorID = 92672, AreaTypeID = 102) %>% 
  arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1] & Sex == "Persons") %>%  # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('his is the proportion of pupils who attain the expected levels at Key Stage 2 for Reading, Writing and Mathematics.'),
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%   
  mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of pupils attain'),
         line_2 = paste0('expected levels at'),
         line_3 = paste0('Key Stage 2 for'),
         line_4 = paste0('reading, writing,'),
         line_5 = paste0('and maths in ', Timeperiod)) %>% 
  mutate(img_path = './images/sharpener.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 10 - under 16s living in poverty ####
indicator_10 <- fingertips_data(IndicatorID = 10101,  AreaTypeID = 202) %>% 
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
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of children aged'),
         line_2 = paste0('under 16 years'),
         line_3 = paste0('living in poverty'),
         line_4 = paste0('in ', Timeperiod),
         line_5 = NA) %>% 
  mutate(img_path = './images/saving-pig.svg')

# Indicator 11 - Unintentional and deliberate injury ####
indicator_11 <- fingertips_data(IndicatorID = 90285,  AreaTypeID = 102) %>% 
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
  mutate(Description = paste0('This is the crude rate of hospital admissions caused by unintentional and deliberate injuries in young people aged 15-24 years per 10,000 resident population aged 15-24 years.'),
         Unit = 'rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 10,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' admissions)'),
         Label_screen = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 10,000'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('crude rate of'),
         line_2 = paste0('hospital admissions'),
         line_3 = paste0('for unintentional'),
         line_4 = paste0('and deliberate injury'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/broken-arm.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 12 - Under 18s conceptions ####
indicator_12 <- fingertips_data(IndicatorID = 20401,  AreaTypeID = 202) %>% 
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
         Unit = 'rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 1,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' pregnancies)'),
         Label_screen = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 1,000'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('in ', Timeperiod, ', ', format(round(Numerator,0), big.mark = ',', trim = TRUE), ' young'),
         line_2 = paste0('women aged 15-18'),
         line_3 = paste0('years became '),
         line_4 = 'pregnant',
         line_5 = NA) %>% 
  mutate(img_path = './images/baby-stroller.svg')

# Indicator 13 - Current smokers aged 15 ####
indicator_13 <- fingertips_data(IndicatorID = 91548,  AreaTypeID = 102) %>% 
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
  mutate(Description = paste0("This is the proportion of 15 year olds who responded to Q17 in the What About YOUth (WAY) survey ('Now read the following statements carefully, and tick the box next to the one that best describes you') with the answers 'I sometimes smoke cigarettes now but I don't smoke as many as one a week", "I usually smoke between one and six cigarettes per week' or 'I usually smoke more than six cigarettes per week'."),
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of 15 year'),
         line_2 = paste0('olds who stated'),
         line_3 = paste0('they were'),
         line_4 = paste0('current smokers'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/cigarette-with-smoke.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 14 - Average Attainment 8 ####
indicator_14 <- fingertips_data(IndicatorID = 93378,  AreaTypeID = 102) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('Average Attainment 8 score for all pupils in state-funded schools, based on local authority of pupil residence.'),
         Unit = 'average score',
         Label = paste0(round(Value, 1)),
         Label_screen = paste0(round(Value, 1)),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('average'),
         line_2 = paste0('Attainment 8'),
         line_3 = paste0('score per'),
         line_4 = paste0('pupil'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/cap.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 15 - 16-18 year old NEET ####
indicator_15 <- fingertips_data(IndicatorID = 93203,  AreaTypeID = 102) %>% 
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
  mutate(Description = paste0('This is the proportion of children aged 16 and 17 years who were not in education, employment, or training.'),
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of children aged'),
         line_2 = paste0('under 16-17 years'),
         line_3 = paste0('not in education,'),
         line_4 = paste0('employment, or'),
         line_5 = paste0('training in ', Timeperiod)) %>% 
  mutate(img_path = './images/wrench.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 16 - Rate of first time entrants to the youth justice system ####
indicator_16 <- fingertips_data(IndicatorID = 10401,  AreaTypeID = 102) %>% 
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
  mutate(Description = paste0('This is the rate of first time entrants to the youth justice system per 100,000 population (all ages).'),
         Unit = 'rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' admissions)'),
         Label_screen = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('rate of first'),
         line_2 = paste0('time entrants'),
         line_3 = paste0('to the youth'),
         line_4 = paste0('justice system'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/handcuffs.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 17 - Outdoor space for exercise ####
indicator_17 <- fingertips_data(IndicatorID = 11601,  AreaTypeID = 102) %>% 
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
  mutate(Description = paste0('This is the weighted estimate of the proportion of residents in each area taking a visit to the natural environment for health or exercise purposes.'),
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of people (16+)'),
         line_2 = paste0('using outdoor'),
         line_3 = paste0('space for'),
         line_4 = paste0('exercise/health'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/tree.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 18 - KSI roads ####
indicator_18 <- fingertips_data(IndicatorID = 11001,  AreaTypeID = 102) %>% 
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
         Unit = 'rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' people)'),
         Label_screen = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('rate of people'),
         line_2 = paste0('killed or'),
         line_3 = paste0('seriously injured'),
         line_4 = paste0('on the roads'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/overturned-car.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 19 - Adult smoking prevalence ####
indicator_19 <- fingertips_data(IndicatorID = 92443,  AreaTypeID = 202) %>% 
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
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of adults aged'),
         line_2 = paste0('18+ estimated'),
         line_3 = paste0('to be smokers'),
         line_4 = paste0('in ', Timeperiod),
         line_5 = NA) %>% 
  mutate(img_path = './images/cigarette-with-smoke.svg')

# Indicator 20 - Emergency admissions for intentional self-harm ####
indicator_20 <- fingertips_data(IndicatorID = 21001,  AreaTypeID = 102) %>% 
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
         Unit = 'rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' admissions)'),
         Label_screen = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('rate of emergency'),
         line_2 = paste0('hospital'),
         line_3 = paste0('admissions for'),
         line_4 = paste0('intentional self-'),
         line_5 = paste0('harm in ', Timeperiod)) %>% 
  mutate(img_path = './images/emergency-ambulance.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 21 - Successful completion of drug treatment (non-opiate users) ####
indicator_21 <- fingertips_data(IndicatorID = 90245,  AreaTypeID = 102) %>% 
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
  mutate(Description = paste0('This is the number of users of non-opiates that left drug treatment successfully (free of drug(s) of dependence) who do not then re-present to treatment again within 6 months as a percentage of the total number of non-opiate users in treatment.'),
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of adults aged'),
         line_2 = paste0('18+ successfully'),
         line_3 = paste0('completing non-'),
         line_4 = paste0('opiate drug'),
         line_5 = paste0('treatment in ', Timeperiod)) %>% 
  mutate(img_path = './images/syringe.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 22 Hospital admissions for alcohol-related conditions (Narrow), all ages ####
indicator_22 <- fingertips_data(IndicatorID = 91414,  AreaTypeID = 102) %>% 
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
         Unit = 'rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' admissions)'),
         Label_screen = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('rate of admission'),
         line_2 = paste0('episodes for alcohol'),
         line_3 = paste0('related conditions'),
         line_4 = paste0('(narrow definition)'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/glass-and-bottle-of-wine.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 23 - Physically active adults (current method) ####
indicator_23 <- fingertips_data(IndicatorID = 93014,  AreaTypeID = 202) %>% 
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
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of adults doing'),
         line_2 = paste0('at least 150'),
         line_3 = paste0('minutes of physical'),
         line_4 = paste0('activity per week '),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/sprinting.svg')

# Indicator 24 - Physically inactive adults (current method) ####
indicator_24 <- fingertips_data(IndicatorID = 93015,  AreaTypeID = 202) %>% 
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
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of adults doing'),
         line_2 = paste0('less than 30'),
         line_3 = paste0('minutes of physical'),
         line_4 = paste0('activity per week'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/sofa-with-armrest.svg')

# Indicator 25 - Self-reported low happiness ####
indicator_25 <- fingertips_data(IndicatorID = 22303,  AreaTypeID = 102) %>% 
  filter(Sex == "Persons" & Age == "16+ yrs") %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1])%>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the proportion of adults aged 16+ who answered 0-4 to the question "Overall, how happy did you feel yesterday? Responses are given on a scale of 0-10 (where 0 is “not at all satisfied/happy/anxious/worthwhile” and 10 is “completely satisfied/happy/anxious/worthwhile”).'),
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of adults'),
         line_2 = paste0('aged 16+'),
         line_3 = paste0('who reported'),
         line_4 = paste0('low happiness'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/indifferent.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 26 - Excess weight ####
indicator_26 <- fingertips_data(IndicatorID = 93088,  AreaTypeID = 202) %>% 
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
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of adults'),
         line_2 = paste0('aged 18+'),
         line_3 = paste0('classified as'),
         line_4 = paste0('overweight or'),
         line_5 = paste0('obese in ', Timeperiod)) %>% 
  mutate(img_path = './images/fast-food.svg')

# Indicator 27 - Breast screening ####
indicator_27 <- fingertips_data(IndicatorID = 22001,  AreaTypeID = 202) %>% 
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
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of eligible women'),
         line_2 = paste0('attending breast'),
         line_3 = paste0('cancer screening'),
         line_4 = paste0('in past 36 months'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/brassiere.svg')

# Indicator 28 - Bowel screening ####
indicator_28 <- fingertips_data(IndicatorID = 91720,  AreaTypeID = 202) %>% 
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
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of eligible people'),
         line_2 = paste0('attending bowel'),
         line_3 = paste0('cancer screening'),
         line_4 = paste0('in past 30 months'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/underpants.svg')

# Indicator 29 - Cumulative percentage of 40-74 year olds receiving Health Checks ####
indicator_29 <- fingertips_data(IndicatorID = 91100,  AreaTypeID = 102) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the rolling 5 year cumulative percentage of the eligible population aged 40-74 offered an NHS Health Check who received an NHS Health Checks.'),
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '% (',format(round(Numerator, 0),big.mark = ',', trim = TRUE), ' patients)'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of eligible'),
         line_2 = paste0('people offered'),
         line_3 = paste0('NHS Health Check'),
         line_4 = paste0('who received one'),
         line_5 = paste0(Timeperiod)) %>% 
  mutate(img_path = './images/report.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 30 - Late diagnosis of HIV ####
indicator_30 <- fingertips_data(IndicatorID = 90791,  AreaTypeID = 202) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the percentage of adults (aged 15 years or more) diagnosed with a CD4 cell count less than 350 cells per mm3 among all newly diagnosed adults with CD4 cell count available within 91 days of diagnosis.'),
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '% (',format(round(Numerator, 0),big.mark = ',', trim = TRUE), ' patients)'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of HIV positive'),
         line_2 = paste0('diagnoses among'),
         line_3 = paste0('those aged 15+'),
         line_4 = paste0('diagnosed as late'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/awareness-ribbon.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 31 - Mortality from cvd ####
indicator_31 <- fingertips_data(IndicatorID = 40401,  AreaTypeID = 102) %>% 
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
         Unit = 'rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' deaths)'),
         Label_screen = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('rate of deaths from'),
         line_2 = paste0('all cardiovascular'),
         line_3 = paste0('diseases in persons aged'),
         line_4 = paste0('under 75 years'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/cardio.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 32 - Mortality from all cancers ####
indicator_32 <- fingertips_data(IndicatorID = 40501,  AreaTypeID = 102) %>% 
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
         Unit = 'rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' deaths)'),
         Label_screen = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID))%>% 
  mutate(line_1 = paste0('rate of deaths'),
         line_2 = paste0('from all cancers'),
         line_3 = paste0('in persons aged'),
         line_4 = paste0('under 75 years'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/awareness-ribbon.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 33 - Social isolation ####
indicator_33 <- fingertips_data(IndicatorID = 90280,  AreaTypeID = 102) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1] & Age == '65+ yrs') %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the percentage of respondents aged 65+ to the Adult Social Care Users Survey who reported that they have as much social contact as they want with people they like.'),
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '%'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of adult social'),
         line_2 = paste0('care users (65+)'),
         line_3 = paste0('who have as much'),
         line_4 = paste0('social contact as'),
         line_5 = paste0('they want in ', Timeperiod)) %>% 
  mutate(img_path = './images/users-group.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 34 - Flu vaccine ####
indicator_34 <- fingertips_data(IndicatorID = 30314,  AreaTypeID = 102) %>% 
  arrange(desc(Timeperiod)) %>%  # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is the flu vaccine uptake (%) in adults aged 65 and over, who received the flu vaccination between 1st September to the end of February in a primary care setting (GPs).'),
         Unit = 'proportion',
         Label = paste0(round(Value, 1), '% (', format(round(Numerator,0), big.mark = ',', trim = TRUE), ' patients)'),
         Label_screen = paste0(round(Value, 1), '%'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('of eligible'),
         line_2 = paste0('adults aged 65+'),
         line_3 = paste0('who received'),
         line_4 = paste0('the flu vaccine'),
         line_5 = paste0('in ', Timeperiod)) %>% 
  mutate(img_path = './images/virus.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 35 - Emergency admissions for hip fractures ####
indicator_35 <- fingertips_data(IndicatorID = 41401,  AreaTypeID = 102) %>% 
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
         Unit = 'rate',
         Label = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000 (95% CI ', format(round(Lower_CI, 0), big.mark = ',', trim = TRUE), '-', format(round(Upper_CI, 0), big.mark = ',', trim = TRUE), ', ', format(round(Numerator, 0), big.mark = ',', trim = TRUE), ' admissions)'),
         Label_screen = paste0(format(round(Value, 0), big.mark = ',', trim = TRUE), ' per 100,000'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    mutate(ID = as.character(ID)) %>% 
  mutate(line_1 = paste0('rate of emergency'),
         line_2 = paste0('admissions for'),
         line_3 = paste0('hip fractures'),
         line_4 = paste0('among those'),
         line_5 = paste0('aged 65+ in ', Timeperiod)) %>% 
  mutate(img_path = './images/hip-bone.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 36 - Male slope inequality in life expectancy at birth ####
indicator_36 <- fingertips_data(IndicatorID = 92901,  AreaTypeID = 102) %>% 
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
  mutate(Description = paste0('This is a measure of the social gradient in life expectancy representing the range in years of life expectancy from most to least deprived neighbourhoods in the area.'),
         Unit = 'life expectancy in years',
         Label = paste0(round(Value, 1), ' years'),
         Label_screen = paste0(round(Value, 1), ' years'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    
  mutate(ID = as.character(ID)) %>% 
  mutate(Name = 'Male slope index of inquality in life expectancy at birth') %>% 
  mutate(line_1 = paste0('male slope of'),
         line_2 = paste0('inequality in'),
         line_3 = paste0('life expectancy'),
         line_4 = paste0('at birth in'),
         line_5 = paste0(Timeperiod)) %>% 
  mutate(img_path = './images/decreasing.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 37 - Female slope inequality in life expectancy at birth ####
indicator_37 <- fingertips_data(IndicatorID = 92901,  AreaTypeID = 102) %>% 
  filter(Sex == "Female") %>% 
  arrange(desc(Timeperiod)) %>% # Order by descending year (latest data on top)
  filter(Timeperiod == unique(Timeperiod)[1]) %>% # Now that we have ordered the data, we select the first unique value Timeperiod as this will be the most recent value
  rename(ID = IndicatorID,
         Name = IndicatorName,
         Area_code = AreaCode,
         Area_name = AreaName,
         Lower_CI = LowerCI95.0limit,
         Upper_CI = UpperCI95.0limit,
         Numerator = Count) %>% 
  mutate(Description = paste0('This is a measure of the social gradient in life expectancy representing the range in years of life expectancy from most to least deprived neighbourhoods in the area.'),
         Unit = 'life expectancy in years',
         Label = paste0(round(Value, 1), ' years'),
         Label_screen = paste0(round(Value, 1), ' years'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    
  mutate(ID = as.character(ID)) %>% 
  mutate(Name = 'Female slope index of inquality in life expectancy at birth') %>% 
  mutate(line_1 = paste0('male slope of'),
         line_2 = paste0('inequality in'),
         line_3 = paste0('life expectancy'),
         line_4 = paste0('at birth in'),
         line_5 = paste0(Timeperiod)) %>% 
  mutate(img_path = './images/decreasing.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 38 - Male Life Expectancy ####
indicator_38 <- fingertips_data(IndicatorID = 90366,  AreaTypeID = 102) %>% 
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
         Unit = 'life expectancy in years',
         Label = paste0(round(Value, 1), ' years'),
         Label_screen = paste0(round(Value, 1), ' years'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    
  mutate(ID = as.character(ID)) %>% 
  mutate(Name = 'Male life expectancy at birth') %>% 
  mutate(line_1 = paste0('male life'),
         line_2 = paste0('expectancy'),
         line_3 = paste0('at birth'),
         line_4 = paste0('in ', Timeperiod),
         line_5 = NA) %>% 
  mutate(img_path = './images/headstone.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Indicator 39 - Female Life Expectancy ####
indicator_39 <- fingertips_data(IndicatorID = 90366,  AreaTypeID = 102) %>% 
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
         Unit = 'life expectancy in years',
         Label = paste0(round(Value, 1), ' years'),
         Label_screen = paste0(round(Value, 1), ' years'),
         Notes = NA) %>% 
  select(ID, Name, Description, Unit, Timeperiod, Area_name, Area_code, Value, Lower_CI, Upper_CI, Numerator, Denominator,Label, Label_screen, Notes) %>%    
  mutate(ID = as.character(ID)) %>% 
  mutate(Name = 'Female life expectancy at birth') %>% 
  mutate(line_1 = paste0('female life'),
         line_2 = paste0('expectancy'),
         line_3 = paste0('at birth'),
         line_4 = paste0('in ', Timeperiod),
         line_5 = NA) %>% 
  mutate(img_path = './images/headstone.svg') %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', Area_code)) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000059', 'Dorset', Area_name))

# Compile ####

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
  bind_rows(indicator_26) %>% 
  bind_rows(indicator_27) %>% 
  bind_rows(indicator_28) %>% 
  bind_rows(indicator_29) %>% 
  bind_rows(indicator_30) %>% 
  bind_rows(indicator_31) %>% 
  bind_rows(indicator_32) %>% 
  bind_rows(indicator_33) %>% 
  bind_rows(indicator_34) %>% 
  bind_rows(indicator_35) %>% 
  bind_rows(indicator_36) %>% 
  bind_rows(indicator_37) %>% 
  bind_rows(indicator_38) %>% 
  bind_rows(indicator_39) %>% 
  mutate(Area_name = ifelse(Area_code == 'E06000058', 'Bournemouth, Christchurch, and Poole', ifelse(Area_code == 'E06000023', 'Bristol', ifelse(Area_code == 'E06000047', 'County Durham', ifelse(Area_code == 'E07000204',	'St Edmundsbury', ifelse(Area_code == 'E12000001',	'North East region', ifelse(Area_code == 'E12000002','North West region', ifelse(Area_code == 'E12000003',	'Yorkshire and the Humber region', ifelse(Area_code == 'E12000005','West Midlands region', ifelse(Area_code == 'E12000007', 'London region', ifelse(Area_code == 'E12000008',	'South East region', ifelse(Area_code == 'E12000009',	'South West region',  ifelse(Area_code == 'E12000004',	'East Midlands Region', ifelse(Area_code == 'E12000006', 'East of England region', ifelse(Area_code == 'E06000010', '	Kingston upon Hull', Area_name))))))))))))))) %>% 
  mutate(Polarity = ifelse(ID %in% c('93085','20101','92196','20601','20602','10101','90285','20401','91548','93203','10401','11001','21001','92443','91414','93015','22303','93088','90791','40401','40501','41401','92901'), 'Lower is better', ifelse(ID %in% c('91323','20201','006','92672','93378','11601','90245','93014','22001','91720','91100','90280','30314','90366'), 'Higher is better',  NA))) %>%
  mutate(Label = ifelse(is.na(Value), 'There is no data for this indicator', Label)) %>%
  mutate(Label_screen = ifelse(is.na(Value), NA, Label_screen)) %>%
  mutate(line_1 = ifelse(is.na(Value), 'There is no', line_1)) %>%
  mutate(line_2 = ifelse(is.na(Value), 'data for this', line_2)) %>%
  mutate(line_3 = ifelse(is.na(Value), 'indicator', line_3)) %>%
  mutate(line_4 = ifelse(is.na(Value), NA, line_4)) %>%
  mutate(line_5 = ifelse(is.na(Value), NA, line_5))

meta_areas <- main_df %>% 
  select(Area_code, Area_name) %>% 
  unique()

meta_codes <-  main_df %>% 
  select(ID, Name) %>% 
  unique()

# Areas
LAD <- read_csv(url("https://opendata.arcgis.com/datasets/a267b55f601a4319a9955b0197e3cb81_0.csv"), col_types = cols(LAD17CD = col_character(),LAD17NM = col_character(),  LAD17NMW = col_character(),  FID = col_integer()))

Counties <- read_csv(url("https://opendata.arcgis.com/datasets/7e6bfb3858454ba79f5ab3c7b9162ee7_0.csv"), col_types = cols(CTY17CD = col_character(),  CTY17NM = col_character(),  Column2 = col_character(),  Column3 = col_character(),  FID = col_integer()))

lookup <- read_csv(url("https://opendata.arcgis.com/datasets/41828627a5ae4f65961b0e741258d210_0.csv"), col_types = cols(LTLA17CD = col_character(),  LTLA17NM = col_character(),  UTLA17CD = col_character(),  UTLA17NM = col_character(),  FID = col_integer()))

# This is a lower tier LA to upper tier LA lookup
UA <- subset(lookup, LTLA17NM == UTLA17NM)

Region <- read_csv(url("https://opendata.arcgis.com/datasets/cec20f3a9a644a0fb40fbf0c70c3be5c_0.csv"), col_types = cols(RGN17CD = col_character(),  RGN17NM = col_character(),  RGN17NMW = col_character(),  FID = col_integer()))
colnames(Region) <- c("Area_Code", "Area_Name", "Area_Name_Welsh", "FID")

Region$Area_Type <- "Region"
Region <- Region[c("Area_Code", "Area_Name", "Area_Type")]

LAD <- subset(LAD, substr(LAD$LAD17CD, 1, 1) == "E")
LAD$Area_Type <- ifelse(LAD$LAD17NM %in% UA$LTLA17NM, "Unitary Authority", "District")
colnames(LAD) <- c("Area_Code", "Area_Name", "Area_Name_Welsh", "FID", "Area_Type")
LAD <- LAD[c("Area_Code", "Area_Name", "Area_Type")]

Counties$Area_type <- "County"
colnames(Counties) <- c("Area_Code", "Area_Name", "Col2", "Col3", "FID", "Area_Type")
Counties <- Counties[c("Area_Code", "Area_Name", "Area_Type")]

England <- data.frame(Area_Code = "E92000001", Area_Name = "England", Area_Type = "Country")

Areas <- rbind(LAD, Counties, Region, England) 
rm(LAD, Counties, Region, England, UA)

main_df <- main_df %>%
  filter(Area_code %in% Areas$Area_Code)

areas <- c('West Sussex', 'East Sussex','Brighton and Hove', 'South East region', 'England')

# i = 1

rm(indicator_1, indicator_2, indicator_3, indicator_4, indicator_5, indicator_6, indicator_7, indicator_8, indicator_9, indicator_10, indicator_11, indicator_12, indicator_13, indicator_14, indicator_15, indicator_16, indicator_17, indicator_18, indicator_19, indicator_20, indicator_21, indicator_22, indicator_23, indicator_24, indicator_25, indicator_26, indicator_27, indicator_28, indicator_29, indicator_30, indicator_31, indicator_32, indicator_33, indicator_34, indicator_35, indicator_36, indicator_37, indicator_38, indicator_39, ind_3_latest_ut_year)

# We can define some hex colours to use later in our comparisons
better <- "#3ECC26"
no_diff <- "#E7AF27"
worse <- "#CC2629"
not_applic <- "#8E8E8E"
higher = "#BED2FF"
lower = "#5555E6"

areas_wo_comp <- setdiff(areas, c('South East region', 'England')) # this is areas without areas; south east region and england

# Compiling data for export to json ####

meta <- main_df %>% 
  select(ID, Name, Description, Unit, Timeperiod, Polarity, img_path) %>% 
  unique() %>% 
  mutate(Number = row_number()) %>%
  mutate(x = ifelse(Number == 1, .16, ifelse(Number == 2, .24, ifelse(Number == 3, .32, ifelse(Number == 4, .43, ifelse(Number == 5, .51, ifelse(Number == 6, .59, ifelse(Number == 7, .74, ifelse(Number == 8, .82, ifelse(Number == 9, .9, ifelse(Number == 10, .92, ifelse(Number == 11, .83, ifelse(Number == 12, .74, ifelse(Number == 13, .66, ifelse(Number == 14, .58, ifelse(Number == 15, .5, ifelse(Number == 16, .42, ifelse(Number == 17, .24, ifelse(Number == 18, .16, ifelse(Number == 19, .08, ifelse(Number == 20, .1, ifelse(Number == 21, .19, ifelse(Number == 22, .28, ifelse(Number == 23, .37, ifelse(Number == 24, .46, ifelse(Number == 25, .54, ifelse(Number == 26, .68, ifelse(Number == 27, .76, ifelse(Number == 28, .84, ifelse(Number == 29, .92, ifelse(Number == 30, .92, ifelse(Number == 31, .83, ifelse(Number == 32, .725, ifelse(Number == 33, .56, ifelse(Number == 34, .48, ifelse(Number == 35, .4,  ifelse(Number == 36, .32, ifelse(Number == 37, .24, ifelse(Number == 38, .16, ifelse(Number == 39, .08, NA)))))))))))))))))))))))))))))))))))))))) %>% 
  mutate(y = ifelse(Number %in% c(1:9), .05, ifelse(Number %in% c(10:19), .3, ifelse(Number %in% c(20:29), .55, ifelse(Number %in% c(30:39), .8, NA))))) 


meta %>% 
  toJSON() %>% 
  write_lines(paste0(github_repo_dir, '/ut_data_meta_extract.json'))

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
  mutate(Area_name = factor(Area_name, levels = areas_wo_comp)) %>% 
  arrange(Area_name) %>% 
  toJSON() %>% 
  write_lines(paste0(github_repo_dir, '/ut_data_extract_compare_england.json'))

# Nearest neighbour rank ####
i = 3

area_x <- areas_wo_comp[i]
area_x_code <- as.character(unique(subset(main_df, Area_name == area_x, select = 'Area_code')))

neighbours <- data.frame(Area_code = nearest_neighbours(area_x_code, AreaTypeID = 102, measure = 'CIPFA')) %>% 
  mutate(Area_code = ifelse(Area_code == 'E10000009',  'E06000059', as.character(Area_code)))

nn_area_x_main <- main_df %>% 
  filter(Area_code %in% c(neighbours$Area_code, area_x_code))

unique(nn_area_x_main$Area_name)

all_nn_represented <- nn_area_x_main %>% 
  group_by(Name) %>% 
  summarise(n())

#Not all brighton and hove indicators have 15 nn to compare.
