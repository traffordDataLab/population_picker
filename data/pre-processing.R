# Mid-2020 population estimates

# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2020
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(lubridate)

api_key <- "" #To download more rows than the Nomis 25,000  limit 

# Local authority
la <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1820327969&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  mutate(geography = "Local authority")

# Electoral ward
ward <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1660945005...1660945019,1660945021,1660945020,1660945022...1660945025&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  mutate(geography = "Electoral ward")

# Middle-layer Super Output Area
msoa <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1245709510...1245709537&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  mutate(geography = "MSOA")

# Lower-layer Super Output Area
lsoa <- read_csv(paste0("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?uid=", api_key, "&geography=1249908541...1249908544,1249908617,1249908620,1249908548...1249908551,1249908553,1249908573,1249908618,1249908619,1249908621,1249908545...1249908547,1249908577,1249908578,1249908554...1249908556,1249908560,1249908563,1249908587,1249908589,1249908591,1249908614,1249908615,1249908557...1249908559,1249908562,1249908564,1249908588,1249908590,1249908611,1249908616,1249908630...1249908634,1249908552,1249908561,1249908565,1249908629,1249908635,1249908574...1249908576,1249908612,1249908613,1249908579,1249908581,1249908582,1249908586,1249908597,1249908598,1249908601...1249908603,1249908530,1249908531,1249908596,1249908606,1249908610,1249908529,1249908592...1249908595,1249908580,1249908583...1249908585,1249908604,1249908536...1249908540,1249908534,1249908605,1249908607...1249908609,1249908523,1249908524,1249908527,1249908599,1249908600,1249908522,1249908526,1249908528,1249908532,1249908535,1249908533,1249908622,1249908627,1249908628,1249908642,1249908636,1249908638...1249908640,1249908643,1249908525,1249908623,1249908624,1249908637,1249908641,1249908510,1249908512,1249908521,1249908625,1249908626,1249908506...1249908508,1249908511,1249908519,1249908515,1249908516,1249908520,1249908571,1249908572,1249908509,1249908513,1249908514,1249908517,1249908518,1249908566...1249908570&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name")) %>% 
  mutate(geography = "LSOA")


# Output Area
oa <- read_csv(paste0("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?uid=", api_key, "&geography=1254126722...1254127431,1254260803...1254260823&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name")) %>% 
  mutate(geography = "OA")

all_geographies <- bind_rows(la, ward, msoa, lsoa, oa) %>% 
  select(period = DATE_NAME,
         area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         gender = GENDER_NAME,
         age = C_AGE_NAME,
         count = OBS_VALUE,
         geography) %>% 
  mutate(period = ymd(str_c(period, "06-30", sep = "-")),
         gender = fct_recode(gender, "Females" = "Female" , "Males" = "Male", "Persons" = "Total"),
         age = as.integer(str_trim(str_replace_all(age, "Age.|\\+", ""))))  %>% 
  spread(age, count) %>% 
  gather(age, n, -period, -area_code, -area_name, -geography, -gender) %>%
  mutate(age = as.integer(age))

write_csv(all_geographies, "mid-year_population_estimates_all_geographies.csv")
