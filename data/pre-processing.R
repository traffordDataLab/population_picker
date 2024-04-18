# Mid-2022 population estimates

# Source: ONS
# URL: https://www.ons.gov.uk/releases/populationestimatesbyoutputareaselectoralhealthandothergeographiesenglandandwalesmid2021andmid2022
# Licence: Open Government Licence v3.0

library(httr) ; library(readxl) ; library(tidyverse)

# Local authority
la <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1820327969&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  mutate(geography = "Local authority") %>% 
  select(period = DATE_NAME,
         area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         sex = GENDER_NAME,
         age = C_AGE_NAME,
         count = OBS_VALUE,
         geography) %>% 
  mutate(period = ymd(str_c(period, "06-30", sep = "-")),
         sex = fct_recode(sex, "Females" = "Female" , "Males" = "Male", "Persons" = "Total"),
         age = as.integer(str_trim(str_replace_all(age, "Age.|\\+", ""))))  %>% 
  spread(age, count) %>% 
  gather(age, n, -period, -area_code, -area_name, -geography, -sex)
# Electoral ward

tmp <- tempfile(fileext = ".xlsx")


GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/wardlevelmidyearpopulationestimatesexperimental/mid2021andmid2022/sapewardstablefinal.xlsx",
    write_disk(tmp))


ward <- read_xlsx(tmp, sheet = 8, skip = 3) %>%
  filter(`LAD 2023 Code` == "E08000009") %>%
  mutate(geography = "Ward") %>%
  rename(area_code = `Ward 2023 Code`, area_name = `Ward 2023 Name`)

# Middle-layer Super Output Area
tmp <- tempfile(fileext = ".xlsx")


GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates/mid2021andmid2022/sapemsoasyoatablefinal.xlsx",
    write_disk(tmp))

msoa <- read_xlsx(tmp, sheet = 6, skip = 3) %>%
  filter(`LAD 2021 Code` == "E08000009") %>%
  mutate(geography = "MSOA") %>%
  rename(area_code = `MSOA 2021 Code`, area_name = `MSOA 2021 Name`)


# Lower-layer Super Output Area

tmp <- tempfile(fileext = ".xlsx")

GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2021andmid2022/sapelsoasyoatablefinal.xlsx",
    write_disk(tmp))

lsoa <- read_xlsx(tmp, sheet = 6, skip = 3) %>%
  filter(`LAD 2021 Code` == "E08000009") %>%
  mutate(geography = "LSOA") %>%
  rename(area_code = `LSOA 2021 Code`, area_name = `LSOA 2021 Name`)


# Output Area

tmp <- tempfile(fileext = ".xlsx")

GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/censusoutputareapopulationestimatessupportinginformation/mid2022/sapeoatablefinal2022v2.xlsx",
    write_disk(tmp))

oa <- read_xlsx(tmp, sheet = 5, skip = 3) %>%
  filter(`LAD 2021 Code` == "E08000009") %>%
  mutate(geography = "OA") %>%
  rename(area_code = `OA 2021 Code`, area_name = `OA 2021 Code`)

 
all_geographies <- bind_rows(ward, msoa, lsoa, oa) %>%
  pivot_longer(Total:M90, names_to = "age", values_to = "value") %>%
  mutate(period = as.Date("2022-06-30"))%>%
  filter(age != "Total") %>%
  separate(age, into = c("sex", "age"), sep = 1) %>%
  mutate(sex = case_match(sex,"F"~"Females","M"~"Males")) %>%
  pivot_wider(names_from = "sex", values_from = value) %>%
  mutate(Persons = Females + Males) %>%
  pivot_longer(Females:Persons, names_to = "sex", values_to = "n") %>%
  select(period, area_code, area_name, sex, geography, age, n)

all_geographies <- bind_rows(la, all_geographies)


write_csv(all_geographies, "mid-year_population_estimates_all_geographies.csv")
