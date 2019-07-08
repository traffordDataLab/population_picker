library(shiny)
library(tidyverse)
library(scales)
library(ggpol)
library(sf)
library(leaflet)
library(htmlwidgets)
library(DT)
library(janitor)
library(shinydashboard)
library(shinyWidgets)
library(ggiraph)

la <-
  read_csv(
    "http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1946157089&date=latest&gender=0...2&c_age=200,101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,obs_value"
  ) %>%
  select(
    area_code = GEOGRAPHY_CODE,
    area_name = GEOGRAPHY_NAME,
    gender = GENDER_NAME,
    age = C_AGE_NAME,
    count = OBS_VALUE
  ) %>%
  filter(age != "All Ages") %>%
  mutate(
    date = as.Date("2018-06-30", format = '%Y-%m-%d'),
    gender = fct_recode(
      gender,
      "Females" = "Female" ,
      "Males" = "Male",
      "Persons" = "Total"
    ),
    age = as.integer(str_trim(str_replace_all(age, "Age.|\\+", "")))
  )  %>%
  spread(age, count) %>%
  mutate(
    geography = "Local Authority",
    all_ages = rowSums(select(., `0`:`90`)),
    aged_0_to_15 = rowSums(select(., `0`:`15`)),
    aged_16_to_64 = rowSums(select(., `16`:`64`)),
    aged_65_and_over = rowSums(select(., `65`:`90`))
  ) %>%
  select(
    date,
    area_code,
    area_name,
    geography,
    gender,
    all_ages,
    aged_0_to_15,
    aged_16_to_64,
    aged_65_and_over,
    everything()
  )

other_areas <-
  read_csv(
    "https://github.com/traffordDataLab/open_data/raw/master/mid-2017_population_estimates/mid-2017_population_estimates_all_geographies.csv"
  ) %>%
  filter(geography != "Local Authority")

pop <- bind_rows(la, other_areas) %>%
  select(-c(all_ages, aged_0_to_15, aged_16_to_64, aged_65_and_over)) %>%
  gather(age, count, -date, -area_code, -area_name, -geography, -gender) %>%
  mutate(age = as.integer(age))