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

theme_x <- function () { 
  theme_minimal(base_size = 12, base_family = "Open Sans") %+replace% 
    theme(
      plot.margin = unit(c(0.8, 0, 0, 0), "cm"),
      panel.spacing = unit(0.05, "lines"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        colour = "#212121",
        size = 14,
        hjust = 0.5,
        vjust = 2
      ),
      plot.subtitle = element_text(
        colour = "#212121",
        size = 11,
        hjust = 0.5
      ),
      strip.text = element_text(
        colour = "#212121",
        size = 11,
        vjust = 1
      ),
      axis.title.x = element_text(colour = "#212121", size = 10, margin = margin(t = 5)),
      axis.text.x = element_text(colour = "#212121", size = 9),
      axis.text.y = element_text(colour = "#212121", size = 9),
      plot.caption = element_text(
        size = 9,
        colour = "#757575",
        hjust = 1,
        margin = margin(t = 15)
      ),
      legend.position = "none"
    )
}

la <-
  read_csv(
    "http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1946157089&date=latest&gender=0...2&c_age=200,101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,obs_value"
  ) %>%
  select(
    area_code = GEOGRAPHY_CODE,
    area_name = GEOGRAPHY_NAME,
    gender = GENDER_NAME,
    age = C_AGE_NAME,
    n = OBS_VALUE
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
  spread(age, n) %>%
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
  gather(age, n, -date, -area_code, -area_name, -geography, -gender) %>%
  mutate(age = as.integer(age))

england <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=2092957699&date=latest&gender=1,2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_name = GEOGRAPHY_NAME, gender = GENDER_NAME, age = C_AGE_NAME, n = OBS_VALUE) %>% 
  mutate(gender = fct_recode(gender, "Females" = "Female", "Males" = "Male"),
         age = as.integer(parse_number(age)))


