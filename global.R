library(shiny) ; library(tidyverse) ; library(scales) ; library(ggpol) ; library(sf)
library(leaflet) ; library(htmlwidgets) ; library(DT) ; library(janitor) ; library(shinydashboard)
library(shinyWidgets) ; library(ggiraph)

pop <- read_csv("data/mid-2019_population_estimates_all_geographies.csv")
england <- read_csv("data/mid-2019_population_estimates_england.csv")
