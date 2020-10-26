library(shiny) ; library(tidyverse) ; library(scales) ; library(ggpol) ; library(sf)
library(leaflet) ; library(htmlwidgets) ; library(janitor) ; library(shinydashboard)
library(shinyWidgets) ; library(ggiraph) ; library(reactable)

pop <- read_csv("data/mid-2019_population_estimates_all_geographies.csv")
england <- read_csv("data/mid-2019_population_estimates_england.csv")
