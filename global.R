library(shiny) ; library(tidyverse) ; library(scales) ; library(ggpol) ; library(sf)
library(leaflet) ; library(htmlwidgets) ; library(DT) ; library(janitor) ; library(shinydashboard)
library(shinyWidgets) ; library(ggiraph)

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

pop <- read_csv("data/mid-2018_population_estimates_all_geographies.csv")
england <- read_csv("data/mid-2018_population_estimates_england.csv")
