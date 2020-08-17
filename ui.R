shinyUI(fluidPage(
  # Set the language of the page - important for accessibility
  tags$html(lang = "en-GB"),
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css")
  ),
  titlePanel(
    div(
      class = "headerContainer",
      a(
        img(
          src = "https://github.com/traffordDataLab/traffordDataLab.github.io/raw/master/images/trafford_council_logo_black_on_white_100px.png",
          style = "position: relative; top: -5px;",
          height = 60,
          alt = "Trafford Council"
        ),
        href = "https://www.trafford.gov.uk",
        target = "_blank"
      ),
      h1("Population picker")
    ),
    windowTitle = "Population picker"
  ),
  br(),
  fluidRow(
    uiOutput("pop_map"),
    uiOutput("pop_table"),
    uiOutput("pop_plot")
  ),
  br(),
  br(),
  br(),
  tags$footer(
      "Developed in ",
      a(href = "https://cran.r-project.org/", target = "_blank", "R"),
      " by the ",
      a(href = "https://www.trafforddatalab.io", target = "_blank", "Trafford Data Lab"),
      " under the ",
      a(href = "https://www.trafforddatalab.io/LICENSE.txt", target = "_blank", "MIT"),
      " licence"
  )
))
