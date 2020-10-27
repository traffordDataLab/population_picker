shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tags$style(HTML("tr:last-child { font-weight: bold }")),
  titlePanel(
    div(
      class = "headerContainer",
      a(
        img(
          src = "https://github.com/traffordDataLab/traffordDataLab.github.io/raw/master/images/trafford_council_logo_black_on_white_100px.png",
          style = "position: relative; top: -5px;",
          height = 60
        ),
        href = "https://www.trafford.gov.uk",
        target = "_blank"
      ),
      "Population picker"
    ),
    windowTitle = "Population picker"
  ),
  br(),
  fluidRow(
    div(class = "col-sm-4",
        includeHTML("help.html")
        )
    ),
  fluidRow(
    uiOutput("pop_map"),
    uiOutput("pop_plot"),
    uiOutput("pop_table")
  ),
  br(),
  br(),
  br(),
  tags$footer(
    fluidRow(
      "Developed in ",
      a(href = "https://cran.r-project.org/", target = "_blank", "R"),
      " by the ",
      a(href = "https://www.trafforddatalab.io", target = "_blank", "Trafford Data Lab"),
      " under the ",
      a(href = "https://www.trafforddatalab.io/LICENSE.txt", target = "_blank", "MIT"),
      " licence"
    ),
    style = "position:fixed; text-align:center; left: 0; bottom:0; width:100%; z-index:1000; height:30px; color: #7C7C7C; padding: 5px 20px; background-color: #E7E7E7"
  )
))
