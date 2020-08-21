shinyUI(fluidPage(title = "Population picker",
  # Set the language of the page - important for accessibility
  tags$html(lang = "en-GB"),
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css")
  ),
  tags$header(
    class = "headerContainer",
    a(
      img(
        src = "https://www.trafforddatalab.io/images/trafford_council_logo_black_on_white_100px.png",
        style = "position: relative; top: -5px;",
        height = 60,
        alt = "Trafford Council"
      ),
      href = "https://www.trafford.gov.uk",
      target = "_blank"
    ),
    h1("Population picker")
  ),
  br(),
  tags$main(
    fluidRow(
      uiOutput("pop_map"),
      uiOutput("pop_table"),
      uiOutput("pop_plot")
    )
  ),
  br(),
  tags$footer(
      "Developed in ",
      a(href = "https://cran.r-project.org/", target = "_blank", "R"),
      " by the ",
      a(href = "https://www.trafforddatalab.io", target = "_blank", "Trafford Data Lab"),
      " under the ",
      a(href = "https://www.trafforddatalab.io/LICENSE.txt", target = "_blank", "MIT"),
      " licence"
  ),
  HTML("
      <script>
          // Remove empty form labels created by Shiny
          var cb_emptyLabels = setInterval(function() {
              var arrEmptyLabels = document.getElementsByClassName('control-label');
      
              if (arrEmptyLabels.length > 0) { 
                  var parent;
              
                  // try - catch in case parent nodes haven't been created yet
                  try {
                      do {
                          parent = arrEmptyLabels[0].parentNode;
                          parent.removeChild(arrEmptyLabels[0]);
                      }
                      while (arrEmptyLabels.length > 0);
                      
                      clearInterval(cb_emptyLabels); // cancel further calls to this fn
                  }
                  catch(e) {
                      // do nothing, wait until function is called again next interval
                  }
              }
          }, 500);
          
          // Add aria-label to map filter button to describe its function for accessibility
          var cb_geographyButton = setInterval(function() {
              try {
                  var btn = document.getElementById('geographySelection');
                  btn.setAttribute('aria-label', 'Choose administrative or statistical geography.');
                  clearInterval(cb_geographyButton); // cancel further calls to this fn
              }
              catch(e) {
                  // do nothing, wait until function is called again next interval
              }
          }, 500);
          
          // Add aria-label to plot cog button to describe its function for accessibility
          var cb_settingsButton = setInterval(function() {
              try {
                  var btn = document.getElementById('plotSettings');
                  btn.setAttribute('aria-label', 'Plot options for the population pyramid.');
                  clearInterval(cb_settingsButton); // cancel further calls to this fn
              }
              catch(e) {
                  // do nothing, wait until function is called again next interval
              }
          }, 500);
          
          // Add aria-label to plot dowload button to describe its function for accessibility
          var cb_downloadButton = setInterval(function() {
              try {
                  var btn = document.getElementById('downloadButton');
                  btn.setAttribute('aria-label', 'Download options.');
                  clearInterval(cb_downloadButton); // cancel further calls to this fn
              }
              catch(e) {
                  // do nothing, wait until function is called again next interval
              }
          }, 500);
          
          // Add labels around the radio buttons for single year and 5 year age bands for accessibility
          var cb_yearBandChoice = setInterval(function() {
              try {
                  var btns = document.getElementsByName('plot_selection');
                  var parent, label;
                  
                  if (btns.length > 1) {
                    for (var i = 0; i < btns.length; i++) {
                      parent = btns[i].parentNode;
                      label = document.createElement('label');
                      
                      // Add all the children of parent to the newly created label
                      while (parent.childNodes.length > 0) {
                        label.appendChild(parent.childNodes[0]);
                      }
                      
                      parent.appendChild(label);    // add the label to the parent
                    }
                  
                    clearInterval(cb_yearBandChoice); // cancel further calls to this fn
                  }
              }
              catch(e) {
                  // do nothing, wait until function is called again next interval
              }
          }, 500);
      </script>
  ")
))
