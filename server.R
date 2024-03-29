library(shiny) ; library(tidyverse) ; library(scales) ; library(ggpol) ; library(sf)
library(leaflet) ; library(htmlwidgets) ; library(janitor) ; library(shinydashboard)
library(shinyWidgets) ; library(ggiraph) ; library(reactable)

pop <- read_csv("data/mid-year_population_estimates_all_geographies.csv")


shinyServer(function(input, output) {
    
    layer <- reactive({
        filename <- paste0("data/", input$geography, ".geojson")
        st_read(filename)
    })
    
    clickedIds <- reactiveValues(ids = vector())
    
    observeEvent(input$geography, {
        clickedIds$ids <- NULL
    })
    
    area_data  <- reactive({
        filter(pop, area_code %in% clickedIds$ids)
    })
    
    pyramid_data <- reactive({
        area_data() %>%
                filter(area_code %in% clickedIds$ids, gender != "Persons") %>%
                mutate(age = as.integer(age),
                    ageband = cut(
                        age,
                        breaks = c(
                            0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,120
                        ),
                        labels = c(
                            "0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                            "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                            "70-74","75-79","80-84","85-89","90+"
                        ),
                        right = FALSE
                    )
                )
    })

    ## Map ------------------------------------------------
    
    output$map <- renderLeaflet({
        leaflet() %>%
            setMaxBounds(-2.478454, 53.357425,-2.253022, 53.480362) %>%
            addTiles(
                urlTemplate = "",
                attribution = '<a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2021)</a>',
                options = tileOptions(minZoom = 11, maxZoom = 17)
            ) %>%
            addPolygons(
                data = layer(),
                fillColor = "transparent",
                color = "#000000",
                stroke = T,
                weight = 1,
                label = layer()$area_name,
                labelOptions = labelOptions(
                    direction = 'left',
                    style = list('color' = '#212121', 'font-family' = 'sans-serif')
                ),
                layerId = layer()$area_code
            ) %>%
            htmlwidgets::onRender(
                " function(el, t) {
        var myMap = this;
        myMap._container.style['background'] = '#ffffff';}"
            )
    })
    
    observeEvent(input$map_shape_click, {
        click <- input$map_shape_click
        proxy <- leafletProxy("map")
        clickedIds$ids <- c(clickedIds$ids, click$id)
        clickedPolys <-
            layer()[layer()$area_code %in% clickedIds$ids,]
        if (click$id %in% clickedPolys$area_name) {
            nameMatch <-
                clickedPolys$area_code[clickedPolys$area_name == click$id]
            clickedIds$ids <-
                clickedIds$ids[!clickedIds$ids %in% click$id]
            clickedIds$ids <-
                clickedIds$ids[!clickedIds$ids %in% nameMatch]
            proxy %>% removeShape(layerId = click$id)
            
        } else {
            proxy %>% addPolygons(
                data = clickedPolys,
                fillColor = "#bdbdbd",
                fillOpacity = 0.4,
                weight = 1,
                color = "#212121",
                stroke = T,
                layerId = clickedPolys$area_name
            )
        }
    })
    
    output$pop_map <- renderUI({
        div(
            class = "col-sm-4",
            box(
                width = '100%',
                leafletOutput("map"),
                div(
                    style = "position: absolute; left: 1.7em; bottom: 4em;",
                    dropdown(
                        radioButtons(
                            "geography",
                            label = NULL,
                            choices = list(
                                "Trafford" = "la",
                                "Ward" = "ward",
                                "Middle-layer Super Output Area" = "msoa",
                                "Lower-layer Super Output Area" = "lsoa",
                                "Output Area" = "oa"
                            ),
                            selected = "ward"
                        ),
                        icon = icon("filter"),
                        size = "s",
                        style = "jelly",
                        width = "280px",
                        up = TRUE
                    )
                )
            ),
            tags$style(
                HTML(
                    '.fa {color: #212121;}
            .bttn-jelly.bttn-default{color:#f0f0f0;}
            .bttn-jelly:hover:before{opacity:1};'
                )
            )
        )
        
    })
    
    ## Areas ------------------------------------------------
    
   output$table <- renderReactable({
     shiny::validate(need(nrow(area_data()) != 0, message = FALSE))
        
        temp <- area_data() %>%
            select(area_name, gender, age, n) %>%
            group_by(area_name, gender) %>%
            summarise(n = sum(n)) %>%
            spread(gender, n)
        
        reactable(temp,
                  bordered = TRUE,
                  compact = TRUE,
                  resizable = TRUE,
                  columns = list(
                      area_name = colDef(name = "Area", footer = "Total"),
                      Females = colDef(format = colFormat(separators = TRUE), footer = function(values) sprintf(prettyNum(sum(values), big.mark = ","))),
                      Males = colDef(format = colFormat(separators = TRUE), footer = function(values) sprintf(prettyNum(sum(values), big.mark = ","))),
                      Persons = colDef(format = colFormat(separators = TRUE), footer = function(values) sprintf(prettyNum(sum(values), big.mark = ",")))
                  ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                  )
        
    })
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("mid-year_population_estimates.csv", sep = "")
        },
        content = function(file) {
            write.csv(pyramid_data() %>%
                              group_by(period,
                                       area_code,
                                       area_name,
                                       geography,
                                       gender,
                                       ageband) %>%
                              summarise(n = sum(n)), file, row.names = FALSE)
        }
    )
    
    output$pop_table <- renderUI({
        div(class = "col-sm-4",
            box(
                width = '100%',
                align = "center",
                uiOutput("table_title"),
                reactableOutput('table')
            ),
            div(
                style = "position: absolute; right: 1.5em; bottom: -2em;",
                conditionalPanel(
                    condition = "output.plot",
                    dropdown(
                        downloadButton(outputId = "downloadData", label = "Download data"),
                        icon = icon("download"),
                        size = "s",
                        style = "jelly",
                        up = FALSE,
                        right = TRUE
                    )
                )
            ))
    })
    
    ## Plot ------------------------------------------------
    
    output$plot <- renderGirafe({
        shiny::validate(need(nrow(area_data()) != 0, message = FALSE))
        
        df <- pyramid_data() %>%
                group_by(gender, ageband) %>%
                summarise(n = sum(n)) %>% 
                ungroup() %>% 
                mutate(
                    gender = factor(gender, levels = c("Males", "Females")),
                    age = as.integer(ageband),
                    percent = round(n/sum(n)*100, 1),
                    tooltip = paste0(
                        "<strong>",
                        percent, "% (", comma(n, accuracy = 1), ")",
                        "</strong><br/>",
                        "<em>",
                        gender,
                        "</em><br/>",
                        ageband,
                        " years"
                    ))
          
            gg <-
                ggplot(df, aes(x = ageband, y = ifelse(gender == "Males", -percent, percent), fill = gender)) +
                geom_bar_interactive(aes(tooltip = tooltip), stat = "identity", alpha = 0.6) +
                scale_y_continuous(labels = abs) +
                scale_fill_manual(values = c("Males" = "#44B7C2", "Females" = "#024B7A")) +
                labs(
                    x = NULL,
                    y = "% of total population",
                    title = NULL,
                    subtitle = NULL,
                    caption = "Source: Office for National Statistics"
                ) +
                coord_flip() +
                theme_minimal(base_size = 14, base_family = "Open Sans") +
                theme(plot.margin = unit(rep(0.5, 4), "cm"),
                      panel.grid.major.y = element_blank(),
                      panel.grid.minor = element_blank(),
                      plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
                      axis.title.x = element_text(),
                      legend.position = "none")

            gg <- girafe(ggobj = gg)
            girafe_options(gg,
                           opts_tooltip(use_fill = TRUE),
                           opts_toolbar(saveaspng = FALSE))
    })
    
    output$plot_title <- renderUI({
      shiny::validate(need(nrow(area_data()) != 0,  message = FALSE))
        
        HTML(
          paste0(
            "Mid-year ",
            strong(format(pyramid_data()$period[[1]], "%Y")),
          br(),
            #paste0(
                strong(prettyNum(
                    sum(area_data()[area_data()$gender == "Persons", ]$n),
                    big.mark = ",",
                    scientific = FALSE
                ),
                " residents"),
                br(),
                round(
                    sum(area_data()[area_data()$gender == "Males", ]$n) / sum(area_data()[area_data()$gender == "Persons", ]$n) *
                        100,
                    1
                ),
                "% Male | ",
                round(
                    sum(area_data()[area_data()$gender == "Females", ]$n) / sum(area_data()[area_data()$gender == "Persons", ]$n) *
                        100,
                    1
                ),
                "% Female"
            )
        )
    })
    
    output$pop_plot <- renderUI({
        div(
            class = "col-sm-4",
            box(
                width = '100%',
                align = "center",
                htmlOutput("plot_title", inline = TRUE),
                girafeOutput("plot")
        )
        )
        
    })
    
})