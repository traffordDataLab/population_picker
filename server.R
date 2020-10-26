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
        if (input$plot_selection == "single_year") {
            df <- area_data() %>%
                filter(area_code %in% clickedIds$ids, gender != "Persons")
        }
        else {
            df <- area_data() %>%
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
        }
        
    })
    
    england_data <- reactive({
        if (input$plot_selection == "single_year") {
            df <- england %>% 
                group_by(gender, age) %>% 
                summarise(n = sum(n)) %>%
                ungroup() %>% 
                mutate(gender = factor(gender, levels = c("Males", "Females")),
                       percent = round(n/sum(n)*100, 1),
                       percent = 
                           case_when(
                               gender == "Males" ~ percent*-1,
                               TRUE ~ as.double(percent)))
        }
        else {
            df <- england %>%
                mutate(ageband = cut(age,
                                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,120),
                                     labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                                                "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                                                "75-79","80-84","85-89","90+"),
                                     right = FALSE)) %>% 
                group_by(gender, ageband) %>%
                summarise(n = sum(n)) %>% 
                ungroup() %>% 
                mutate(gender = factor(gender, levels = c("Males", "Females")),
                       percent = round(n/sum(n)*100, 1),
                       percent = case_when(
                           gender == "Males" ~ percent * -1, TRUE ~ as.double(percent)))
            
            
            
        }
        
    })

            
    ## Map ------------------------------------------------
    
    output$map <- renderLeaflet({
        leaflet() %>%
            setMaxBounds(-2.478454, 53.357425,-2.253022, 53.480362) %>%
            addTiles(
                urlTemplate = "",
                attribution = '<a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2019)</a>',
                options = tileOptions(minZoom = 11, maxZoom = 17)
            ) %>%
            addPolygons(
                data = layer(),
                fillColor = "transparent",
                fillOpacity = 0.4,
                color = "#212121",
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
                fillColor = "#FFFF00",
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
            class = "col-sm-12 col-md-6 col-lg-4",
            box(
                width = '100%',
                includeHTML("help.html"),
                leafletOutput("map"),
                div(
                    style = "position: absolute; left: 1.7em; bottom: 4em;",
                    dropdown(
                        radioButtons(
                            "geography",
                            label = NULL,
                            choices = list(
                                "District" = "la",
                                "Ward" = "ward",
                                "MSOA" = "msoa",
                                "LSOA" = "lsoa",
                                "OA" = "oa"
                            ),
                            selected = "ward"
                        ),
                        icon = icon("filter"),
                        size = "s",
                        style = "jelly",
                        width = "110px",
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
    
    output$table_title <- renderUI({
        validate(need(nrow(area_data()) != 0, message = FALSE))
        
        HTML(paste(h4("Population by area", style = "color:#757575;")))
        
    })
    
    output$table <- renderTable({
        validate(need(nrow(area_data()) != 0, message = FALSE))
        
        area_data() %>%
            select(area_name, gender, age, n) %>%
            group_by(area_name, gender) %>%
            summarise(n = sum(n)) %>%
            spread(gender, n) %>%
            rename(Area = area_name) %>%
            adorn_totals("row") %>%
            mutate(
                Females = prettyNum(Females, big.mark = ",", scientific = FALSE),
                Males = prettyNum(Males, big.mark = ",", scientific = FALSE),
                Persons = prettyNum(Persons, big.mark = ",", scientific = FALSE)
            ) %>% 
            select(Area, Males, Females, Persons)
        
    }, bordered = TRUE, align = 'l')
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("mid-year_population_estimates.csv", sep = "")
        },
        content = function(file) {
            
            if (input$plot_selection == "single_year") {
                write.csv(pyramid_data(), file, row.names = FALSE)
            }
            else {
                write.csv(pyramid_data() %>%
                              group_by(period,
                                       area_code,
                                       area_name,
                                       geography,
                                       gender,
                                       ageband) %>%
                              summarise(n = sum(n)), file, row.names = FALSE)
            }
        }
    )
    
    output$pop_table <- renderUI({
        div(class = "col-sm-12 col-md-6 col-lg-4",
            box(
                width = '100%',
                align = "center",
                uiOutput("table_title"),
                tableOutput('table')
            ))
    })
    
    ## Plot ------------------------------------------------
    
    output$plot <- renderggiraph({
        validate(need(nrow(area_data()) != 0, message = FALSE))
        
        if (input$plot_selection == "single_year") {
            df <- pyramid_data() %>%
                group_by(gender, age) %>%
                summarise(n = sum(n)) %>% 
                ungroup() %>% 
                mutate(
                    gender = factor(gender, levels = c("Males", "Females")),
                    age = as.integer(age),
                    percent = round(n/sum(n)*100, 1),
                    tooltip = paste0(
                        "<strong>",
                        percent, "% (", comma(n, accuracy = 1), ")",
                        "</strong><br/>",
                        "<em>",
                        gender,
                        "</em><br/>",
                        age,
                        " years"
                    ))
                
            gg <-
                ggplot(df, aes(x = age, y = ifelse(gender == "Males", -percent, percent), fill = gender)) +
                geom_bar_interactive(aes(tooltip = tooltip),  stat = "identity", alpha = 0.6) +
                scale_y_continuous(labels = abs) +
                scale_fill_manual(values = c("Males" = "#7FC5DC", "Females" = "#7FDCC5")) +
                labs(
                    x = NULL,
                    y = "% of total population",
                    caption = "Source: Office for National Statistics",
                    fill = NULL
                ) +
                coord_flip() +
                theme_minimal(base_size = 12) +
                theme(plot.margin = unit(rep(0.5, 4), "cm"),
                      panel.grid.major.y = element_blank(),
                      panel.grid.minor = element_blank(),
                      plot.title.position = "plot",
                      plot.title = element_text(size = 14, face = "bold"),
                      plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
                      plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
                      legend.position = "none")
            
            if(input$england == TRUE){
                gg <- gg + geom_line(data = england_data(), 
                                     aes(x = age, y = percent, group = gender, colour = gender), stat = "identity", size = 1) +
                    scale_colour_manual(values = c("#7FC5DC", "#7FDCC5"), labels = c("Female", "Male"))
            }
            
            gg <- girafe(ggobj = gg)
            girafe_options(gg,
                           opts_tooltip(use_fill = TRUE),
                           opts_toolbar(saveaspng = FALSE))
            
        }
        else {
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
                scale_fill_manual(values = c("Males" = "#7FC5DC", "Females" = "#7FDCC5")) +
                labs(
                    x = NULL,
                    y = "% of total population",
                    caption = "Source: Office for National Statistics",
                    fill = NULL
                ) +
                coord_flip() +
                theme_minimal(base_size = 12) +
                theme(plot.margin = unit(rep(0.5, 4), "cm"),
                      panel.grid.major.y = element_blank(),
                      panel.grid.minor = element_blank(),
                      plot.title.position = "plot",
                      plot.title = element_text(size = 14, face = "bold"),
                      plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
                      plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
                      legend.position = "none")
            
            if(input$england == TRUE){
                gg <- gg + geom_line(data = england_data(), 
                                     aes(x = ageband, y = percent, group = gender, colour = gender), stat = "identity", size = 1) +
                    scale_colour_manual(values = c("#7FC5DC", "#7FDCC5"), labels = c("Female", "Male"))
            }

            gg <- girafe(ggobj = gg)
            girafe_options(gg,
                           opts_tooltip(use_fill = TRUE),
                           opts_toolbar(saveaspng = FALSE))
            
        }
    })
    
    output$plot_title <- renderUI({
        validate(need(nrow(area_data()) != 0,  message = FALSE))
        
        HTML(
            paste0(
                h4("Age profile, ", format(
                    as.Date(unique(area_data()$period), format = "%Y-%b-%d"), "%Y"
                ), style = "color:#757575;"),
                prettyNum(
                    sum(area_data()[area_data()$gender == "Persons", ]$n),
                    big.mark = ",",
                    scientific = FALSE
                ),
                " residents",
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
            class = "col-sm-12 col-md-6 col-lg-4",
            box(
                width = '100%',
                align = "center",
                htmlOutput("plot_title", inline = TRUE),
                ggiraphOutput("plot"),
                br()
            ),
            div(
                style = "position: absolute; left: 1.5em; bottom: 0.5em;",
                conditionalPanel(
                    condition = "output.plot",
                    dropdown(
                        radioGroupButtons(
                            inputId = "plot_selection",
                            label = NULL,
                            choiceNames = c("Single year of age", "Five year age bands"),
                            choiceValues = c("single_year", "five_years"),
                            direction = "vertical",
                            selected = "single_year"
                        ),
                        checkboxInput(
                            inputId = "england",
                            label = "England (2019)",
                            value = FALSE
                        ),
                        icon = icon("cog"),
                        size = "s",
                        style = "jelly",
                        width = "200px",
                        up = TRUE
                    )
                )
            ),
            div(
                style = "position: absolute; left: 4.5em; bottom: 0.5em;",
                conditionalPanel(
                    condition = "output.plot",
                    dropdown(
                        downloadButton(outputId = "downloadData", label = "Download data"),
                        icon = icon("download"),
                        size = "s",
                        style = "jelly",
                        up = TRUE
                    )
                )
            )
        )
        
    })
    
})