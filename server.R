shinyServer(function(input, output) {
    layer <- reactive({
        filename <- paste0("data/", input$geography, ".geojson")
        st_read(filename)
    })
    
    clickedIds <- reactiveValues(ids = vector())
    
    observeEvent(input$geography, {
        clickedIds$ids <- NULL
    })
    
    ## Map ------------------------------------------------
    
    output$map <- renderLeaflet({
        leaflet() %>%
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
            layer()[layer()$area_code %in% clickedIds$ids, ]
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
    
    area_data  <- reactive({
        filter(pop,
               area_code %in% clickedIds$ids,
               age >= input$age[1],
               age <= input$age[2])
    })
    
    output$pop_map <- renderUI({
        div(
            class = "col-sm-12 col-md-6 col-lg-4",
            box(
                width = '100%',
                includeHTML("help.html"),
                leafletOutput("map"),
                div(
                    style = "position: absolute; left: 2.2em",
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
                            selected = "la"
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
    
    ## Plot ------------------------------------------------
    
    output$plot <- renderggiraph({
        validate(need(clickedIds$ids, message = FALSE))
        
        if (input$plot_selection == "single_year") {
            df <- area_data() %>%
                filter(area_code %in% clickedIds$ids, gender != "Persons") %>%
                mutate(gender = factor(gender, levels = c("Males", "Females")),
                       age = as.integer(age)) %>%
                group_by(gender, age) %>%
                summarise(n = sum(count)) %>%
                mutate(
                    percent = round(n / sum(n) * 100, 1),
                    percent =
                        case_when(
                            gender == "Males" ~ percent * -1,
                            TRUE ~ as.double(percent)
                        ),
                    tooltip = case_when(
                        gender == "Males" ~ paste0(
                            "<strong>",
                            percent * -1,
                            " %",
                            "</strong><br/>",
                            "<em>",
                            gender,
                            "</em><br/>",
                            age,
                            " years"
                        ),
                        TRUE ~ paste0(
                            "<strong>",
                            percent,
                            " %",
                            "</strong><br/>",
                            "<em>",
                            gender,
                            "</em><br/>",
                            age,
                            " years"
                        )
                    )
                )
            
            gg <-
                ggplot(df, aes(
                    x = factor(age),
                    y = percent,
                    fill = gender
                )) +
                geom_bar_interactive(aes(tooltip = tooltip),
                                     stat = "identity") +
                scale_fill_manual(
                    values = c("#7FC5DC", "#7FDCC5"),
                    labels = c("Female", "Male")
                ) +
                facet_share(
                    ~ gender,
                    dir = "h",
                    scales = "free",
                    reverse_num = TRUE
                ) +
                coord_flip() +
                labs(
                    x = NULL,
                    y = NULL,
                    caption = "Source: Office for National Statistics",
                    fill = NULL
                ) +
                theme_minimal() +
                theme(
                    plot.margin = unit(c(0.8, 0, 0, 0), "cm"),
                    panel.spacing = unit(0.05, "lines"),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor = element_blank(),
                    plot.title = element_text(
                        colour = "#757575",
                        size = 14,
                        hjust = 0.5,
                        vjust = 2
                    ),
                    plot.subtitle = element_text(
                        colour = "#757575",
                        size = 11,
                        hjust = 0.5
                    ),
                    strip.text = element_text(
                        colour = "#757575",
                        size = 11,
                        vjust = 1
                    ),
                    axis.text.x = element_text(size = 9),
                    axis.text.y = element_text(size = 9),
                    plot.caption = element_text(
                        size = 9,
                        color = "grey50",
                        hjust = 1,
                        margin = margin(t = 15)
                    ),
                    legend.position = "none"
                )
            
            gg <- girafe(ggobj = gg)
            girafe_options(gg,
                           opts_tooltip(use_fill = TRUE),
                           opts_toolbar(saveaspng = FALSE))
            
        }
        else {
            temp <- area_data() %>%
                filter(area_code %in% clickedIds$ids, gender != "Persons") %>%
                mutate(
                    gender = factor(gender, levels = c("Males", "Females")),
                    age = as.integer(age),
                    ageband = cut(
                        age,
                        breaks = c(
                            0,
                            5,
                            10,
                            15,
                            20,
                            25,
                            30,
                            35,
                            40,
                            45,
                            50,
                            55,
                            60,
                            65,
                            70,
                            75,
                            80,
                            85,
                            90,
                            120
                        ),
                        labels = c(
                            "0-4",
                            "5-9",
                            "10-14",
                            "15-19",
                            "20-24",
                            "25-29",
                            "30-34",
                            "35-39",
                            "40-44",
                            "45-49",
                            "50-54",
                            "55-59",
                            "60-64",
                            "65-69",
                            "70-74",
                            "75-79",
                            "80-84",
                            "85-89",
                            "90+"
                        ),
                        right = FALSE
                    )
                ) %>%
                group_by(gender, ageband) %>%
                summarise(n = sum(count)) %>%
                mutate(
                    percent = round(n / sum(n) * 100, 1),
                    percent =
                        case_when(
                            gender == "Males" ~ percent * -1,
                            TRUE ~ as.double(percent)
                        ),
                    tooltip = case_when(
                        gender == "Males" ~ paste0(
                            "<strong>",
                            percent * -1,
                            " %",
                            "</strong><br/>",
                            "<em>",
                            gender,
                            "</em><br/>",
                            ageband,
                            " years"
                        ),
                        TRUE ~ paste0(
                            "<strong>",
                            percent,
                            " %",
                            "</strong><br/>",
                            "<em>",
                            gender,
                            "</em><br/>",
                            ageband,
                            " years"
                        )
                    )
                )
            
            gg <-
                ggplot(temp, aes(
                    x = ageband,
                    y = percent,
                    fill = gender
                )) +
                geom_bar_interactive(aes(tooltip = tooltip),
                                     stat = "identity") +
                scale_fill_manual(
                    values = c("#7FC5DC", "#7FDCC5"),
                    labels = c("Female", "Male")
                ) +
                facet_share(
                    ~ gender,
                    dir = "h",
                    scales = "free",
                    reverse_num = TRUE
                ) +
                coord_flip() +
                labs(
                    x = NULL,
                    y = NULL,
                    caption = "Source: Office for National Statistics",
                    fill = NULL
                ) +
                theme_minimal() +
                theme(
                    plot.margin = unit(c(0.8, 0, 0, 0), "cm"),
                    panel.spacing = unit(0.05, "lines"),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor = element_blank(),
                    plot.title = element_text(
                        colour = "#757575",
                        size = 14,
                        hjust = 0.5,
                        vjust = 2
                    ),
                    plot.subtitle = element_text(
                        colour = "#757575",
                        size = 11,
                        hjust = 0.5
                    ),
                    strip.text = element_text(
                        colour = "#757575",
                        size = 11,
                        vjust = 1
                    ),
                    axis.text.x = element_text(size = 9),
                    axis.text.y = element_text(size = 9),
                    plot.caption = element_text(
                        size = 9,
                        color = "grey50",
                        hjust = 1,
                        margin = margin(t = 15)
                    ),
                    legend.position = "none"
                )
            
            gg <- girafe(ggobj = gg)
            girafe_options(gg,
                           opts_tooltip(use_fill = TRUE),
                           opts_toolbar(saveaspng = FALSE))
            
        }
    })
    
    output$plot_title <- renderUI({
        validate(need(clickedIds$ids, message = FALSE))
        
        HTML(paste0(
            h4("Population pyramid, ", format(
                as.Date(unique(area_data()$date), format = "%Y-%b-%d"), "%Y"
            ), style = "color:#757575;"),
            prettyNum(
                sum(area_data()[area_data()$gender == "Persons",]$count),
                big.mark = ",",
                scientific = FALSE
            ),
            " residents (",
            round(
                sum(area_data()[area_data()$gender == "Males",]$count) / sum(area_data()[area_data()$gender == "Persons",]$count) *
                    100,
                1
            ),
            "% Male | ",
            round(
                sum(area_data()[area_data()$gender == "Females",]$count) / sum(area_data()[area_data()$gender == "Persons",]$count) *
                    100,
                1
            ),
            "% Female)"
        ))
    })
    
    
    output$slide <- renderUI({
        validate(need(clickedIds$ids, message = FALSE))
        
        if (input$plot_selection == "single_year") {
            sliderInput(
                "age",
                label = NULL,
                min = 0,
                max = 90,
                value = c(0, 90),
                step = 1,
                ticks = TRUE,
                post = " years"
            )
            
        }
        else {
            sliderInput(
                "age",
                label = NULL,
                min = 0,
                max = 90,
                value = c(0, 90),
                step = 5,
                ticks = TRUE,
                post = " years"
            )
            
        }
        
    })
    
    output$pop_plot <- renderUI({
        validate(need(clickedIds$ids, message = FALSE))
        
        div(
            class = "col-sm-12 col-md-6 col-lg-4",
            box(
                width = '100%',
                align = "center",
                htmlOutput("plot_title", inline = TRUE),
                ggiraphOutput("plot"),
                br(),
                uiOutput("slide")
            ),
            div(
                style = "position: absolute; left: 1.5em",
                dropdown(
                    radioGroupButtons(
                        inputId = "plot_selection",
                        label = NULL,
                        choiceNames = c("Single year of age", "Five year age bands"),
                        choiceValues = c("single_year", "five_years"),
                        selected = "five_years",
                        direction = "vertical"
                    ),
                    icon = icon("cog"),
                    size = "s",
                    style = "jelly",
                    width = "200px",
                    up = TRUE
                )
            )
        )
    })
    
    ## Table ------------------------------------------------
    
    output$table <- renderTable({
        validate(need(clickedIds$ids, message = FALSE))
        
        area_data() %>%
            select(area_name, gender, age, count) %>%
            group_by(area_name, gender) %>%
            summarise(n = sum(count)) %>%
            spread(gender, n) %>%
            rename(Area = area_name) %>%
            adorn_totals("row") %>%
            mutate(
                Females = prettyNum(Females, big.mark = ",", scientific = FALSE),
                Males = prettyNum(Males, big.mark = ",", scientific = FALSE),
                Persons = prettyNum(Persons, big.mark = ",", scientific = FALSE)
            )
        
    }, bordered = TRUE, align = 'l')
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("mid-year_population_estimates.csv", sep = "")
        },
        content = function(file) {
            write.csv(
                area_data() %>%
                    filter(gender != "Total") %>%
                    mutate(Year = format(
                        as.Date(date, format = "%Y-%b-%d"), "%Y"
                    )) %>%
                    select(
                        Year,
                        `Area code` = area_code,
                        `Area name` = area_name,
                        Geography = geography,
                        Gender = gender,
                        Age = age,
                        Count = count
                    ) %>%
                    group_by(`Area code`) %>%
                    arrange(Age),
                file,
                row.names = FALSE
            )
        }
    )
    
    # output$download <- renderUI({
    #     div(
    #         style = "position: absolute; left: 1.5em; bottom: 0.5em;",
    #         dropdown(
    #             downloadButton(outputId = "downloadData", label = "Download data for single year of age"),
    #             icon = icon("download"),
    #             size = "s",
    #             style = "jelly",
    #             width = "200px",
    #             up = TRUE
    #         )
    #     )
    # })
    # 
    output$pop_table <- renderUI({
        validate(need(clickedIds$ids, message = FALSE))
        
        div(class = "col-sm-12 col-md-6 col-lg-4",
            box(
                width = '100%',
                align = "center",
                HTML(paste(
                    h4("Population by area", style = "color:#757575;")
                )),
                tableOutput('table')
            ),
            div(
                style = "position: absolute; left: 1.5em;",
                dropdown(
                    downloadButton(outputId = "downloadData", label = "Download data for single year of age"),
                    icon = icon("download"),
                    size = "s",
                    style = "jelly",
                    up = TRUE
                )
            ))
    })
    
})
