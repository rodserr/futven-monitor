
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # Inputs----
      # Vin----
    data_eliminatorias_raw <- reactive(wcq_ven)
    
    output$vin_select_pos_ui <- renderUI({
        
        p <- unique(data_eliminatorias_raw()$pos)
        
        shinyWidgets::pickerInput(
            inputId = "vin_select_pos",
            label = "Filtrar Posicion:",
            choices = p,
            selected = p,
            multiple = T,
            options = list(
                `actions-box` = TRUE
            ),
            choicesOpt = list(
                style = rep("color: white;", length(p))
            )
        )
        
    })
    
    output$vin_select_player_ui <- renderUI({

        .choices <- split(data_eliminatorias_raw()$player, data_eliminatorias_raw()$pos)

        pickerInput(
            inputId = "vin_select_player",
            label = "Destacar Jugador:",
            choices = .choices,
            selected = NULL,
            multiple = T,
            options = pickerOptions(
                maxOptions = 1,
                liveSearch = TRUE,
                size = 10,
                noneSelectedText = 'Ningún jugador seleccionado'
            ),
            choicesOpt = list(
                style = rep(("color: white;"), length(data_eliminatorias_raw()$player))
            )
        )

    })

    observeEvent(input$vin_select_pos,
        {

            p <- data_eliminatorias_raw() %>% filter(pos %in% input$vin_select_pos)
            .choices <- split(p$player, p$pos)
            freezeReactiveValue(input, "vin_select_player")
            updatePickerInput(
                session = session, 
                inputId = "vin_select_player", 
                choices = .choices,
                selected = NULL,
                choicesOpt = list(
                    style = rep(("color: white;"), length(data_eliminatorias_raw()$player))
                )
            )

        })
    
    data_eliminatorias <- reactive({
        
        req(input$vin_select_pos)
        
        data_eliminatorias_raw() %>% 
            filter(pos %in% input$vin_select_pos)
        
    })
    
    output$vin_age <- renderText({
        
        edad <- mean(data_eliminatorias()$age)
        
        HTML(sprintf("<div> Edad Promedio <br> <strong> %s años </strong> </div>", round(edad, 1)))
        
    })
    
    output$vin_used_players <- renderText({
        
        c <- data_eliminatorias() %>% filter(min >= 1) %>% nrow()
        
        HTML(sprintf("<div> Cantidad de Jugadores con al menos 1 min de juego <br> <strong> %s </strong>  </div>", c))
        
    })
    
      # Venex----
    venex_clean <- reactive(venex)
    
    output$venex_select_pos_ui <- renderUI({
        
        p <- unique(venex_clean()$pos)
        
        shinyWidgets::pickerInput(
            inputId = "venex_select_pos",
            label = "Filtrar Posicion:",
            choices = p,
            selected = p,
            multiple = T,
            options = list(
                `actions-box` = TRUE
            ),
            choicesOpt = list(
                style = rep("color: white;", length(p))
            )
        )
        
    })

    output$venex_select_country_ui <- renderUI({
        
        p <- unique(venex_clean()$country)
        
        shinyWidgets::pickerInput(
            inputId = "venex_select_country",
            label = "Filtrar País:",
            choices = p,
            selected = p,
            multiple = T,
            options = list(
                `actions-box` = TRUE
            ),
            choicesOpt = list(
                style = rep("color: white;", length(p))
            )
        )
        
    })
    
    output$venex_select_player_ui <- renderUI({
        
        .choices <- split(venex_clean()$player, venex_clean()$pos)
        
        pickerInput(
            inputId = "venex_select_player",
            label = "Destacar Jugador:",
            choices = .choices,
            selected = NULL,
            multiple = T,
            options = pickerOptions(
                maxOptions = 1,
                liveSearch = TRUE,
                size = 10,
                noneSelectedText = 'Ningún jugador seleccionado'
            ),
            choicesOpt = list(
                style = rep(("color: white;"), length(venex_clean()$player))
            )
        )
        
    })
    
    observeEvent(c(input$venex_select_pos, input$venex_select_country),
                 {
                     
                     p <- venex_clean() %>% 
                         filter(
                             pos %in% input$venex_select_pos,
                             country %in% input$venex_select_country
                        )
                     
                     
                     
                     .choices <- split(p$player, p$pos)
                     freezeReactiveValue(input, "venex_select_player")
                     updatePickerInput(
                         session = session, 
                         inputId = "venex_select_player", 
                         choices = .choices,
                         selected = NULL,
                         choicesOpt = list(
                             style = rep(("color: white;"), length(venex_clean()$player))
                         )
                     )
                     
                 })
    
    venex_filter <- reactive({
        v <- venex_clean() %>% 
            filter(
                country %in% input$venex_select_country,
                pos %in% input$venex_select_pos
            )
        
        validate(
            need(nrow(v) > 0, 'No existen jugadores con la combinación actual de parámetros')
        )
        
        v
        
    })
    
    output$venex_age <- renderText({
      
      edad <- mean(venex_filter()$age)
      
      HTML(sprintf("<div> Edad Promedio <br> <strong> %s años </strong> </div>", round(edad, 1)))
      
    })
    
    output$venex_count <- renderText({
      
      c <- venex_filter() %>% filter(min > 0) %>% nrow()
      
      HTML(sprintf("<div> Cantidad de Jugadores <br> <strong> %s </strong>  </div>", c))
      
    })
    
    # Plots-----
      # Vin----
    output$vin_time <- renderPlot({
        
        data_eliminatorias() %>%
            plot_top(x90s, 'starts', spotlight = input$vin_select_player) +
            labs(title = 'Minutos Jugados dividido por 90', x = 'Minutos /90m', subtitle = 'El número representa cantidad de Titularidades')
        
    })
    
    output$vin_ga <- renderPlot({
      
      data_eliminatorias() %>%
        plot_top(g_a, spotlight = input$vin_select_player) + 
        ggrepel::geom_text_repel(aes(label = glue::glue('Goals: {gls}  Asist: {ast}')), nudge_y = .2, nudge_x = -.07, size = 5) +
        labs(title = 'Goles + Asistencias cada 90 minutos', x = 'Goles + Asistencias /90m')
      
    })
    
    output$vin_shots <- renderPlot({
        
        data_eliminatorias() %>%
            plot_top(sh, 'sh_90', .nudge_coef = .6, spotlight = input$vin_select_player) +
            labs(title = 'Remates', x = 'Remates', subtitle = 'El número representa Remates /90m')
        
    })
    
    output$vin_fls <- renderPlot({
        
        data_eliminatorias() %>%
            plot_top(fls, 'fls_90', .nudge_coef = .6, spotlight = input$vin_select_player) +
            labs(title = 'Faltas Cometidas', x = 'Cantidad de Faltas Cometidas', subtitle = 'El número representa Faltas cometidas /90m')
        
    })
    
    output$vin_fld <- renderPlot({
        
        data_eliminatorias() %>%
            plot_top(fld, 'fld_90', .nudge_coef = .8, spotlight = input$vin_select_player) +
            labs(title = 'Faltas Recibidas', x = 'Cantidad de Faltas Recibidas', subtitle = 'El número representa Faltas recibidas /90m')
        
    })
    
    output$vin_rec <- renderPlot({
        
        data_eliminatorias() %>%
            plot_top(rec, 'rec_90', .nudge_coef = .6, spotlight = input$vin_select_player) +
            labs(title = 'Recuperaciones', x = 'Cantidad de Recuperaciones', subtitle = 'El número representa Recuperaciones /90 minutos')
        
    })
      
      # Venex-----
    output$venex_time <- renderPlot({
        
        venex_filter() %>%
            plot_top(min, 'starts', spotlight = input$venex_select_player) +
            labs(title = 'Minutos Jugados dividido por 90', x = 'Minutos /90m', subtitle = 'El número representa cantidad de Titularidades')
        
    })
    
    output$venex_ga <- renderPlot({
        
        venex_filter() %>%
            plot_top(g_a, spotlight = input$venex_select_player) + 
            ggrepel::geom_text_repel(
              aes(label = glue::glue('Goles: {goal}  Asist: {asist}')), 
              nudge_y = .2, nudge_x = -.07, size = 5
            ) +
            labs(title = 'Total Goles + Asistencias', x = 'Goles + Asistencias')
        
    })
    
    output$venex_pos_age <- renderPlot({
      
      player <- input$venex_select_player
      if(is.null(player)){
        p <- ''
      }else{p <- player}
      
      destacado <- venex_filter() %>%
        filter(player == p) %>% 
        transmute(player, age, pos, highlight = T)
      
      venex_filter() %>%
        filter(country %in% input$venex_select_country) %>% 
        count(age, pos) %>% 
        left_join(destacado, by = c('age', 'pos')) %>%
        ggplot(aes(x=age, y=n, fill=pos, color=highlight)) +
        geom_col(size = 2) +
        theme(
          legend.position = 'top',
          legend.justification = 'left'
        ) +
        scale_x_continuous(breaks = scales::pretty_breaks()) +
        labs(x = 'Edad', y = 'Cantidad de Jugadores', title = 'Cantidad de Jugadores por edad y posicion') +
        scale_color_manual(values = c('white', 'blue'), labels = c(destacado$player, '')) +
        guides(
          fill = guide_legend(title = 'Posición:'),
          color = guide_legend(title = '', override.aes = list(size = 2, fill = NA))
        )
      
    })
    
    output$venex_map <- renderHighchart({
        
        aux_country <- tibble(
            country = c('CHI', 'BUL', 'CRO', 'PAR', 'GER', 'POR', 'RSA', 'KSA'),
            `iso-a3` = c('CHL', 'BGR', 'HR', 'PRY', 'DEU', 'PRT', 'ZAF', 'ARE')
        )
        
        country_count_players <- venex_filter() %>%
            left_join(aux_country, by = 'country') %>% 
            transmute(`iso-a3` = if_else(is.na(`iso-a3`), country, `iso-a3`)) %>% 
            count(`iso-a3`, name = 'value')
        
        mapdata <- JS("Highcharts.maps['custom/world']")
        
        highchart(type = "map") %>% 
            hc_add_series(
                mapData = mapdata, 
                data = country_count_players,
                joinBy = 'iso-a3',
                nullColor = '#791721',
                borderColor = '#791721'
            ) %>% 
            hc_colorAxis(minColor = .blue, maxColor = .yellow)
        
    })
    
      # Scout----
    next_7 <- reactive({
      next_7_days %>% 
        filter(comp_level %in% input$scout_comp_level)
    })
    
    last_7 <- reactive({
      last_7_days %>%
        filter(comp_level %in% input$scout_comp_level)
    })
    
    output$games_next_7_days <- renderTimevis({
        
        games_next_7days <- next_7() %>%
            mutate(
                Comp = str_remove(Comp, '\\d\\.\\s'),
                Country = if_else(is.na(Country), Comp, Country)
            ) %>% 
            transmute(
                start = Date,
                content = sprintf('%s vs %s', Player, Opponent),
                group = Country
            )
        
        games_groups <- games_next_7days %>% 
            distinct(group) %>% 
            transmute(id = group, content = group)
        
        timevis::timevis(
            data = games_next_7days,
            groups = games_groups,
            showZoom = F
        )
    })
    
    output$table_last_7_days <- DT::renderDataTable({
        last_7() %>% 
            group_by(Player) %>% 
            summarise(
                across(c(Min, Gls, Ast), ~sum(.x, na.rm = T)),
                GA = Gls+Ast,
                Opponents = paste0(Opponent, collapse = ', ')
            ) %>% 
            filter(GA != 0) %>% 
            arrange(desc(GA)) %>% 
            DT::datatable(
                rownames = F,
                options = list(
                    info = F,
                    searching = F,
                    paging = F,
                    columnDefs = list(list(className = 'dt-center', targets = '_all'))
                )
            )

    })
    
    output$barplot_last_7_days <- renderPlot({
      last_7() %>% 
        mutate(Resultado = word(Result)) %>% 
        count(Resultado) %>%
        filter(Resultado != '–') %>% 
        ggplot(aes(x = Resultado, y = n, label = n)) +
        geom_col() +
        geom_text(nudge_y = 1) +
        labs(y = 'Cantidad de Juegos')
    })
})
