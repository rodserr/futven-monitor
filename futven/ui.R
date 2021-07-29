
# Define UI for application that draws a histogram
shinyUI(navbarPage(
    title = 'Futve',
    selected = 'Venex',
    theme = bs_theme(bg = "#591721", fg = "#fff", primary = .blue, font_scale = 1, bootswatch = "journal"),
    tags$head(tags$link(href = "style.css", rel = "stylesheet")),
    # Eliminatorias----
    tabPanel(
        'Eliminatorias',
        wellPanel(
            fluidRow(
                column(3, uiOutput('vin_select_pos_ui') ),
                column(3, uiOutput('vin_select_player_ui') ),
                column(3, wellPanel(htmlOutput('vin_age')) ),
                column(3, wellPanel(htmlOutput('vin_used_players')) )
            )
        ),
        br(),
        fluidRow(
            column(4, plotOutput('vin_time')),
            column(4, plotOutput('vin_ga')),
            column(4, plotOutput('vin_shots'))
        ),
        br(),
        fluidRow(
            column(4, plotOutput('vin_fls')),
            column(4, plotOutput('vin_fld')),
            column(4, plotOutput('vin_rec'))
        )
    ),
    # Venex----
    tabPanel(
        'Venex',
        wellPanel(
            fluidRow(
                column(2, uiOutput('venex_select_pos_ui') ),
                column(3, uiOutput('venex_select_country_ui') ),
                column(3, uiOutput('venex_select_player_ui') ),
                column(2, wellPanel(htmlOutput('venex_count')) ),
                column(2, wellPanel(htmlOutput('venex_age')) )
            )
        ),
        br(),
        fluidRow(
            tags$script(src = "https://code.highcharts.com/mapdata/custom/world.js"),
            column(6, highchartOutput('venex_map')),
            column(6, plotOutput('venex_time'))
        ),
        br(),
        fluidRow(
            column(6, plotOutput('venex_pos_age')),
            column(6, plotOutput('venex_ga'))
        )
    ),
    # Scout----
    tabPanel(
        'Scout',
        br(),
        fluidRow(
            column(
                3, 
                wellPanel(
                    checkboxGroupInput(
                        'scout_comp_level', 
                        label = 'Nivel de la competición',
                        inline = T,
                        choices = c('Primera División' = 1, 'Otras Divisiones' = 2),
                        selected = 1
                    )
                )
            )
        ),
        fluidRow(
            column(
                7,
                h4('Performance de los ultimos 7 dias'),
                DT::dataTableOutput('table_last_7_days')
            ),
            column(
                4,
                offset = 1,
                h4('Resultados de los ultimos 7 dias'),
                plotOutput('barplot_last_7_days')
            )
        ),
        fluidRow(
            column(
                12,
                h4('Calendario de juegos de los proximos 7 dias'),
                timevisOutput('games_next_7_days')
            )
        )
    ),
    # About----
    tabPanel(
        'Info',
        fluidRow(
            column(6, wellPanel(htmltools::includeMarkdown('md/info.md'))),
            column(6, wellPanel(htmltools::includeMarkdown('md/resources.md')))
        )
    )
))
