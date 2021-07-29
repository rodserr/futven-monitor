# Scraper
extract_last_team <- function(.str, remove_ven = F){
  
  # .str: character string of teams separate by comma
  # remove_ven: remove 'Venezuela, ' from .str
  
  if(remove_ven){
    .str <- .str %>% str_remove('Venezuela, ')
  }
  
  str_extract(.str, '[^,]*') %>% str_trim()
  
}

venex_scraper <- function(.x, stype = 'standard'){
  Sys.sleep(1)
  paste0('https://fbref.com', .x) %>% worldfootballR::fb_player_season_stats(stype) 
}

clean_country <- function(country){
  str_extract(country, '(?<=\\s).*')
}

venex_scraper_2 <- function(.href){
  
  # 2nd scraper in case venex_scraper fails
  
  tryCatch({
    venex_scraper(.href)
  }, error = function(e){
    paste0('https://fbref.com', .href) %>% 
      read_html() %>% 
      html_node(xpath = '//*[@id="stats_standard_dom_lg"]') %>% 
      html_table() %>% 
      janitor::row_to_names(1) %>% 
      janitor::clean_names(case = 'upper_camel') %>% 
      filter(!str_detect(Season, '\\(|Season'), !Season == '') %>% 
      mutate(Country = clean_country(Country)) %>% 
      rename(
        c(
          'MP_Time'='Mp', 'Starts_Time'='Starts', 'Min_Time'='Min', 'Mins_Per_90_Time'='X90S',
          'G_minus_PK'='GPk', 'PK'='Pk', 'Gls_Per_Minutes'='Gls_2', 'Ast_Per_Minutes'='Ast_2',
          'G+A_Per_Minutes'='GA', 'G_minus_PK_Per_Minutes'='GPk_2', 'G+A_minus_PK_Per_Minutes'='GAPk'
        )
      ) %>% 
      mutate(across(-c('Season', 'Squad', 'Country', 'Comp', 'LgRank'), ~str_remove(.x, '\\,') %>% as.numeric()))
  })
  
}

get_venex <- function(){
  
  fb_ven <- read_html('https://fbref.com/en/country/players/VEN/Venezuela-Football-Players')
  
  player_tag <- fb_ven %>% 
    html_elements('p') %>% 
    html_node('a') 
  
  players_ven <- tibble(
    name = player_tag %>% html_text2(),
    href = player_tag %>% html_attr('href'),
    active = player_tag %>% html_node('strong') %>% html_text2(),
    metadata = fb_ven %>% html_elements('p') %>% html_text2()
  ) %>%
    mutate(active = if_else(is.na(active), F, T)) %>%
    filter(str_detect(href, '/en/players/')) %>% 
    separate(metadata, into = c(NA, 'position', 'teams'), sep = '·', fill = 'right') %>% 
    mutate(last_team = extract_last_team(teams)) 
  
  # Teams form Venezuela League
  teams_ven <- c(
    'Deportivo Táchira', 'Monagas', 'Zulia', 'Deportivo Lara', 'Metropolitanos', 'Gran Valencia Maracay',
    'Estudiantes de Mérida', 'Aragua', 'Universidad Central de Venezuela FC', 'Deportivo La Guaira', 'Deportivo Anzoátegui',
    'Portuguesa', 'Trujillanos', 'Caracas', 'LALA', 'Academia Puerto Cabello', 'Bolívar SC', 'Mineros', 'Yaracuyanos FC',
    'CD Hermanos Colmenarez', 'Carabobo', 'Atlético Venezuela', 'Zamora', 'Estudiantes de Caracas', 'Llaneros', 'Guaireña FC'
  )
  
  # Some men players have 'Venezuela' as last team
  men_bugs <- c('Ronald Hernández', 'José Manuel Velázquez', 'Wuilker Faríñez', 'Alexander González')
  
  # Remove Women players
  women_players <- players_ven %>% 
    filter(last_team == 'Venezuela', !name %in% men_bugs) %>% 
    pull(name)
  
  players_ven %>% 
    filter(
      active, 
      !is.na(last_team),
      !last_team %in% teams_ven,
      !name %in% women_players
    ) %>% 
    mutate(
      last_team = if_else(last_team == 'Venezuela', extract_last_team(teams, remove_ven = T), last_team)
    )
  
}

get_scoutting <- function(){
  fb_scout <- read_html('https://fbref.com/en/stathead/scout/m/VEN')
  
  last_7_days <- fb_scout %>% 
    html_node(xpath = '//*[@id="stats_players"]') %>% 
    html_table() %>% 
    janitor::row_to_names(1) %>% 
    janitor::clean_names(case = 'upper_camel') %>% 
    transmute(
      Player,
      Date = str_extract(Date, '.*(?=\\,)') %>% lubridate::ymd(),
      Country = clean_country(Country),
      comp_level = word(Comp) %>% str_remove('\\.'),
      Comp, Result, Squad, Opponent,
      across(all_of(c('Min', 'Gls', 'Ast')), possibly(as.numeric, NA))
    )
  
  # https://stackoverflow.com/questions/57022738/r-web-scraping-packages-failing-to-read-in-all-tables-of-url
  next_7_days <- fb_scout %>% 
    html_nodes(xpath = '//comment()') %>%   
    html_text() %>%  
    paste(collapse = '') %>%   
    read_html() %>% 
    html_node('table') %>%   
    html_table() %>% 
    janitor::clean_names(case = 'upper_camel') %>% 
    filter(!str_detect(Squad, 've ')) %>% 
    transmute(
      Player,
      Date = str_extract(Date, '.*(?=\\,)') %>% lubridate::ymd(),
      Time,
      comp_level = word(Comp) %>% str_remove('\\.'),
      Country = clean_country(Country),
      Comp, Squad, Opponent
    )
  
  list(
    last_7_days = last_7_days,
    next_7_days = next_7_days
  )
}

# BQ
bq_update_before <- function(table_name, datatable, schema = "futven", dataset = "production"){
  
  exist <- bq_table(schema, dataset, table_name) %>% bq_table_exists() 
  
  if(exist){
    # Delete current tables
    bq_table(schema, dataset, table_name) %>% 
      bq_table_delete() 
  }
  
  # Create new empty tables
  bq_table(schema, dataset, table_name) %>% 
    bq_table_create(fields = as_bq_fields(datatable))
}

bq_writer <- function(table_name, datatable, schema = "futven", dataset = "production"){
  tryCatch({
    
    Sys.sleep(10)
    
    table_path <- bq_table(schema, dataset, table_name)
    
    table_path %>%
      bq_table_upload(datatable, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
  },
  error = function(e){
    error_string <- sprintf('write %s to BigQuery tables', dataset)
    send_error_to_slack(error_string, e)
  })
}

# Prod
update_csv <- function(.updated, .deprecated){
  players_upd <- unique(.updated$player)
  
  .deprecated %>% 
    filter(!player %in% players_upd) %>% 
    bind_rows(.updated)
  
}

plot_top <- function(df, .variable_y, .variable_num = NULL, .nudge_coef = .4, spotlight = NULL){
  
  if(is.null(spotlight)){
    spotlight <- 'NA'
  }
  
  sp <- df %>% filter(player == spotlight)
  
  p <- df %>% 
    # filter({{.variable_y}} != 0) %>%
    slice_max({{.variable_y}}, n = 10, with_ties = F) %>% 
    add_row(sp) %>% 
    filter(!is.na(min)) %>% 
    distinct(.keep_all = T) %>% 
    ggplot(
      aes(x = {{.variable_y}}, y = reorder(player, {{.variable_y}} ), color = player == spotlight)
    ) +
    geom_point() +
    geom_segment(aes(x=0, y=player, xend={{.variable_y}}, yend=player)) +
    labs(y = NULL) +
    theme(
      legend.position = 'none',
      plot.subtitle = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks())
  
  
  if(!is.null(.variable_num)){
    # p <- p + ggrepel::geom_text_repel(aes_string(label = .variable_num), nudge_x = .nudge_coef, size = 6)
    p <- p + ggrepel::geom_text_repel(aes(label = get(.variable_num)), nudge_x = .nudge_coef, size = 6)
  }
  
  if( !is_empty(sp$min) && is.na(sp$min) ){
    p <- p + labs(caption = sprintf("%s no ha debutado en esta competición", spotlight))
  }
  
  p
  
}

# Dev
country_diff <- function(.df){
  mapdata <- highcharter::get_data_from_map(highcharter::download_map_data("custom/world"))
  
  is_in_mapdata <- mapdata %>% 
    left_join(.df %>% distinct(country) %>% mutate(gotit = T), by = c('iso-a3' = 'country')) %>% 
    filter(gotit) %>% 
    select(name, `iso-a3`)
  
  .df %>% 
    distinct(country) %>% 
    filter(!country %in% is_in_mapdata$`iso-a3`)
  
}