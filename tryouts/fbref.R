# Libraries----
library(tidyverse)
library(rvest)
library(bigrquery)
library(worldfootballR)
source('futven/helpers.R')

# Get list of players in playing in foreign countries
players_venex <- get_venex()

# Scout
scout_report <- get_scoutting()

# Last updated
last_updated <- '2021-07-12'

players_last_7_days <- scout_report$last_7_days %>% 
  filter(!is.na(Min), Date >= last_updated) %>% 
  pull(Player)

# Get standard stats
venex <- players_venex %>%
  filter(name %in% players_last_7_days) %>% 
  mutate( stats = map(href, possibly(venex_scraper, NA)) )

# 2nd try to scrape 
venex_failed <- venex %>% 
  filter(is.na(stats)) %>%
  mutate( stats = map(href, possibly(venex_scraper_2, NA)) )

# Merge trys
venex_all <- bind_rows(venex, venex_failed) %>% 
  unnest(stats) %>% 
  filter(!Season %in% c('', NA)) %>% 
  rename(player = name, pos = position)

unique(venex_all$Season)

# Clean venex
venex_clean <- venex_all %>% 
  filter(Season %in% c('2021', '2020-2021')) %>%
  group_by(player) %>% 
  summarise(
    .groups = 'drop',
    age = max(Age),
    pos = last(pos),
    country = last(Country),
    team = last(Squad),
    match_played = sum(MP_Time, na.rm = T),
    starts = sum(Starts_Time, na.rm = T),
    minutes_played = sum(Min_Time, na.rm = T),
    goal = sum(Gls, na.rm = T),
    asist = sum(Ast, na.rm = T)
  ) %>% 
  mutate(
    pos = str_trim(pos) %>% str_sub(start = 1L, end = 2L),
    min = minutes_played/90,
    g_a = (goal+asist)
  )

# Read current datasets
venex_all_old <- read_csv('data_backup/venex_all.csv')
venex_clean_old <- read_csv('futven/data/venex_2021.csv')

# Update
venex_all_new <- update_csv(venex_all, venex_all_old)
venex_clean_new <- update_csv(venex_clean, venex_clean_old)

# venex_all_new %>% write_csv('data_backup/venex_all.csv')
# venex_clean_new %>% write_csv('futven/data/venex_2021.csv')


# Bigquery----
bq_auth(path = "futven/.secret/futven-0c9a31d25e8d.json")

bq_update_before('venex', venex_clean_new)
bq_table('futven', 'production', 'venex') %>%
  bq_table_upload(venex_clean_new, create_disposition = 'CREATE_IF_NEEDED', write_disposition = 'WRITE_APPEND')

# Scout----
scout_report$next_7_days %>% write_csv('futven/data/next_7_days.csv')
scout_report$last_7_days %>% write_csv('futven/data/last_7_days.csv')
