# Library----
suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(shiny)
  library(shinyWidgets)
  library(htmltools)
  library(thematic)
  library(bslib)
  library(ggrepel)
  library(glue)
  library(scales)
  library(highcharter)
  library(bigrquery)
  library(timevis)
  library(lubridate)
  library(DT)
  library(worldfootballR)
  source('helpers.R')
})

# Theme----
.blue <- '#12a5c7'
.yellow <- '#e88f04'

ggplot2::theme_set(ggplot2::theme_minimal() + theme(text=element_text(size=15)))
thematic_shiny(font = "auto", qualitative = c(.blue, .yellow, '#991721', '#808080'), sequential = c(.blue, .yellow))

# Read----
dev <- F

# World Cup Qualifier Data
wcq_ven <- read_csv('data/wcq_ven.csv')

# Venex
if(dev){
  venex <- read_csv('data/venex_2021.csv')
  next_7_days <- read_csv('data/next_7_days.csv')
  last_7_days <- read_csv('data/last_7_days.csv')
  
} else{
  bq_auth(path = ".secret/futven-0c9a31d25e8d.json")
  venex <- bq_table('futven', 'production', 'venex') %>% bq_table_download()
  
  scout_report <- get_scoutting()
  next_7_days <- scout_report$next_7_days
  last_7_days <- scout_report$last_7_days
}

