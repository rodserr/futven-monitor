
fb_ven_wcq <- read_html('https://fbref.com/en/squads/df384984/2022/Venezuela-Stats')

wcq_ven <- list(
  standard = '//*[@id="stats_standard_3879"]',
  shooting = '//*[@id="stats_shooting_3879"]',
  misc = '//*[@id="stats_misc_3879"]'
) %>% 
  map(function(.x, page = fb_ven_wcq){
    page %>% 
      html_node(xpath = .x) %>% 
      html_table() %>% 
      janitor::row_to_names(1) %>% 
      janitor::clean_names() %>% 
      select(-ends_with('_2'), -matches)
      
  }) %>% 
  reduce(function(x, y){
    left_join(x, y, by = 'player', suffix = c('', '_2')) %>% 
      select(-ends_with('_2'))
  }) %>% 
  select(-c(p_katt, g_a_pk, g_sh, g_so_t, dist, p_kwon, p_kcon, og, x2crd_y)) %>% # g_a, so_t_percent, sh_90, so_t_90
  filter(!player %in% c('Squad Total', 'Opponent Total')) %>% 
  mutate(
    pos = str_sub(pos, start = 1L, end = 2L),
    age = str_replace(age, '-', '.'),
    across(-c('player', 'pos'), as.numeric),
    sh_90 = round(sh_90, 1),
    fls_90 = round(fls/x90s, 1),
    fld_90 = round(fld/x90s, 1),
    rec = tkl_w+int,
    rec_90 = round(rec/x90s, 1)
  )

wcq_ven %>% write_csv('futven/data/wcq_ven.csv')
