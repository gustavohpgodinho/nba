
require(tidyverse)

FOLDER_PROCESSED_DATA <- "D:/Mestrado/NBA/nba/data/processed/"

load(paste0(FOLDER_PROCESSED_DATA, "pbp.RData"))

pbp %>% 
  dplyr::filter(tp_event == 6) %>% 
  fix_fouls() %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(pcod, season, num_games) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::group_by(pcod) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::mutate(cod = stringr::str_remove(pcod, "p[0-9]{3}")) %>% 
  dplyr::mutate(n = 1000 * n / num_games) %>% 
  dplyr::select(cod, pcod, cnt, season, n) %>% 
  tidyr::spread(season, n) %>% 
  print(n = 100)

xdf %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(stringr::str_detect(cod, "_6\\(21")) %>% 
  dplyr::group_by(cods, season, num_games) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%  
  dplyr::group_by(cods) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::mutate(n = 1000 * n / num_games) %>% 
  dplyr::select(-num_games) %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt))


xdf <- pbp %>% 
  dplyr::filter(tp_event == 6) %>% 
  dplyr::distinct(season, game_id, period, clock) %>% 
  dplyr::inner_join(pbp, by = c('season', 'game_id', 'period', 'clock')) %>% 
  dplyr::filter(stringr::str_detect(cod, "_1\\(|_6|_3|_5\\(20|_5\\(19|_7\\(6")) %>% 
  fix_fouls() %>% 
  dplyr::group_by(season, game_id, period, clock) %>% 
  dplyr::mutate(pcods = paste0(pcod, collapse = '')) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(tp_event == 6) %>% 
  dplyr::select(season, game_id, period, clock, gram, pcod, cntfouls1, cntfouls2, pcods) %>% 
  dplyr::mutate(cod = stringr::str_remove_all(pcod, 'p[0-9]{3}')) %>% 
  dplyr::mutate(limit = ifelse(period <= 4 & cntfouls1 <= 4, "a", NA_character_), 
                limit = ifelse(period <= 4 & cntfouls2 <= 1, "a", limit), 
                limit = ifelse(period <= 4 & cntfouls1 > 4, "p", limit), 
                limit = ifelse(period <= 4 & cntfouls2 > 1, "p", limit), 
                limit = ifelse(period > 4 & cntfouls1 <= 3, "a", limit), 
                limit = ifelse(period > 4 & cntfouls2 <= 1, "a", limit), 
                limit = ifelse(period > 4 & cntfouls1 > 3, "p", limit), 
                limit = ifelse(period > 4 & cntfouls2 > 1, "p", limit)) %>% 
  dplyr::mutate(l2m = as.integer(stringr::str_detect(clock, "^(00\\:|01\\:|02\\:00\\:00$)"))) %>% 
  dplyr::mutate(cods = stringr::str_remove_all(pcods, "p[0-9]{3}"),
                cods = stringr::str_replace_all(cods, "(_3\\([0-9]{2})([cw])(\\))", "\\1\\3"),
                cods = stringr::str_replace_all(cods, "(_1\\()(.+?)(\\))", "\\10\\3")) %>% 
  dplyr::mutate(cnt_fouls = stringr::str_count(cods, "_6\\((10|11|12|14|15|16|21|22)"),
                cnt_fouls2 = stringr::str_count(cods, "_6\\((17|19|20)"),
                cnt_tech = stringr::str_count(cods, "_6\\(0"),
                cnt_off = stringr::str_count(cods, "_6\\(13"),
                cnt_dou = stringr::str_count(cods, "_6\\((4|18)"),
                cnt_ft1 = stringr::str_count(cods, "_3\\((11|12|13|22|23|33)"),
                cnt_ft2 = stringr::str_count(cods, "_3\\(40"),
                cnt_ft3 = stringr::str_count(cods, "_3\\((51|52|61)"),
                cnt_ft4 = stringr::str_count(cods, "_3\\((71|72)"),
                cnt_ft5 = stringr::str_count(cods, "_3\\((81|82|83)"),
                cnt_ms = stringr::str_count(cods, "_1\\(")) %>% 
  dplyr::mutate(s = paste0(limit, cnt_ms, cnt_fouls, cnt_fouls2, cnt_tech, cnt_off, 
                           cnt_dou, cnt_ft1, cnt_ft2, cnt_ft3, cnt_ft4, cnt_ft5, l2m)) %>% 
  dplyr::select(-c(cnt_ms, cnt_fouls, cnt_fouls2, cnt_tech, cnt_off, cnt_dou, 
                   cnt_ft1, cnt_ft2, cnt_ft3, cnt_ft4, cnt_ft5))

pbp %>% 
  dplyr::filter(tp_event == 1) %>% 
  dplyr::select(season, game_id, period, clock, cod) %>% 
  dplyr::left_join(pbp %>% 
                     dplyr::filter(stringr::str_detect(cod, "_3\\([1235678]")) %>% 
                     dplyr::distinct(season, game_id, period, clock) %>% 
                     dplyr::mutate(aux = 1), 
                   by = c('season', 'game_id', 'period', 'clock')) %>% 
  dplyr::mutate(aux = ifelse(is.na(aux), 0, aux)) %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cod, aux, season, num_games) %>%
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%  
  dplyr::group_by(cod, aux) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::mutate(n = 1000 * n / num_games) %>% 
  dplyr::select(-num_games) %>% 
  dplyr::group_by(cod, aux) %>% 
  dplyr::mutate(coefvar = 100 * sd(n)/mean(n),
                cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  print(n = 20)

pbp %>% 
  dplyr::filter(tp_event == 1) %>% 
  dplyr::select(season, game_id, period, clock, cod) %>% 
  dplyr::left_join(pbp %>% 
                     dplyr::filter(stringr::str_detect(cod, "_3\\([1235678]")) %>% 
                     dplyr::distinct(season, game_id, period, clock) %>% 
                     dplyr::mutate(aux = 1), 
                   by = c('season', 'game_id', 'period', 'clock')) %>% 
  dplyr::mutate(aux = ifelse(is.na(aux), 0, aux)) %>% 
  dplyr::group_by(cod, aux, season) %>%
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%  
  dplyr::group_by(cod, aux) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(n = 100 * n / sum(n)) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  print(n = 20)

# mostrar para o Pedro
xdf %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>%  
  dplyr::mutate(ind = stringr::str_detect(cods, "_3\\([1235678]"),
                ind = as.integer(ind)) %>% 
  dplyr::mutate(ind = ifelse(cod == "_6(13a)", 2, ind),
                ind = ifelse(stringr::str_detect(cod, "_6\\(0"), 3, ind)) %>% 
  dplyr::group_by(limit, ind, season, num_games) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::group_by(limit, ind) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(n = 100 * n / sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-num_games) %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  print(n = 20)

xdf %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>%  
  dplyr::mutate(ind = stringr::str_detect(cods, "_3\\([1235678]"),
                ind = as.integer(ind)) %>% 
  dplyr::mutate(ind = ifelse(cod == "_6(13a)", 2, ind),
                ind = ifelse(stringr::str_detect(cod, "_6\\(0"), 3, ind)) %>% 
  dplyr::group_by(season, num_games, limit, ind, cods) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::group_by(cods) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::mutate(n = 1000 * n / num_games) %>% 
  dplyr::select(-num_games) %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt)) %>%
  dplyr::group_split(limit, ind) %>% 
  plyr::l_ply(.data = ., .fun = print, n = 20)

xdf %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>%  
  dplyr::mutate(ind = stringr::str_detect(cods, "_3\\("),
                ind = as.integer(ind)) %>% 
  dplyr::mutate(ind = ifelse(cod == "_6(13a)", 2, ind),
                ind = ifelse(stringr::str_detect(cod, "_6\\(0"), 3, ind)) %>% 
  dplyr::group_by(season, num_games, limit, ind, cods) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::group_by(cods) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::mutate(n = 1000 * n / num_games) %>% 
  dplyr::select(-num_games) %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt)) %>%
  dplyr::group_split(limit, ind) %>% 
  plyr::l_ply(.data = ., .fun = print, n = 20)


xdf %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>%  
  dplyr::mutate(ind = stringr::str_detect(cods, "_3\\(")) %>% 
  dplyr::filter(limit == 'p', ind == 0) %>% 
  dplyr::group_by(limit, cods, season, num_games) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::group_by(limit, cods) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(n = 100 * n / sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-num_games) %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  print(n = 20)

xdf %>% 
  dplyr::group_by(s, pcod, cods) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::group_by(s) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::arrange(desc(cnt), s, pcod, desc(n)) %>% 
  dplyr::ungroup() %>% 
  print(n = 900)

xdf %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(limit, cods, season, num_games) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%  
  dplyr::group_by(limit, cods) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::mutate(n = 1000 * n / num_games) %>% 
  dplyr::select(-num_games) %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt)) %>%
  print(n = 20)

xdf %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(limit, cods, season, num_games) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%  
  dplyr::group_by(limit, cods) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(n = 100 * n / sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-num_games) %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  print(n = 20)

xdf %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(limit, cods, season, num_games) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%  
  dplyr::group_by(limit, cods) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(n = 100 * n / sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(limit, cods) %>% 
  dplyr::mutate(n = n - dplyr::lag(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-num_games) %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt))

xdf %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(limit, cods, season, num_games) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%  
  dplyr::group_by(limit, cods) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  dplyr::mutate(n = 1000 * n / num_games) %>% 
  dplyr::group_by(limit, cods) %>% 
  dplyr::mutate(n = n - dplyr::lag(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-num_games) %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  print(n = 20)




xdf <- pbp %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::group_by(season, num_games, tp_event) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::mutate(n = 1000 * n / num_games) %>% 
  dplyr::filter(!season %in% c('2008-09', '2009-10', '2010-11')) %>% 
  dplyr::select(-num_games) %>% 
  dplyr::group_by(tp_event) %>% 
  dplyr::mutate(coefvar = 100 * sd(n)/mean(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  tidyr::spread(season, n)


pbp %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::filter(tp_event == 6) %>% 
  dplyr::group_by(season, num_games, pcod) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::mutate(n = 1000 * n / num_games) %>% 
  dplyr::select(-num_games) %>% 
  dplyr::group_by(pcod) %>% 
  dplyr::mutate(coefvar = 100 * sd(n)/mean(n),
                cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = cnt) %>% 
  dplyr::mutate(cod = stringr::str_remove(pcod, "p[0-9]{3}")) %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  tidyr::spread(season, n) %>% 
  dplyr::group_by(cod) %>% 
  dplyr::mutate(cnt = sum(cnt)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(cnt), cod) %>% 
  dplyr::mutate(perc = 100 * cumsum(cnt) / sum(cnt),
                perc = round(perc, 2)) %>% 
  print(n = 100)


xdf <- pbp %>% 
  dplyr::filter(period <= 4) %>% 
  dplyr::filter(stringr::str_detect(pcod, "p500_3\\([1|2|3|4|5|6|7|8]|p501_5\\([19|20]|_7\\(6")) %>% 
  dplyr::distinct(game_id, period, clock) %>% 
  dplyr::mutate(ind = 1) %>% 
  dplyr::right_join(pbp, by = c('game_id', 'period', 'clock')) %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_games = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(tp_event == 6, period <= 4) %>% 
  dplyr::distinct(season, num_games, game_id, period, clock) %>% 
  dplyr::inner_join(pbp,  by = c('season', 'game_id', 'period', 'clock')) %>% 
  dplyr::filter(stringr::str_detect(cod, "_6|_3|_5\\(19|_5\\(20|_7\\(6")) %>% 
  dplyr::group_by(season, num_games, game_id, period, clock) %>% 
  dplyr::mutate(pcods = paste0(pcod, collapse = '')) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(tp_event == 6, period <= 4) %>% 
  dplyr::select(season, num_games, pcod, pcods, cntfouls1, cntfouls2) %>% 
  dplyr::mutate(aux = "a",
                aux = ifelse(cntfouls1 > 4, "pn", aux),
                aux = ifelse(cntfouls2 > 1, "pn", aux)) %>% 
  dplyr::mutate(cods = stringr::str_remove_all(pcods, "p[0-9]{3}")) %>% 
  dplyr::group_by(season, num_games, pcod, cods, aux) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::mutate(n = 1000 * n / num_games) %>% 
  dplyr::select(-num_games) %>% 
  dplyr::mutate(cod = stringr::str_remove(pcod, "p[0-9]{3}")) %>% 
  dplyr::group_by(cods) %>% 
  dplyr::mutate(cnt = sum(n)) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(cod) %>% 
  dplyr::mutate(cnt1 = sum(cnt)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", "s\\2")) %>% 
  tidyr::spread(season, n) %>% 
  dplyr::arrange(desc(cnt1), desc(cnt), cod, cods, aux) %>% 
  dplyr::group_split(cod)


xdf


pbp %>% 
  dplyr::filter(period <= 4) %>% 
  dplyr::filter(stringr::str_detect(pcod, "p500_3\\(1|p501_5\\(20|_7\\(6")) %>% 
  dplyr::distinct(game_id, period, clock) %>% 
  dplyr::mutate(ind = 1) %>% 
  dplyr::right_join(pbp, by = c('game_id', 'period', 'clock')) %>% 
  dplyr::filter(pcod == "p451_6(10p)", cntfouls1 == 2, cntfouls2 == 2, is.na(ind)) %>% 
  dplyr::distinct(game_id, period, clock) %>% 
  dplyr::filter(period <= 4) %>% 
  dplyr::inner_join(pbp, by = c('game_id', 'period', 'clock')) %>% 
  View()


pbp %>% 
  dplyr::filter(tp_event == 6) %>% 
  dplyr::group_by(season, pcod, cod, cntfouls1) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  
  
  xdf
