require(tidyverse)

extract_pbp <- function(path){
  # pbp

  readr::read_file(path) %>%
    stringr::str_extract(string = ., pattern = "sbpg:playbyplay\\,pbp\\:\\{playGrps:\\[\\[(.+?)\\]\\]\\,tms") %>% 
    stringr::str_remove_all(string = ., pattern = "sbpg\\:playbyplay\\,pbp\\:\\{playGrps\\:\\[\\[") %>% 
    stringr::str_remove_all(string = ., pattern = "\\]\\]\\,tms") %>% 
    stringr::str_replace_all(string = ., pattern = "(\\})(\\],\\[)(\\{)", "\\1;\\3") %>% 
    stringr::str_replace_all(string = ., pattern = "(\\})(\\,)(\\{)", "\\1;\\3") %>% 
    stringr::str_replace_all(string = ., 
                             pattern = "(period)(\\:\\{)(number\\:[0-9]{1}\\,)(displayValue)(\\:(.+?))(\\})", 
                             replacement = "\\1_\\3desc_period\\5") %>% 
    stringr::str_replace_all(string = ., 
                             pattern = "(clock)(\\:)(\\{displayValue)(\\:(.+?))(\\})", 
                             replacement = "\\1\\4") %>% 
    stringr::str_remove_all(string = ., pattern = "\\{|\\}") %>% 
    stringr::str_split(string = ., pattern = ";") %>% 
    unlist() %>% 
    data.frame(doc = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>%
    dplyr::mutate(seq_id = dplyr::row_number()) %>% 
    dplyr::mutate(playid = stringr::str_extract(doc, "^id\\:[0-9]+")) %>%
    dplyr::mutate(doc = stringr::str_replace_all(doc, "\\,\\s+", " ")) %>% 
    dplyr::mutate(cols = stringr::str_split(doc, pattern = "\\,")) %>% 
    tidyr::unnest(cols) %>% 
    dplyr::select(-doc) %>% 
    tidyr::separate(data = ., col = "cols", into = c("key", "value"), sep = "\\:", extra = "merge", fill = "right") %>% 
    dplyr::distinct() %>% 
    tidyr::spread(key, value) %>% 
    dplyr::select(-playid) %>% 
    dplyr::mutate(gameid = stringr::str_replace(string = path, pattern = '(.+?)([0-9]+)(\\.txt$)', repl = "\\2")) 
  
  #%>% 
  #  dplyr::select(gameid, id, period_number, desc_period, clock, text, homeAway, 
  #                homeScore, awayScore, scoringPlay)
}

extract_shots_details <- function(path){
  
  # pbp shots
  readr::read_file(path) %>% 
    stringr::str_extract(string = ., pattern = "shtChrt\\:\\{plays\\:\\[(.+?)\\]\\,tms") %>% 
    stringr::str_remove_all(string = ., pattern = "shtChrt\\:\\{plays:\\[") %>% 
    stringr::str_remove_all(string = ., pattern = "\\]\\,tms") %>% 
    stringr::str_replace_all(string = ., pattern = "(\\})(\\,)(\\{)", "\\1;\\3") %>% 
    stringr::str_replace_all(string = ., 
                             pattern = "(period)(\\:\\{)(number\\:[0-9]{1}\\,)(displayValue)(\\:(.+?))(\\})", 
                             replacement = "\\1_\\3desc_period\\5") %>% 
    stringr::str_replace_all(string = ., 
                             pattern = "(athlete)(\\:\\{)(id\\:[0-9]+\\,)(name)(\\:(.+?))(\\})", 
                             replacement = "\\1_\\3\\4\\5") %>% 
    stringr::str_replace_all(string = ., 
                             pattern = "(coordinate)(\\:\\{)(x\\:.?[0-9]+\\,)(y\\:.?[0-9]+)(\\})", 
                             replacement = "\\1\\3\\4") %>% 
    stringr::str_replace_all(string = ., 
                             pattern = "(type)(\\:\\{)(id\\:[0-9]+\\,)(txt)(\\:(.+?))(\\})", 
                             replacement = "\\1_\\3\\4_shot\\5") %>% 
    stringr::str_remove_all(string = ., pattern = "\\{|\\}") %>% 
    stringr::str_split(string = ., pattern = ";") %>% 
    unlist() %>% 
    data.frame(doc = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>%
    dplyr::mutate(doc = stringr::str_replace_all(doc, "\\,\\s+", " ")) %>% 
    dplyr::mutate(playid = stringr::str_extract(doc, "^id\\:[0-9]+")) %>% 
    dplyr::mutate(cols = stringr::str_split(doc, pattern = "\\,")) %>% 
    tidyr::unnest(cols) %>% 
    dplyr::select(-doc) %>% 
    tidyr::separate(data = ., col = "cols", into = c("key", "value"), sep = "\\:", extra = "merge", fill = "right") %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(value = ifelse(is.na(value), "1", value)) %>% 
    tidyr::spread(key, value) %>% 
    dplyr::select(-playid) %>% 
    dplyr::mutate(gameid = stringr::str_replace(string = path, pattern = '(.+?)([0-9]+)(\\.txt$)', repl = "\\2"))
  
  #%>% 
  #  dplyr::select(gameid, id, period_number, desc_period, homeAway, type_id, 
  #                txt_shot, coordinatex, y, shootingPlay, scoringPlay, isFreeThrow,
  #                athlete_id, name)
}

extract_teams_details <- function(path){
  
  # teams
  xdf <- readr::read_file(path) %>% 
    stringr::str_extract(string = ., pattern = "shtChrt\\:\\{plays\\:\\[(.+?)\\]\\,tms\\:\\{home\\:\\{(.+?)\\}\\,away\\:(.+?)\\}\\}\\,ntrlSte") %>% 
    stringr::str_extract(string = ., pattern = "\\,tms\\:\\{home\\:\\{(.+?)\\}\\,away\\:(.+?)\\}\\}\\,ntrlSte") %>% 
    stringr::str_remove(string = ., pattern = "\\,tms\\:") %>% 
    stringr::str_remove(string = ., pattern = "\\,ntrlSte") %>% 
    stringr::str_remove_all(string = ., pattern = "^\\{|\\}$") %>% 
    stringr::str_replace_all(string = ., pattern = "(\\})(\\,)(away\\:)", "\\1;\\3") %>% 
    stringr::str_replace_all(string = ., pattern = "(home|away)(\\:\\{)(id)", "\\1_\\3") %>% 
    stringr::str_remove_all(string = ., pattern = "records\\:\\[(.+?)\\]\\,") %>% 
    stringr::str_replace_all(string = ., pattern = "\\}\\,\\{", "}_{") %>% 
    stringr::str_split(string = ., pattern = ";") %>% 
    unlist() %>% 
    data.frame(doc = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(doc = stringr::str_remove_all(doc, "\\}$")) %>% 
    dplyr::mutate(cols = stringr::str_split(doc, pattern = "\\,")) %>% 
    tidyr::unnest(cols) %>% 
    tidyr::separate(data = ., col = "cols", into = c("key", "value"), sep = "\\:", extra = "merge", fill = "right") %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(value = ifelse(key == 'linescores', stringr::str_remove_all(value, "\\[\\{|\\}|displayValue\\:|\\]"), value),
                  value = ifelse(key == 'linescores', stringr::str_replace_all(value, "_\\{", "\\,"), value)) %>% 
    dplyr::filter(key %in% c('home_id', 'away_id', 'abbrev', 'displayName', 'shortDisplayName', 
                             'location', 'isHome', 'linescores', 'score', 'winner')) %>% 
    tidyr::spread(key, value) %>% 
    dplyr::mutate(gameid = stringr::str_replace(string = path, pattern = '(.+?)([0-9]+)(\\.txt$)', repl = "\\2")) %>% 
    dplyr::select(-doc)
  
  if(nrow(xdf) == 0){
    
    xdf <- readr::read_file(path) %>% 
      stringr::str_extract(string = ., pattern = "sbpg:playbyplay\\,pbp\\:\\{playGrps:\\[\\[(.+?)\\]\\]\\,tms(.+?)\\}\\}\\,") %>% 
      stringr::str_extract(string = ., pattern = "tms(.+?)\\}\\}\\,") %>% 
      stringr::str_remove(string = ., pattern = "^tms\\:") %>% 
      stringr::str_remove(string = ., pattern = "\\}\\,$") %>% 
      stringr::str_remove_all(string = ., pattern = "^\\{|\\}$") %>% 
      stringr::str_replace_all(string = ., pattern = "(\\})(\\,)(away\\:)", "\\1;\\3") %>% 
      stringr::str_replace_all(string = ., pattern = "(home|away)(\\:\\{)(id)", "\\1_\\3") %>% 
      stringr::str_remove_all(string = ., pattern = "records\\:\\[(.+?)\\]\\,") %>% 
      stringr::str_replace_all(string = ., pattern = "\\}\\,\\{", "}_{") %>% 
      stringr::str_split(string = ., pattern = ";") %>% 
      unlist() %>% 
      data.frame(doc = ., stringsAsFactors = FALSE) %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(doc = stringr::str_remove_all(doc, "\\}$")) %>% 
      dplyr::mutate(cols = stringr::str_split(doc, pattern = "\\,")) %>% 
      tidyr::unnest(cols) %>% 
      tidyr::separate(data = ., col = "cols", into = c("key", "value"), sep = "\\:", extra = "merge", fill = "right") %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(value = ifelse(key == 'linescores', stringr::str_remove_all(value, "\\[\\{|\\}|displayValue\\:|\\]"), value),
                    value = ifelse(key == 'linescores', stringr::str_replace_all(value, "_\\{", "\\,"), value)) %>% 
      dplyr::filter(key %in% c('home_id', 'away_id', 'abbrev', 'displayName', 'shortDisplayName', 
                               'location', 'isHome', 'linescores', 'score', 'winner')) %>% 
      tidyr::spread(key, value) %>% 
      dplyr::mutate(gameid = stringr::str_replace(string = path, pattern = '(.+?)([0-9]+)(\\.txt$)', repl = "\\2")) %>% 
      dplyr::select(-doc)
  }

  return(xdf)
  #%>% 
  # dplyr::select(gameid, home_id, away_id, abbrev, displayName, shortDisplayName, 
  #                isHome, linescores, location, score, winner)
  
}

extract_game_info <- function(path){
  
  desc_game <- readr::read_file(path) %>%
    stringr::str_extract(string = ., pattern = "(\\<div class\\=ScoreCell__GameNote di\\>)(.+?)(\\<.div\\>)") %>% 
    stringr::str_replace(string = ., pattern = "(\\<div class\\=ScoreCell__GameNote di\\>)(.+?)(\\<.div\\>)", replacement = "\\2")
  
    
  # game info
  readr::read_file(path) %>% 
    stringr::str_extract(string = ., pattern = "gmInfo\\:\\{(.+?)\\}\\,sbpg") %>%
    stringr::str_replace(string = ., pattern = "(gmInfo\\:)(\\{(.+?)\\})(\\,sbpg)", "\\2") %>% 
    stringr::str_remove_all(string = ., pattern = "\\{|\\}") %>% 
    stringr::str_remove_all(string = ., pattern = "refs\\:\\[|\\]|\\[") %>% 
    stringr::str_replace_all(string = ., pattern = '\\,', ";") %>% 
    stringr::str_split(string = ., pattern = ';') %>% 
    unlist() %>% 
    data.frame(doc = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    tidyr::separate(data = ., col = "doc", into = c("key", "value"), sep = "\\:", extra = "merge", fill = "right") %>% 
    dplyr::group_by(key) %>% 
    dplyr::mutate(value = paste0(value, collapse = ", ")) %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct() %>% 
    dplyr::filter(key %in% c('attnd', 'cvrg', 'dtTm', 'dspNm', 'cpcty', 
                             'loc', 'locAddr', 'state')) %>% 
    tidyr::spread(key, value) %>% 
    dplyr::mutate(gameid = stringr::str_replace(string = path, pattern = '(.+?)([0-9]+)(\\.txt$)', repl = "\\2")) %>% 
    dplyr::mutate(desc_game = desc_game)
  
  #%>% 
  #  dplyr::select(gameid, dtTm, loc, locAddr, state, attnd, cpcty, cvrg, dspNm)
  
}


load("D:/Mestrado/NBA/jogos_nba.RData")


df_teams_details <- dir(path = "D:/Mestrado/NBA/data/dados_crawler/paginas_html_espn", full.names = TRUE) %>% 
  plyr::ldply(.data = ., .fun = extract_teams_details, .progress = 'time') %>% 
  dplyr::as_tibble()

save(df_teams_details, file = "D:/Mestrado/NBA/data/dados_crawler/espn/df_teams_details.RData")

jogos_espn <- df_teams_details %>% 
  dplyr::group_by(gameid) %>% 
  dplyr::summarise(home_team = unique(displayName)[2],
                   home_score = score[2],
                   away_score = score[1],
                   away_team = unique(displayName)[1],
                   home_id = unique(na.omit(home_id)),
                   away_id = unique(na.omit(away_id)),
                   home_abbr = abbrev[2],
                   away_abbr = abbrev[1],
                   home_linescore = linescores[2],
                   away_linescore = linescores[1])

save(jogos_espn, file = "D:/Mestrado/NBA/data/dados_crawler/espn/jogos_espn.RData")

df_game_details <- dir(path = "D:/Mestrado/NBA/data/dados_crawler/paginas_html_espn", full.names = TRUE) %>% 
  plyr::ldply(.data = ., .fun = extract_game_info, .progress = 'time') %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(gameid, desc_game, dtTm, dspNm, loc, locAddr, state, attnd, cpcty, cvrg) %>% 
  dplyr::mutate(cvrg = ifelse(cvrg == "", NA_character_, cvrg))

save(df_game_details, file = "D:/Mestrado/NBA/data/dados_crawler/espn/df_game_details.RData")

xdf_espn <- jogos_espn %>% 
  dplyr::left_join(df_game_details %>% dplyr::select(gameid, desc_game, dtTm), by = 'gameid') %>% 
  dplyr::mutate(season_type = ifelse(stringr::str_detect(desc_game, "Preseason"), "Pre Season", NA_character_),
                season_type = ifelse(is.na(season_type) & stringr::str_detect(desc_game, "EAST 1ST ROUND|EAST 1st ROUND|East 1st Round|EAST FINALS|East Finals|East Semifinals|EAST SEMIFINALS|EASTERN CONFERENCE|NBA Finals|NBA FINALS|NBA Play-In|West 1st Round|WEST 1ST ROUND|WEST 1st ROUND|West Finals|WEST FINALS|WEST PLAY-IN|WEST SEMIFINALS|West Semifinals|WESTERN CONFERENCE"), "Playoffs", season_type),
                season_type = ifelse(is.na(season_type) & stringr::str_detect(desc_game, "ALL\\-STAR GAME|All\\-Star"), "AllStarGame", season_type),
                season_type = ifelse(is.na(season_type) & stringr::str_detect(desc_game, "RISING STARS"), "RisingStars", season_type),
                season_type = ifelse(is.na(season_type) & stringr::str_detect(desc_game, "AT LONDON ENGLAND|AT LONDON|NBA MEXICO CITY GAMES|NBA LONDON GAME|NBA PARIS GAME|NBA Mexico City Game 2022|NBA Paris Game 2023"), "Regular Season", season_type),
                season_type = ifelse(is.na(desc_game), "Regular Season", season_type),
                season_type = ifelse(is.na(season_type), "Regular Season", season_type)) %>% 
  dplyr::mutate(dtTm = stringr::str_replace(dtTm, "(.+)(T)(.+)", "\\1")) %>%
  dplyr::mutate(dtTm = lubridate::ymd(dtTm)) %>% 
  dplyr::filter(season_type %in% c('Regular Season', 'Playoffs')) %>%
  dplyr::select(gameid, dtTm, season_type, home_team, away_team, home_score, away_score) %>% 
  setNames(nm = c('espn_id', 'date', 'season_type', 'home_team', 'away_team', 'home_pts', 'away_pts')) %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate_at(.vars = c('home_team', 'away_team'), 
                   .funs = function(x){ifelse(x == "LA Clippers", "Los Angeles Clippers", x)}) %>% 
  dplyr::mutate_at(.vars = c('home_pts', 'away_pts'), .funs = as.integer) %>%
  dplyr::filter(home_team %in% equipes_nba, away_team %in% equipes_nba)


df_files <- dir(path = "D:/Mestrado/NBA/data/dados_crawler/paginas_html_espn", full.names = TRUE) %>% 
  data.frame(path = ., stringsAsFactors = FALSE) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(espn_id = stringr::str_extract(path, "[0-9]+"))



df_match_codigos_partidas %>%
  dplyr::filter(!is.na(espn_id)) %>% 
  dplyr::left_join(xdf_espn %>% dplyr::select(espn_id, date), by = 'espn_id') %>% 
  dplyr::left_join(jogos_nba, by = c('nba_id' = 'GAME_ID')) %>% 
  dplyr::select(espn_id, nba_id, date, SEASON) %>% 
  dplyr::mutate(aux = as.character(date)) %>% 
  dplyr::mutate(aux = stringr::str_sub(aux, end = 7)) %>% 
  dplyr::mutate(SEASON = ifelse(date > '2023-05-01', '2022-23', SEASON)) %>% 
  dplyr::group_by(aux) %>% 
  dplyr::mutate(SEASON = unique(na.omit(SEASON))) %>% 
  dplyr::ungroup() %>%
  dplyr::select(-aux) %>% 
  dplyr::left_join(df_files, by = "espn_id") %>% 
  dplyr::group_split(SEASON) %>% 
  plyr::l_ply(.data = ., .fun = function(df){
    
    future::plan(future::multisession(), workers = 8)
    
    xdf <- furrr::future_map(.x = df$path, .f = extract_pbp, .progress = TRUE) %>% 
      dplyr::bind_rows() %>% 
      dplyr::as_tibble()
      
    future::plan(future::sequential())
    
    nome_arquivo <- paste0("D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_", stringr::str_remove_all(df$SEASON[1], "\\D"), ".RData")
    
    save(xdf, file = nome_arquivo)

    
  }, .progress = 'time')

future::plan(future::sequential())

xx <- load("D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_202223.RData")

pbpespn_202223 <- get(xx) %>% 
  dplyr::select(gameid, id, seq_id, period_number, desc_period, clock, 
                text, homeAway, scoringPlay, homeScore, awayScore, isOT, otNum, isFoul)

save(pbpespn_202223, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_202223.RData")



df_pbpespn <- "D:/Mestrado/NBA/data/dados_crawler/espn/" %>% 
  dir(full.names = TRUE) %>% 
  purrr::keep(.x = ., .p = stringr::str_detect, pattern = 'pbpespn') %>% 
  plyr::ldply(.data = ., .fun = function(path){
    
    xx <- load(path)
    xx <- get(xx)
    return(xx)
    
  }, .progress = 'time') %>% 
  dplyr::as_tibble()


save(df_pbpespn, file = "D:/Mestrado/NBA/data/dados_crawler/espn/df_pbpespn.RData")



df_match_codigos_partidas %>%
  dplyr::filter(!is.na(espn_id)) %>% 
  dplyr::left_join(xdf_espn %>% dplyr::select(espn_id, date), by = 'espn_id') %>% 
  dplyr::left_join(jogos_nba, by = c('nba_id' = 'GAME_ID')) %>% 
  dplyr::select(espn_id, nba_id, date, SEASON) %>% 
  dplyr::mutate(aux = as.character(date)) %>% 
  dplyr::mutate(aux = stringr::str_sub(aux, end = 7)) %>% 
  dplyr::mutate(SEASON = ifelse(date > '2023-05-01', '2022-23', SEASON)) %>% 
  dplyr::group_by(aux) %>% 
  dplyr::mutate(SEASON = unique(na.omit(SEASON))) %>% 
  dplyr::ungroup() %>%
  dplyr::select(-aux) %>% 
  dplyr::left_join(df_files, by = "espn_id") %>% 
  dplyr::group_split(SEASON) %>% 
  plyr::l_ply(.data = ., .fun = function(df){
    
    future::plan(future::multisession(), workers = 8)
    
    xdf <- furrr::future_map(.x = df$path, .f = extract_shots_details, .progress = TRUE) %>% 
      dplyr::bind_rows() %>% 
      dplyr::as_tibble()
    
    future::plan(future::sequential())
    
    nome_arquivo <- paste0("D:/Mestrado/NBA/data/dados_crawler/espn/shotsespn_", stringr::str_remove_all(df$SEASON[1], "\\D"), ".RData")
    
    save(xdf, file = nome_arquivo)
    
    
  }, .progress = 'time')


df_shots_details <- "D:/Mestrado/NBA/data/dados_crawler/espn/" %>% 
  dir(full.names = TRUE) %>% 
  purrr::keep(.x = ., .p = stringr::str_detect, pattern = 'shotsespn') %>% 
  plyr::ldply(.data = ., .fun = function(path){
    
    xx <- load(path)
    xx <- get(xx)
    return(xx)
    
  }, .progress = 'time') %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(athlete_id = ifelse(is.na(athlete_id), stringr::str_remove(athlete, "^id\\:"), athlete_id)) %>% 
  dplyr::filter(is.na(`<NA>`)) %>% 
  dplyr::select(gameid, id, type_id, txt_shot, coordinatex, y, athlete_id, name, scoringPlay, shootingPlay, isFreeThrow) %>% 
  setNames(nm = c('gameid', 'id', 'shot_id', 'shot_text', 'coordx', 'coordy', 'athlete_id', 'athlete_name', 'scoringPlay',
                  'shootingPlay', 'isFreeThrow'))


save(df_shots_details, file = "D:/Mestrado/NBA/data/dados_crawler/espn/df_shots_details.RData")
