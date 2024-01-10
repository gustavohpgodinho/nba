
require(tidyverse)
require(xml2)
require(rvest)
require(RSelenium)
require(wdman)
require(netstat)

extract_basket_reference_link_months <- function(season, selenium_remote_driver){
  
  base_url <- 'https://www.basketball-reference.com'
  base_url_season <- paste0(base_url, "/leagues/NBA_", season, "_games.html")
  
  selenium_remote_driver$navigate(base_url_season)
  
  selenium_remote_driver$getPageSource()[[1]] %>% 
    xml2::read_html() %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr('href') %>% 
    data.frame(links = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::filter(stringr::str_detect(links, "\\/leagues\\/NBA"),
                  stringr::str_detect(links, "_games\\-")) %>% 
    dplyr::mutate(season = paste0(season - 1, '-', season)) %>% 
    dplyr::mutate(links = paste0(base_url, links)) %>% 
    dplyr::mutate(season = stringr::str_replace(season, "\\-[0-9]{2}", "-"))
  
}
get_basket_reference_nba_games <- function(df){
  
  
  page_content <- xml2::read_html(df$links)

  table_content <- page_content %>%
    rvest::html_nodes("#schedule") %>%
    rvest::html_table() %>%
    '[['(1) %>% 
    setNames(nm = c('date', 'hour_start_et', 'away_team', 'away_pts', 'home_team', 'home_pts',
                    'boxscore', 'ind_ot', 'attendance', 'arena', 'notes')) %>% 
    dplyr::filter(boxscore == "Box Score")

  table_links <- page_content %>%
    rvest::html_nodes("#schedule") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr('href')

  table_text <- page_content %>%
    rvest::html_nodes("#schedule") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()

  df_links <- data.frame(season = df$season[1],
                         text = table_text,
                         links = table_links,
                         stringsAsFactors = FALSE) %>%
    dplyr::filter(text == "Box Score") %>%
    dplyr::select(-text)

  Sys.sleep(rpois(1, 5))
  
  dplyr::bind_cols(table_content, df_links) %>% 
    dplyr::mutate_all(.funs = as.character)
  
  
}
get_basket_reference_data <- function(){
  
  nba_seasons <- 2009:2024
  
  remote_driver <- RSelenium::rsDriver(browser = 'chrome',
                                       chromever = "119.0.6045.105",
                                       verbose = FALSE, port = 4564L)
  
  remDr <- remote_driver$client
  remDr$open()



  links_br <- plyr::llply(.data = nba_seasons, 
                          .fun = extract_basket_reference_link_months,
                          selenium_remote_driver = remDr,
                          .progress = 'time') %>% 
    dplyr::bind_rows()
  
  remote_driver$server$stop()
  
  games_br_data <- links_br %>% 
    dplyr::group_split(links) %>% 
    plyr::llply(.data = ., 
                .fun = get_basket_reference_nba_games,
                .progress = 'time') %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(game_id = stringr::str_remove_all(links, "\\/boxscores\\/|\\.html"), 
                  attendance = stringr::str_remove(attendance, ","), 
                  ind_ot = ifelse(ind_ot == "", NA_character_, ind_ot), 
                  notes = ifelse(notes == "", NA_character_, notes)) %>% 
    dplyr::select(season, game_id, date, hour_start_et, home_team, home_pts, 
                  away_pts, away_team, ind_ot, attendance, arena, notes) %>% 
    dplyr::mutate_at(.vars = c('home_pts', 'away_pts', 'attendance'), .funs = as.integer)
  

  
  return(games_br_data)
  
}
extract_br_page_pbp <- function(df){
  
  print(paste0(Sys.time(), "--", df$BR_ID[1]))
  
  url <- df$pbp
  
  xdf <- url %>% 
    xml2::read_html() %>% 
    as.character() %>% 
    gsub(pattern = "\\\\", replacement = "", x = .) %>% 
    gsub(pattern = "\"", replacement = "", x = .)
  
  if(rbinom(n = 1, size = 1, prob = 0.2)){
    
    Sys.sleep(rpois(n = 1, lambda = 10))  
  }
  
  
  
  path <- paste0("D:/Mestrado/NBA/data/dados_crawler/paginas_html_br/", df$BR_ID[1], ".txt")
  
  fileConn <- file(path)
  writeLines(xdf, fileConn)
  close(fileConn)
  
}

find_match_links_espn <- function(df_dt){
  
  if(rbinom(n = 1, size = 1, prob = 0.01) == 1){
    
    Sys.sleep(rpois(n = 1, lambda = 10))
  }
  
  if(rbinom(n = 1, size = 1, prob = 0.01) == 1){
    
    Sys.sleep(rpois(n = 1, lambda = 30))
  }
  
  df_dt$link %>%
    rvest::read_html() %>%
    rvest::html_nodes('table') %>%
    rvest::html_nodes('a') %>%
    purrr::keep(.x = ., .p = function(node){
      
      node %>%
        rvest::html_attr('href') %>%
        stringr::str_detect(string = ., 
                            pattern = 'gameId')
      
    }) %>%
    plyr::ldply(.data = ., .fun = function(node){
      
      df_response <- data.frame(matchup = node %>% rvest::html_text(),
                                link = node %>% html_attr('href'),
                                stringsAsFactors = FALSE)
      
      
      if(nrow(df_response) > 0){
        
        df_response <- df_response %>% 
          dplyr::mutate(dt = df_dt$dt)
        
      }
      
      return(df_response)
    })
  
}
get_espn_data <- function(){
  
  dates_query <- seq(as.Date('2008-09-29'), Sys.Date(), by = 'week') %>% 
    data.frame(dt = .) %>% 
    dplyr::mutate(dt = stringr::str_remove_all(dt, "-")) %>%
    dplyr::mutate(link = paste0('https://www.espn.com/nba/schedule/_/date/', dt)) %>% 
    dplyr::as_tibble()
  
  games_data <- list()
  
  dates_query %>% 
    plyr::a_ply(.data = ., .margins = 1, .fun = function(df){
      
      xdf <- find_match_links_espn(df_dt = df)
      
      games_data[[length(games_data) + 1]] <<- xdf %>% 
        dplyr::mutate(base_dt = df$dt)
      
    }, .progress = 'time')
  
  games_data %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(game_id = stringr::str_extract(link, "\\d+")) %>% 
    dplyr::select(game_id, matchup, base_dt) %>% 
    dplyr::filter(stringr::str_detect(matchup, ".+?\\s+[0-9]+\\,\\s+.+?\\s+[0-9]+")) %>% 
    dplyr::mutate(ind_ot = stringr::str_extract(matchup, '\\s+\\((.+?)OT\\)|\\s+\\(OT\\)')) %>%
    dplyr::mutate(ind_ot = stringr::str_remove_all(ind_ot, "\\s+")) %>% 
    dplyr::mutate(matchup = stringr::str_remove(matchup, '\\s+\\((.+?)OT\\)|\\s+\\(OT\\)')) %>%
    dplyr::mutate(matchup = stringr::str_replace_all(matchup, "\\s+([0-9])", "_\\1")) %>% 
    tidyr::separate(col = 'matchup', into = c('home_team', 'away_team'), sep = '\\,\\s+') %>% 
    tidyr::separate(col = 'home_team', into = c('home_team', 'home_pts'), sep = '_') %>% 
    tidyr::separate(col = 'away_team', into = c('away_team', 'away_pts'), sep = '_') %>% 
    dplyr::arrange(game_id) %>% 
    dplyr::select(base_dt, game_id, home_team, home_pts, away_pts, away_team, ind_ot) %>% 
    dplyr::as_tibble()
  
}
extract_espn_page_pbp <- function(df){
  
  xdf <- df$pbp %>% 
    xml2::read_html() %>% 
    as.character() %>% 
    gsub(pattern = "\\\\", replacement = "", x = .) %>% 
    gsub(pattern = "\"", replacement = "", x = .)
  
  path <- paste0("D:/Mestrado/NBA/data/dados_crawler/paginas_html_espn/", df$ESPN_ID[1], ".txt")
  
  fileConn <- file(path)
  writeLines(xdf, fileConn)
  close(fileConn)
  
}


selenium_object <- wdman::selenium(retcommand = TRUE, check = FALSE)

binman::list_versions("chromedriver")

load("D:/Mestrado/NBA/jogos_nba.RData")

jogos_nba_br <- get_basket_reference_data()

save(jogos_nba_br, file = "D:/Mestrado/NBA/jogos_nba_br.RData")

jogos_nba_espn <- get_espn_data()

save(jogos_nba_espn, file = "D:/Mestrado/NBA/jogos_nba_espn.RData")


df_auxiliar_jogos_nba <- jogos_nba %>% 
  dplyr::mutate_at(.vars = c('HOME_TEAM', 'AWAY_TEAM'), 
                   .funs = stringr::str_replace, 
                   pattern = 'LA Clippers', 
                   replacement = 'Los Angeles Clippers') %>% 
  dplyr::select(GAME_DATE, GAME_ID, HOME_TEAM, 
                HOME_PTS, AWAY_PTS, AWAY_TEAM) %>% 
  setNames(nm = c('DATE', 'GAME_ID', 'HOME_TEAM', 'HOME_PTS', 'AWAY_PTS', 'AWAY_TEAM'))

df_br_to_nba <- jogos_nba_br %>% 
  dplyr::mutate_at(.vars = c('home_team', 'away_team'), 
                   .funs = stringr::str_replace, 
                   pattern = 'LA Clippers', 
                   replacement = 'Los Angeles Clippers') %>% 
  dplyr::mutate(dt = stringr::str_sub(game_id, 1, 8),
                dt = lubridate::ymd(dt)) %>% 
  dplyr::select(dt, game_id, home_team, home_pts, 
                away_pts, away_team) %>% 
  setNames(nm = c('DATE', 'BR_ID', 'HOME_TEAM', 'HOME_PTS', 
                  'AWAY_PTS', 'AWAY_TEAM')) %>% 
  dplyr::full_join(df_auxiliar_jogos_nba,
                   by = c('DATE', 'HOME_TEAM', 'HOME_PTS', 
                          'AWAY_PTS', 'AWAY_TEAM')) %>% 
  dplyr::filter(!is.na(BR_ID), !is.na(GAME_ID)) %>% 
  dplyr::arrange(GAME_ID)

df_auxiliar_datas <- jogos_nba_espn %>% 
  dplyr::distinct(base_dt) %>% 
  dplyr::mutate(date = lubridate::ymd(base_dt)) %>% 
  dplyr::mutate(ind = 1) %>% 
  dplyr::full_join(jogos_nba %>% 
                     dplyr::distinct(GAME_DATE) %>% 
                     dplyr::mutate(ind = 1), 
                   by = 'ind', relationship = 'many-to-many') %>% 
  dplyr::mutate(dif = GAME_DATE - date) %>% 
  dplyr::mutate(dif = as.integer(dif)) %>% 
  dplyr::filter(dif >= 0, dif <  7) %>% 
  dplyr::select(GAME_DATE, base_dt) %>% 
  dplyr::distinct()

df_auxiliar_jogos_nba <- jogos_nba %>% 
  dplyr::mutate_at(.vars = c('HOME_TEAM_ABBR', 'AWAY_TEAM_ABBR'), 
                   .funs = stringr::str_replace, 
                   pattern = 'NOH', replacement = 'NOP') %>% 
  dplyr::left_join(df_auxiliar_datas, by = 'GAME_DATE') %>% 
  dplyr::select(GAME_ID, base_dt, HOME_TEAM_ABBR, HOME_PTS, 
                AWAY_PTS, AWAY_TEAM_ABBR) %>% 
  setNames(nm = c('GAME_ID', 'base_dt', 'home_team', 'home_pts', 
                  'away_pts', 'away_team'))
  
df_espn_to_nba <- jogos_nba_espn %>% 
  dplyr::filter(!home_team %in% c('ADL', 'ALB', 'CSK', 'EAST', 'FCB', 'FUL', 'GIA', 'LEB', 'MDD', 'REAL', 'USA', 'WEST', 'WORLD')) %>% 
  dplyr::filter(!away_team %in% c('ADL', 'ALB', 'CSK', 'EAST', 'FCB', 'FUL', 'GIA', 'LEB', 'MDD', 'REAL', 'USA', 'WEST', 'WORLD')) %>% 
  dplyr::mutate_at(.vars = c('home_team', 'away_team'), .funs = stringr::str_replace, pattern = '^GS$', replacement = 'GSW') %>% 
  dplyr::mutate_at(.vars = c('home_team', 'away_team'), .funs = stringr::str_replace, pattern = '^NJ$', replacement = 'NJN') %>% 
  dplyr::mutate_at(.vars = c('home_team', 'away_team'), .funs = stringr::str_replace, pattern = '^NO$', replacement = 'NOP') %>% 
  dplyr::mutate_at(.vars = c('home_team', 'away_team'), .funs = stringr::str_replace, pattern = '^NY$', replacement = 'NYK') %>% 
  dplyr::mutate_at(.vars = c('home_team', 'away_team'), .funs = stringr::str_replace, pattern = '^SA$', replacement = 'SAS') %>% 
  dplyr::mutate_at(.vars = c('home_team', 'away_team'), .funs = stringr::str_replace, pattern = '^UTAH$', replacement = 'UTA') %>% 
  dplyr::mutate_at(.vars = c('home_team', 'away_team'), .funs = stringr::str_replace, pattern = '^WSH$', replacement = 'WAS') %>% 
  dplyr::mutate_at(.vars = c('home_pts', 'away_pts'), .funs = as.integer) %>% 
  dplyr::filter(base_dt %in% unique(df_auxiliar_datas$base_dt)) %>% 
  dplyr::left_join(df_auxiliar_jogos_nba,
                   by = c('base_dt', 'home_team', 'home_pts', 'away_pts', 'away_team')) %>% 
  dplyr::left_join(df_auxiliar_jogos_nba %>% 
                     setNames(nm = c('GAME_ID1', 'base_dt', 'away_team', 'away_pts', 'home_pts', 'home_team')),
                   by = c('base_dt', 'home_team', 'home_pts', 'away_pts', 'away_team')) %>% 
  dplyr::mutate(ind_change_teams = ifelse(!is.na(GAME_ID1), 1, 0)) %>% 
  dplyr::mutate(GAME_ID = ifelse(is.na(GAME_ID), GAME_ID1, GAME_ID)) %>% 
  dplyr::mutate(GAME_ID = ifelse(game_id == '400578620', "0021400328", GAME_ID)) %>% 
  dplyr::rename(ESPN_ID = game_id) %>% 
  dplyr::select(-GAME_ID1)
  
match_jogos_nba <- jogos_nba %>% 
  dplyr::left_join(df_br_to_nba %>% 
                     dplyr::select(GAME_ID, BR_ID), by = 'GAME_ID') %>% 
  dplyr::left_join(df_espn_to_nba %>% 
                     dplyr::select(GAME_ID, ESPN_ID), by = 'GAME_ID') %>% 
  dplyr::select(SEASON, SEASON_TYPE, GAME_DATE, GAME_ID, BR_ID, ESPN_ID)
  

save(match_jogos_nba, file = "D:/Mestrado/NBA/match_jogos_nba.RData")

# extract pbp basket reference
game_ids <- dir("D:/Mestrado/NBA/data/dados_crawler/paginas_html_br") %>% 
  stringr::str_remove_all(., ".txt") %>% 
  unique()

missing_games <- match_jogos_nba  %>% 
  dplyr::distinct(BR_ID, .keep_all = TRUE) %>% 
  dplyr::filter(!BR_ID %in% game_ids) %>% 
  dplyr::filter(!is.na(BR_ID)) %>% 
  dplyr::mutate(row = rnorm(n = nrow(.))) %>% 
  dplyr::mutate(pbp = paste0("https://www.basketball-reference.com/boxscores/pbp/", BR_ID, '.html')) %>% 
  dplyr::arrange(row)

missing_games %>%
  dplyr::mutate(aux = dplyr::row_number()) %>% 
  dplyr::group_split(aux) %>%
  plyr::l_ply(.data = ., .fun = function(df){
    
    tryCatch(expr = {
      
      df %>% 
        dplyr::mutate(pbp = stringr::str_replace(pbp, 'CHA\\.', 'CHO.')) %>%
        dplyr::mutate(pbp = stringr::str_replace(pbp, 'PHX\\.', 'PHO.')) %>%
        dplyr::mutate(pbp = stringr::str_replace(pbp, 'BKN\\.', 'BRK.')) %>%
        extract_br_page_pbp(df = .)
      
    }, error = function(e){
      
      print("Erro. ESPERANDO 6 MINUTOS PARA DESBLOQUEAR O SITE")
      Sys.sleep(360)
      
    })
    
  }, .progress = 'time')


##########################

# extract pbp basket reference
game_ids <- dir("D:/Mestrado/NBA/data/dados_crawler/paginas_html_espn") %>% 
  stringr::str_remove_all(., ".txt") %>% 
  unique()

missing_games <- match_jogos_nba  %>% 
  dplyr::distinct(ESPN_ID, .keep_all = TRUE) %>% 
  dplyr::filter(!ESPN_ID %in% game_ids) %>% 
  dplyr::filter(!is.na(ESPN_ID)) %>% 
  dplyr::mutate(row = rnorm(n = nrow(.))) %>% 
  dplyr::mutate(pbp = paste0("https://www.espn.com/nba/playbyplay/_/gameId/", ESPN_ID)) %>% 
  dplyr::arrange(row)

future::plan(future::multisession(), 
             workers = future::availableCores())

missing_games %>% 
  dplyr::group_split(row) %>% 
  furrr::future_map(.x = ., .f = extract_espn_page_pbp, .progress = TRUE)

future::plan(future::sequential())
