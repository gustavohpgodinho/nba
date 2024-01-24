require(tidyverse)
require(future)
require(furrr)

spread_pbp <- function(df){
  
  df$text %>% 
    stringr::str_replace_all(., pattern = "\\,\\s+", replacement = "[;]") %>% 
    stringr::str_split(string = ., pattern = ",") %>% 
    unlist() %>% 
    data.frame(key = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    tidyr::separate(col = key, into = c('key', 'value'), sep = "\\:", 
                    extra = 'merge', fill = 'left') %>% 
    tidyr::spread(key = key, value = value) %>% 
    dplyr::mutate(description = stringr::str_replace_all(description, '\\[;\\]', ', '))
  

}

read_pbp <- function(df){
  
  tryCatch(expr = {
    
    game_id <- df$file %>% 
      stringr::str_replace(pattern = "(^.+\\/)([0-9]{10})(\\.txt$)", replacement = "\\2")
    
    xdf <- read_file(file = df$file) %>% 
      stringr::str_extract_all(string = ., pattern = "\\{actionNumber(.+?)\\}") %>% 
      unlist() %>% 
      data.frame(text = ., stringsAsFactors = FALSE) %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(text = stringr::str_remove_all(string = text, pattern = "\\{|\\}")) %>% 
      plyr::adply(.data = ., .margins = 1, .fun = spread_pbp) %>% 
      dplyr::as_tibble()
    
    simple_file <- "D:/Mestrado/NBA/data/pbp_crawler/"
    file_name <- paste0(simple_file, game_id, ".RData")
    
    save(xdf, file = file_name)
    
  }, error = function(e){
    
    message("An error occurred")
    print(e)
    
  })

}

compact_pbp_by_season <- function(season, games){
  
  games_ <- games %>% 
    dplyr::filter(SEASON == season) %>% 
    dplyr::select(GAME_ID, SEASON)
  
  xdf <- dir(path = "D:/Mestrado/NBA/data/pbp_crawler", full.names = TRUE) %>% 
    data.frame(file = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(GAME_ID = stringr::str_extract(string = file, pattern = "[0-9]{10}")) %>% 
    dplyr::inner_join(games_, by = 'GAME_ID') %>% 
    plyr::alply(.data = ., .margins = 1, .fun = function(df){
      
      xx <- load(df$file) 
      
      xx <- get(xx)
      
      xx <- xx %>% 
        dplyr::mutate(game_id = df$GAME_ID[1])
      
      return(xx)
      
    }, .progress = 'time') %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate_all(.funs = function(x){ifelse(x == "", NA_character_, x)}) %>% 
    dplyr::group_by(game_id, actionNumber) %>% 
    dplyr::mutate(n = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_split(n) %>% 
    plyr::llply(.data = ., .fun = function(df){
      
      if(df$n[1] == 1){
        
        df %>% 
          dplyr::rename(personId1 = personId, 
                        playerName1 = playerName, 
                        playerNameI1 = playerNameI,
                        teamId1 = teamId, 
                        teamTricode1 = teamTricode)
        
      } else if(df$n[1] == 2){
        
        df %>% 
          dplyr::group_by(game_id, actionNumber) %>% 
          dplyr::mutate(description = paste0(description, collapse = " "),
                        personId = paste0(personId, collapse = ";"), 
                        playerName = paste0(playerName, collapse = ';'),
                        playerNameI = paste0(playerNameI, collapse = ";"),
                        teamId = paste0(teamId, collapse = ";"),
                        teamTricode = paste0(teamTricode, collapse = ";")) %>% 
          dplyr::ungroup() %>% 
          tidyr::separate(personId, c('personId1', 'personId2'), sep = ";") %>% 
          tidyr::separate(playerName, c('playerName1', 'playerName2'), sep = ";") %>% 
          tidyr::separate(playerNameI, c('playerNameI1', 'playerNameI2'), sep = ";") %>% 
          tidyr::separate(teamId, c('teamId1', 'teamId2'), sep = ";") %>% 
          tidyr::separate(teamTricode, c('teamTricode1', 'teamTricode2'), sep = ";") %>% 
          dplyr::filter(!is.na(actionType))
        
      } else{
        
        print('error')
        df
      }
      
    }, .progress = 'time') %>% 
    dplyr::bind_rows() %>% 
    dplyr::select(-n) %>% 
    dplyr::mutate_at(.vars = c('period', 'actionNumber'), .funs = as.numeric) %>% 
    dplyr::mutate(season = season) %>% 
    dplyr::arrange(game_id, period, actionNumber) %>% 
    dplyr::select(season, game_id, actionId, actionNumber, actionType, subType, 
                  clock, period, description, location, personId1, personId2, 
                  playerName1, playerName2, playerNameI1, playerName2, teamId1, 
                  teamId2, teamTricode1, teamTricode2, isFieldGoal, shotResult, 
                  shotDistance, xLegacy, yLegacy, scoreHome, scoreAway, 
                  pointsTotal, videoAvailable) %>% 
    dplyr::mutate_at(.vars = c('personId1', 'personId2', 'teamId1', 'teamId2'), 
                     .funs = function(x){ifelse(x == 0, NA_character_, x)}) %>% 
    dplyr::mutate(shotDistance = ifelse(isFieldGoal == 0, NA_character_, shotDistance),
                  xLegacy = ifelse(isFieldGoal == 0, NA_character_, xLegacy),
                  yLegacy = ifelse(isFieldGoal == 0, NA_character_, yLegacy)) %>% 
    dplyr::mutate_at(.vars = c('shotDistance', 'xLegacy', 'yLegacy', 'scoreHome',
                               'scoreAway', 'pointsTotal', 'videoAvailable'), 
                     .funs = as.numeric)
  
  return(xdf) 
}

load_pbp_data <- function(season, games){
  
  
  games_ <- games %>% 
    dplyr::filter(SEASON == season) %>% 
    dplyr::select(GAME_ID, SEASON)
  
  xdf <- dir(path = "D:/Mestrado/NBA/data/pbp_crawler", full.names = TRUE) %>% 
    data.frame(file = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(GAME_ID = stringr::str_extract(string = file, pattern = "[0-9]{10}")) %>% 
    dplyr::inner_join(games_, by = 'GAME_ID') %>% 
    plyr::alply(.data = ., .margins = 1, .fun = function(df){
      
      xx <- load(df$file) 
      
      xx <- get(xx)
      
      xx <- xx %>% 
        dplyr::mutate(game_id = df$GAME_ID[1])
      
      return(xx)
      
    }, .progress = 'time') %>% 
    dplyr::bind_rows()
  
  
}

extract_players_pattern <- function(df){
  
  df %>% 
    dplyr::select(-game_id) %>% 
    unlist() %>% 
    unname() %>% 
    na.omit() %>% 
    unique() %>% 
    data.frame(name = ., stringsAsFactors = FALSE) %>% 
    dplyr::mutate(size = stringr::str_length(name)) %>% 
    dplyr::arrange(desc(size)) %>% 
    dplyr::pull(name) %>% 
    paste0(collapse = '|') %>% 
    stringr::str_replace_all(., pattern = '\\.', repl = '\\\\.') %>% 
    stringr::str_replace_all(., pattern = '\\s+', repl = '\\\\s+') %>% 
    stringr::str_replace_all(., pattern = "\\'", repl = "\\\\'") %>% 
    stringr::str_replace_all(., pattern = "\\-", repl = "\\\\-") %>% 
    data.frame(game_id = df$game_id[1], players_pattern = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble()
  
}

load("D:/Mestrado/NBA/jogos_nba.RData")

games_already_crawled <- dir("D:/Mestrado/NBA/data/pbp_crawler") %>% 
  stringr::str_remove_all(string = ., pattern = "\\.RData")

missing_games <- dir(path = "D:/Mestrado/NBA/data/dados_crawler/paginas_html_nba",
                     full.names = TRUE) %>% 
  data.frame(file = ., stringsAsFactors = FALSE) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(game_id = stringr::str_extract(string = file, pattern = "[0-9]{10}")) %>% 
  dplyr::mutate(ind_season = stringr::str_sub(string = game_id, start = 1, end = 5)) %>% 
  dplyr::filter(!game_id %in% games_already_crawled)

while(nrow(missing_games) > 0){
  
  games_already_crawled <- dir("D:/Mestrado/NBA/data/pbp_crawler") %>% 
    stringr::str_remove_all(string = ., pattern = "\\.RData")
  
  missing_games <- dir(path = "D:/Mestrado/NBA/data/dados_crawler/paginas_html_nba",
                       full.names = TRUE) %>% 
    data.frame(file = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(game_id = stringr::str_extract(string = file, pattern = "[0-9]{10}")) %>% 
    dplyr::mutate(ind_season = stringr::str_sub(string = game_id, start = 1, end = 5)) %>% 
    dplyr::filter(!game_id %in% games_already_crawled)
  
  future::plan(future::multisession(), 
               workers = future::availableCores())
  
  missing_games %>% 
    dplyr::group_split(game_id) %>% 
    furrr::future_map(.x = ., .f = read_pbp, .progress = TRUE)
  
  future::plan(future::sequential())
  
}

season_list <- jogos_nba %>% 
  dplyr::distinct(SEASON) %>% 
  dplyr::pull(SEASON) 

future::plan(future::multisession(), 
             workers = future::availableCores())

list_pbp <- furrr::future_map(.x = season_list, .f = compact_pbp_by_season, games = jogos_nba, .progress = TRUE)

future::plan(future::sequential())


pbpc_2008_09 <- list_pbp[[1]]
save(pbpc_2008_09, file = "D:/Mestrado/NBA/data/pbpc_2008_09.RData")
rm(pbpc_2008_09)

pbpc_2009_10 <- list_pbp[[2]]
save(pbpc_2009_10, file = "D:/Mestrado/NBA/data/pbpc_2009_10.RData")
rm(pbpc_2009_10)

pbpc_2010_11 <- list_pbp[[3]]
save(pbpc_2010_11, file = "D:/Mestrado/NBA/data/pbpc_2010_11.RData")
rm(pbpc_2010_11)

pbpc_2011_12 <- list_pbp[[4]]
save(pbpc_2011_12, file = "D:/Mestrado/NBA/data/pbpc_2011_12.RData")
rm(pbpc_2011_12)

pbpc_2012_13 <- list_pbp[[5]]
save(pbpc_2012_13, file = "D:/Mestrado/NBA/data/pbpc_2012_13.RData")
rm(pbpc_2012_13)

pbpc_2013_14 <- list_pbp[[6]]
save(pbpc_2013_14, file = "D:/Mestrado/NBA/data/pbpc_2013_14.RData")
rm(pbpc_2013_14)

pbpc_2014_15 <- list_pbp[[7]]
save(pbpc_2014_15, file = "D:/Mestrado/NBA/data/pbpc_2014_15.RData")
rm(pbpc_2014_15)

pbpc_2015_16 <- list_pbp[[8]]
save(pbpc_2015_16, file = "D:/Mestrado/NBA/data/pbpc_2015_16.RData")
rm(pbpc_2015_16)

pbpc_2016_17 <- list_pbp[[9]]
save(pbpc_2016_17, file = "D:/Mestrado/NBA/data/pbpc_2016_17.RData")
rm(pbpc_2016_17)

pbpc_2017_18 <- list_pbp[[10]]
save(pbpc_2017_18, file = "D:/Mestrado/NBA/data/pbpc_2017_18.RData")
rm(pbpc_2017_18)

pbpc_2018_19 <- list_pbp[[11]]
save(pbpc_2018_19, file = "D:/Mestrado/NBA/data/pbpc_2018_19.RData")
rm(pbpc_2018_19)

pbpc_2019_20 <- list_pbp[[12]]
save(pbpc_2019_20, file = "D:/Mestrado/NBA/data/pbpc_2019_20.RData")
rm(pbpc_2019_20)

pbpc_2020_21 <- list_pbp[[13]]
save(pbpc_2020_21, file = "D:/Mestrado/NBA/data/pbpc_2020_21.RData")
rm(pbpc_2020_21)

pbpc_2021_22 <- list_pbp[[14]]
save(pbpc_2021_22, file = "D:/Mestrado/NBA/data/pbpc_2021_22.RData")
rm(pbpc_2021_22)

pbpc_2022_23 <- list_pbp[[15]]
save(pbpc_2022_23, file = "D:/Mestrado/NBA/data/pbpc_2022_23.RData")
rm(pbpc_2022_23)

pbpc_2023_24 <- list_pbp[[16]]
save(pbpc_2023_24, file = "D:/Mestrado/NBA/data/pbpc_2023_24.RData")
rm(pbpc_2023_24)

pbpc_total <- dplyr::bind_rows(list_pbp) 
save(pbpc_total, file = "D:/Mestrado/NBA/data/pbpc_total.RData")



future::plan(future::multisession(), 
             workers = future::availableCores())

pbp_players_pattern <- pbpc_total %>% 
  dplyr::select(game_id, playerNameI1, playerName1, playerName2) %>% 
  dplyr::distinct() %>% 
  dplyr::group_split(game_id) %>% 
  furrr::future_map(.x = ., .f = extract_players_pattern, .progress = TRUE) %>% 
  dplyr::bind_rows()

future::plan(future::sequential())


save(pbp_players_pattern, file = "D:/Mestrado/NBA/pbp_players_pattern.RData")

load("D:/Mestrado/NBA/jogos_nba.RData")
load("D:/Mestrado/NBA/data/pbpc_total.RData")
load("D:/Mestrado/NBA/pbp_players_pattern.RData")

remove_players_name_pbp <- function(df){
  
  
  player_pattern <- pbp_players_pattern %>% 
    dplyr::filter(game_id == df$game_id[1]) %>% 
    dplyr::pull(players_pattern)
  
  
  df %>% 
    dplyr::mutate(desc = stringr::str_replace_all(description, player_pattern, '[player]')) %>% 
    dplyr::mutate(desc = stringr::str_replace_all(desc, "[0-9]+\\'", "[dist]"), 
                  desc = stringr::str_replace_all(desc, "[0-9]+\\s+(PTS|AST|BLK|STL|PF)", "cnt \\1"), 
                  desc = stringr::str_replace_all(desc, "Off\\:[0-9]+\\s+Def\\:[0-9]+", "Off:cnt Def:cnt"), 
                  desc = stringr::str_replace_all(desc, "P[0-9]+\\.T[0-9]+", "Pcnt.Tcnt"), 
                  desc = stringr::str_replace_all(desc, "Reg\\.[0-9]+\\s+Short\\s+[0-9]+", "Reg.cnt Short cnt"), 
                  desc = stringr::str_replace_all(desc, "Full\\s+[0-9]+\\s+Short\\s+[0-9]+", "Full cnt Short cnt"), 
                  desc = stringr::str_replace_all(desc, "[0-9]+\\:[0-9]+\\s+PM\\s+EST", "[hour]"), 
                  desc = stringr::str_replace_all(desc, "P[0-9]+\\.PN", "Pcnt.PN"), 
                  desc = stringr::str_replace_all(desc, "T#[0-9]+", "T#cnt")) %>% 
    dplyr::select(game_id, period, clock, description, desc)
  
}


future::plan(future::multisession(), 
             workers = future::availableCores())

pbp_clean <- pbpc_total %>% 
  dplyr::inner_join(jogos_nba %>% 
                      dplyr::filter(SEASON == '2019-20') %>% 
                      dplyr::select(GAME_ID), 
                    by = c('game_id' = 'GAME_ID')) %>% 
  dplyr::group_split(game_id) %>% 
  furrr::future_map(.x = ., .f = remove_players_name_pbp, .progress = TRUE) %>% 
  dplyr::bind_rows()

future::plan(future::sequential())


rm(pbpc_total)