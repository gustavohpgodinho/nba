require(tidyverse)
require(future)
require(furrr)

#' @title transform_pbp_text_file_into_dataframe
#'
#' @param df it is a dataframe with the game_id and text columns and broke this in a tidy dataframe
#'
#' @return a tidy dataframe withe one row for each action number in the game
#' @export
#'
#' @examples
transform_pbp_text_file_into_dataframe <- function(df){
  
  df$text %>% 
    stringr::str_replace_all(., pattern = "\\,\\s+", replacement = "[;]") %>% 
    stringr::str_split(string = ., pattern = ",") %>% 
    unlist() %>% 
    data.frame(key = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    tidyr::separate(col = key, 
                    into = c('key', 'value'), 
                    sep = "\\:", 
                    extra = 'merge', 
                    fill = 'left') %>% 
    tidyr::spread(key = key, value = value) %>% 
    dplyr::mutate(description = stringr::str_replace_all(description, '\\[;\\]', ', '))
  

}

#' @title read_pbp_text_file_and_save_rdata
#'
#' @description It's a method to read the dataframe and use the function transform_pbp_text_file_into_dataframe to build the dataframe and save the RData file.
#' @param df it is a dataframe with the game_id and text columns and broke this in a tidy dataframe
#' @param file_path it is the path to save the dataframe in RData files
#'
#' @return
#' @export
#'
#' @examples
read_pbp_text_file_and_save_rdata <- function(df, file_path){
  
  tryCatch(expr = {
    
    game_id <- df$file %>% 
      stringr::str_replace(pattern = "(^.+\\/)([0-9]{10})(\\.txt$)", replacement = "\\2")
    
    df_ <- read_file(file = df$file) %>% 
      stringr::str_extract_all(string = ., pattern = "\\{actionNumber(.+?)\\}") %>% 
      unlist() %>% 
      data.frame(text = ., stringsAsFactors = FALSE) %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(text = stringr::str_remove_all(string = text, pattern = "\\{|\\}")) %>% 
      plyr::adply(.data = ., .margins = 1, .fun = transform_pbp_text_file_into_dataframe) %>% 
      dplyr::as_tibble()
    
    file_name <- paste0(file_path, game_id, ".RData")
    
    save(df_, file = file_name)
    
  }, error = function(e){
    
    message("An error occurred. See what happened")
    print(e)
    break
    
  })

}

#' @title build_pbp_dataframe_by_season
#'
#' @description get the RData files of the play by play of each game and build a play by play dataframe with all games of the season
#' @param season season to build the dataframe
#' @param games a dataframe with the games details. It needs to have the GAME_ID column.
#' @param path_pbp_by_games_file A path where the pbp data of each game is saved.
#'
#' @return
#' @export
#'
#' @examples
build_pbp_dataframe_by_season <- function(season, games, path_pbp_by_games_file){
  
  games_ <- games %>% 
    dplyr::filter(SEASON == season) %>% 
    dplyr::select(GAME_ID, SEASON)
  
  df_ <- dir(path = path_pbp_by_games_file, full.names = TRUE) %>% 
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
  
  return(df_) 
}


#' @title extract_players_name_pattern
#'
#' @description get the pattern of the players name in play by play data
#' @param df a dataframe with play by play data
#'
#' @return a dataframe with the names of players that appear in the play by play data
#' @export
#'
#' @examples
extract_players_name_pattern <- function(df){
  
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


#' save_pbp_game_data_file
#'
#' @description It's a method to get the pbp missing data, read them and save RData dataframe files for each game
#' @param path_pbp_crawled_games path that saves the pbp nba site RData files. 
#' @param pbp_text_files  path that saves the pbp text nba site files.
#'
#' @return
#' @export
#'
#' @examples
save_pbp_game_data_file <- function(path_pbp_crawled_games, path_pbp_text_files){
  
  games_already_crawled <- dir(path_pbp_crawled_games) %>% 
    stringr::str_remove_all(string = ., pattern = "\\.RData")
  
  missing_games <- dir(path = path_pbp_text_files, full.names = TRUE) %>% 
    data.frame(file = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(game_id = stringr::str_extract(string = file, pattern = "[0-9]{10}")) %>% 
    dplyr::mutate(ind_season = stringr::str_sub(string = game_id, start = 1, end = 5)) %>% 
    dplyr::filter(!game_id %in% games_already_crawled)
  
  print(paste0(nrow(missing_games), " to extract the play by play data"))
  
  while(nrow(missing_games) > 0){
    
    games_already_crawled <- dir(path_pbp_crawled_games) %>% 
      stringr::str_remove_all(string = ., pattern = "\\.RData")
    
    missing_games <- dir(path = path_pbp_text_files, full.names = TRUE) %>% 
      data.frame(file = ., stringsAsFactors = FALSE) %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(game_id = stringr::str_extract(string = file, pattern = "[0-9]{10}")) %>% 
      dplyr::mutate(ind_season = stringr::str_sub(string = game_id, start = 1, end = 5)) %>% 
      dplyr::filter(!game_id %in% games_already_crawled)
    
    future::plan(future::multisession(), workers = future::availableCores())
    
    missing_games %>% 
      dplyr::group_split(game_id) %>% 
      furrr::future_map(.x = ., .f = read_pbp_text_file_and_save_rdata, file_path = path_pbp_crawled_games, .progress = TRUE)
    
    future::plan(future::sequential())
    
  }
  
}


#' @title remove_players_name_pbp
#'
#' @param df dataframe with pbp data
#' @param df_players_pattern dataframe with the pattern of the players name in each game
#'
#' @return a dataframe with the players name replaced by [player]
#' @export
#'
#' @examples
remove_players_name_pbp <- function(df, df_players_pattern){
  
  player_pattern <- df_players_pattern %>% 
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


# Part 1: Extract the pbp data that is missing
games_nba <- readr::read_delim(file = "D:/Mestrado/NBA/nba/data/games_nba.csv", 
                               delim = ';', show_col_types = FALSE) %>% 
  dplyr::mutate_at(.vars = c('HOME_TEAM_ID', 'AWAY_TEAM_ID'), .funs = as.character) %>% 
  dplyr::mutate_at(.vars = c('HOME_PTS', 'AWAY_PTS'), .funs = as.integer)

save_pbp_game_data_file(path_pbp_crawled_games = "D:/Mestrado/NBA/nba/data/crawled_pbp/crawled_pbp_nbasite/",
                        path_pbp_text_files = "D:/Mestrado/NBA/nba/data/crawler/html_nba_site/")


# Part 2: Group pbp data by season
season_list <- games_nba %>% 
  dplyr::distinct(SEASON) %>% 
  dplyr::pull(SEASON) 

future::plan(future::multisession(), workers = future::availableCores())

list_pbp <- furrr::future_map(.x = season_list, 
                              .f = build_pbp_dataframe_by_season, 
                              games = games_nba, 
                              path_pbp_by_games_file = "D:/Mestrado/NBA/nba/data/crawled_pbp/crawled_pbp_nbasite/", 
                              .progress = TRUE)

future::plan(future::sequential())


pbpc_2008_09 <- list_pbp[[1]]
save(pbpc_2008_09, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2008_09.RData")
rm(pbpc_2008_09)

pbpc_2009_10 <- list_pbp[[2]]
save(pbpc_2009_10, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2009_10.RData")
rm(pbpc_2009_10)

pbpc_2010_11 <- list_pbp[[3]]
save(pbpc_2010_11, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2010_11.RData")
rm(pbpc_2010_11)

pbpc_2011_12 <- list_pbp[[4]]
save(pbpc_2011_12, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2011_12.RData")
rm(pbpc_2011_12)

pbpc_2012_13 <- list_pbp[[5]]
save(pbpc_2012_13, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2012_13.RData")
rm(pbpc_2012_13)

pbpc_2013_14 <- list_pbp[[6]]
save(pbpc_2013_14, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2013_14.RData")
rm(pbpc_2013_14)

pbpc_2014_15 <- list_pbp[[7]]
save(pbpc_2014_15, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2014_15.RData")
rm(pbpc_2014_15)

pbpc_2015_16 <- list_pbp[[8]]
save(pbpc_2015_16, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2015_16.RData")
rm(pbpc_2015_16)

pbpc_2016_17 <- list_pbp[[9]]
save(pbpc_2016_17, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2016_17.RData")
rm(pbpc_2016_17)

pbpc_2017_18 <- list_pbp[[10]]
save(pbpc_2017_18, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2017_18.RData")
rm(pbpc_2017_18)

pbpc_2018_19 <- list_pbp[[11]]
save(pbpc_2018_19, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2018_19.RData")
rm(pbpc_2018_19)

pbpc_2019_20 <- list_pbp[[12]]
save(pbpc_2019_20, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2019_20.RData")
rm(pbpc_2019_20)

pbpc_2020_21 <- list_pbp[[13]]
save(pbpc_2020_21, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2020_21.RData")
rm(pbpc_2020_21)

pbpc_2021_22 <- list_pbp[[14]]
save(pbpc_2021_22, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2021_22.RData")
rm(pbpc_2021_22)

pbpc_2022_23 <- list_pbp[[15]]
save(pbpc_2022_23, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2022_23.RData")
rm(pbpc_2022_23)

pbpc_2023_24 <- list_pbp[[16]]
save(pbpc_2023_24, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_2023_24.RData")
rm(pbpc_2023_24)

pbpc_total <- dplyr::bind_rows(list_pbp) 
save(pbpc_total, file = "D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_total.RData")

# Part 3: Extract players patterns

future::plan(future::multisession(), workers = future::availableCores())

pbp_players_name_pattern <- pbpc_total %>% 
  dplyr::select(game_id, playerNameI1, playerName1, playerName2) %>% 
  dplyr::distinct() %>% 
  dplyr::group_split(game_id) %>% 
  furrr::future_map(.x = ., .f = extract_players_name_pattern, .progress = TRUE) %>% 
  dplyr::bind_rows()

future::plan(future::sequential())

save(pbp_players_name_pattern, file = "D:/Mestrado/NBA/nba/data/pbp_players_name_pattern.RData")

load("D:/Mestrado/NBA/nba/data/crawled_pbp_season_files/pbpc_total.RData")
load("D:/Mestrado/NBA/nba/data/pbp_players_name_pattern.RData")

future::plan(future::multisession(), workers = future::availableCores())

pbp_clean_description <- pbpc_total %>% 
  dplyr::group_split(game_id) %>% 
  furrr::future_map(.x = ., 
                    .f = remove_players_name_pbp, 
                    df_players_pattern = pbp_players_name_pattern,
                    .progress = TRUE) %>% 
  dplyr::bind_rows() %>% 
  dplyr::rename(clean_description = desc)

future::plan(future::sequential())

save(pbp_clean_description, file = "D:/Mestrado/NBA/nba/data/pbp_clean_description.RData")

rm(pbpc_total)
