require(tidyverse)
require(future)
require(furrr)

#' Transform Play-by-Play Text File into DataFrame
#'
#' This function takes a DataFrame with 'game_id' and 'text' columns, breaks it into a tidy DataFrame,
#' and returns one row for each action number in the game.
#'
#' @param df A DataFrame with 'game_id' and 'text' columns.
#' 
#' @return A tidy DataFrame with one row for each action number in the game.
#' @export
#'
#' @examples
#' df_example <- data.frame(game_id = c(123), text = c('actionNumber:1, description:First Action'))
#' transform_pbp_text_file_into_dataframe(df_example)
#'
transform_pbp_text_file_into_dataframe <- function(df){
  
  df$text %>% 
    stringr::str_replace_all(., pattern = "\\,\\s+", replacement = "[;]") %>% 
    stringr::str_replace_all(., pattern = "\\[(.*?)\\]", replacement = function(match)str_replace_all(match, ",", ";")) %>% 
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

#' Read Play-by-Play Text File and Save as RData
#'
#' This function reads a DataFrame with 'game_id' and 'text' columns, breaks it into a tidy DataFrame,
#' uses the function `transform_pbp_text_file_into_dataframe` to process the text data, 
#' and saves the resulting DataFrame as an RData file.
#'
#' @param df A DataFrame with 'game_id' and 'text' columns.
#' @param file_path The path to save the resulting DataFrame in RData files.
#'
#' @return None
#' @export
#'
#' @examples
#' df_example <- data.frame(file = c('path/to/file.txt'), stringsAsFactors = FALSE)
#' read_pbp_text_file_and_save_rdata(df_example, file_path = 'path/to/save/')
#'
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

#' @title Build Play-by-Play DataFrame by Season
#'
#' @description 
#' This function compiles RData files containing play-by-play data for each game in a given season.
#' It builds a tidy play-by-play dataframe with all games of the specified season.
#'
#' @param season The season for which to build the dataframe.
#' @param games A dataframe with game details, including the GAME_ID column.
#' @param path_pbp_by_games_file The path where the play-by-play data of each game is saved.
#'
#' @return A tidy dataframe containing play-by-play information for all games in the specified season.
#' @export
#'
#' @examples
#' games_data <- readr::read_delim(file = "path/to/games_data.csv", delim = ';', show_col_types = FALSE)
#' pbp_data <- build_pbp_dataframe_by_season(season = 2021, games = games_data, path_pbp_by_games_file = "path/to/pbp_files/")
#'
build_pbp_dataframe_by_season <- function(season, games, path_pbp_by_games_file){
  
  # Filter games for the specified season
  games_ <- games %>% 
    dplyr::filter(SEASON == season) %>% 
    dplyr::select(GAME_ID, SEASON)
  
  # Load RData files for each game, merging with game details
  df_ <- dir(path = path_pbp_by_games_file, full.names = TRUE) %>% 
    data.frame(file = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(GAME_ID = stringr::str_extract(string = file, pattern = "[0-9]{10}")) %>% 
    dplyr::inner_join(games_, by = 'GAME_ID') %>% 
    plyr::alply(.margins = 1, .fun = function(df) {
      
      xx <- load(df$file)
      xx <- get(xx)
      
      xx <- xx %>% 
        dplyr::mutate(game_id = df$GAME_ID[1]) %>% 
        dplyr::mutate(num_columns = length(xx))
      
      return(xx)
      
    }, .progress = 'time') %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate_all(.funs = function(x) ifelse(x == "", NA_character_, x)) %>% 
    dplyr::filter(num_columns <= 23) %>% 
    dplyr::group_by(game_id, actionNumber) %>% 
    dplyr::mutate(n = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_split(n) %>% 
    plyr::llply(.fun = function(df) {
      
      if (df$n[1] == 1) {
        
        df %>% 
          dplyr::rename(personId1 = personId, 
                        playerName1 = playerName, 
                        playerNameI1 = playerNameI,
                        teamId1 = teamId, 
                        teamTricode1 = teamTricode)
        
      } else if (df$n[1] == 2) {
        
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
        
      } else {
        
        print('error')
        df
        
      }
    }, .progress = 'time') %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate_at(.vars = c('period', 'actionNumber'), .funs = as.numeric) %>% 
    dplyr::mutate(season = season) %>% 
    dplyr::select(season, game_id, actionId, actionNumber, actionType, subType, 
                  clock, period, description, location, personId1, personId2, 
                  playerName1, playerName2, playerNameI1, playerName2, teamId1, 
                  teamId2, teamTricode1, teamTricode2, isFieldGoal, shotResult, 
                  shotDistance, xLegacy, yLegacy, scoreHome, scoreAway, 
                  pointsTotal, videoAvailable) %>% 
    dplyr::mutate_at(.vars = c('personId1', 'personId2', 'teamId1', 'teamId2'), 
                     .funs = function(x) ifelse(x == 0, NA_character_, x)) %>% 
    dplyr::mutate(shotDistance = ifelse(isFieldGoal == 0, NA_character_, shotDistance),
                  xLegacy = ifelse(isFieldGoal == 0, NA_character_, xLegacy),
                  yLegacy = ifelse(isFieldGoal == 0, NA_character_, yLegacy)) %>% 
    dplyr::mutate_at(.vars = c('shotDistance', 'xLegacy', 'yLegacy', 'scoreHome',
                               'scoreAway', 'pointsTotal', 'videoAvailable'), 
                     .funs = as.numeric) %>% 
    dplyr::arrange(game_id, period, actionNumber)
  
  return(df_) 
}


#' @title Extract Players' Name Pattern
#'
#' @description 
#' Extracts the pattern of players' names from play-by-play data.
#'
#' @param df A dataframe with play-by-play data.
#'
#' @return A dataframe with the names of players that appear in the play-by-play data.
#' @export
#'
#' @examples
#' pbp_data <- readr::read_delim(file = "path/to/pbp_data.csv", delim = ',', show_col_types = FALSE)
#' players_pattern <- extract_players_name_pattern(pbp_data)
#'
extract_players_name_pattern <- function(df){
  
  df %>% 
    dplyr::select(-game_id) %>% 
    unlist() %>% 
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


#' Save Play-by-Play Game Data Files
#'
#' @description 
#' This method retrieves missing play-by-play data, reads them, and saves RData dataframe files for each game.
#'
#' @param path_pbp_crawled_games Path to save the crawled play-by-play RData files.
#' @param path_pbp_text_files Path to the play-by-play text files.
#'
#' @return
#' @export
#'
#' @examples
#' save_pbp_game_data_file(path_pbp_crawled_games = "path/to/crawled_games", path_pbp_text_files = "path/to/pbp_text_files")
#'
save_pbp_game_data_file <- function(path_pbp_crawled_games, path_pbp_text_files){
  
  games_already_crawled <- dir(path_pbp_crawled_games) %>% 
    stringr::str_remove_all(string = ., pattern = "\\.RData")
  
  missing_games <- dir(path = path_pbp_text_files, full.names = TRUE) %>% 
    data.frame(file = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(game_id = stringr::str_extract(string = file, pattern = "[0-9]{10}")) %>% 
    dplyr::mutate(ind_season = stringr::str_sub(string = game_id, start = 1, end = 5)) %>% 
    dplyr::filter(!game_id %in% games_already_crawled)
  
  print(paste0(nrow(missing_games), " games to extract the play-by-play data"))
  
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
      furrr::future_map(.x = ., .f = read_pbp_text_file_and_save_rdata, 
                        file_path = path_pbp_crawled_games, .progress = TRUE)
    
    future::plan(future::sequential())
    
  }
  
}


FOLDER_PROCESSED_DATA <- "D:/Mestrado/NBA/nba/data/processed/"
FILE_GAMES <- paste0(FOLDER_PROCESSED_DATA, "games_nba.csv")
FOLDER_SAVE_PBP_RDATA <- "D:/Mestrado/NBA/nba/data/temp/pbp_nbasite/rdata/"
FOLDER_GET_PBP_TEXT_FILES <- "D:/Mestrado/NBA/nba/data/raw/html_nba_site/"
FOLDER_SAVE_PBP_SEASON_FILES <- paste0(FOLDER_PROCESSED_DATA, "pbp_season_files/crawled_nbasite/")


# Part 1: Extract the pbp data that is missing
# Read NBA games data
games_nba <- readr::read_delim(file = FILE_GAMES, delim = ';', show_col_types = FALSE) %>% 
  dplyr::mutate(dplyr::across(.cols = c(HOME_TEAM_ID, AWAY_TEAM_ID), .fns = as.character),
                dplyr::across(.cols = c(HOME_PTS, AWAY_PTS), .fns = as.integer))

save_pbp_game_data_file(path_pbp_crawled_games = FOLDER_SAVE_PBP_RDATA, path_pbp_text_files = FOLDER_GET_PBP_TEXT_FILES)


# Part 2: Group pbp data by season
season_list <- games_nba %>% 
  dplyr::distinct(SEASON) %>% 
  dplyr::pull(SEASON)

future::plan(future::multisession(), workers = future::availableCores())


list_pbp <- furrr::future_map(
  .x = season_list, 
  .f = build_pbp_dataframe_by_season, 
  games = games_nba, 
  path_pbp_by_games_file = FOLDER_SAVE_PBP_RDATA,
  .progress = TRUE)

future::plan(future::sequential())

season_list <- stringr::str_replace_all(season_list, '\\-', "_")

# Save individual seasons
pbpc_files <- purrr::map2(.x = list_pbp, .y = season_list, ~ {
  
  
  file_name <- paste0('pbpc_', .y)
  
  assign(file_name, .x)
  
  save(list = file_name, file = sprintf("%spbpc_%s.RData", FOLDER_SAVE_PBP_SEASON_FILES, .y))
  
  rm(file_name)
  
})

# Save total
pbpc_total <- dplyr::bind_rows(list_pbp)
save(pbpc_total, file = paste0(FOLDER_SAVE_PBP_SEASON_FILES, "pbpc_total.RData"))

# Part 3: Extract players patterns
future::plan(future::multisession(), workers = future::availableCores())

pbp_players_name_pattern <- pbpc_total %>% 
  dplyr::select(game_id, playerNameI1, playerName1, playerName2) %>% 
  dplyr::distinct() %>% 
  dplyr::group_split(game_id) %>% 
  furrr::future_map(.f = extract_players_name_pattern, .progress = TRUE) %>% 
  dplyr::bind_rows()

future::plan(future::sequential())

save(pbp_players_name_pattern, file = paste0(FOLDER_PROCESSED_DATA, "pbp_players_name_pattern.RData"))

