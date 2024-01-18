require(tidyverse)
require(future)
require(furrr)


#' Extract NBA Website Page
#' 
#' Method to extract the HTML play-by-play page of NBA games and save the raw text data.
#'
#' @param df A dataframe with 'GAME_ID' and 'link' columns representing the game identifier and the URL.
#' @param path_save_files The path to save the generated text files.
#'
#' @return None
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   df_example <- data.frame(GAME_ID = c(1), link = c("https://example.com"))
#'   path_save_files_example <- "/path/to/save/files"
#'   extract_nba_game_page (df_example, path_save_files_example)
#' }
#' 
#' @importFrom xml2 read_html
#' @import base writeLines
#' 
#' @export
extract_nba_game_page  <- function(df, path_save_files) {
  
  tryCatch({
    
    # Read HTML content
    obj <- xml2::read_html(df$link) %>%
      as.character() %>%
      gsub(pattern = '[\\\\"]', replacement = '', .)
    
    # Define the path to save the file
    path <- file.path(path_save_files, paste0(df$GAME_ID[1], '.txt'))
    
    # Save the cleaned HTML content to a text file
    writeLines(obj, path)
    
  }, error = function(e) {
    message('An error occurred')
    print(e)
    
  })
  
}




#' @title execute_extraction_nba_website_pages
#' 
#' @description Method to execute the extraction of the games that the play by play page was not extracted yet
#' @param nba_games dataframe with the data of the games of the NBA with the GAME_ID
#' @param files_path  path to save the txt files
#'
#' @return
#' @export
#'
#' @examples
execute_extraction_nba_website_pages <- function(nba_games, files_path){
  
  games_already_crawled <- dir(files_path) %>% 
    stringr::str_remove_all(., ".txt") %>% 
    unique()
  
  missing_games <- nba_games %>% 
    dplyr::select(GAME_ID, SEASON) %>% 
    dplyr::mutate(aux = rnorm(n = nrow(.))) %>% 
    dplyr::arrange(desc(aux)) %>% 
    dplyr::filter(!GAME_ID %in% games_already_crawled) %>% 
    dplyr::mutate(link = paste0("https://www.nba.com/game/cle-vs-bos-", GAME_ID, "/play-by-play?period=All")) %>% 
    dplyr::mutate(row = dplyr::row_number())
  
  while(nrow(missing_games) > 0){
    
    games_already_crawled <- dir(files_path) %>% 
      stringr::str_remove_all(., ".txt") %>% 
      unique()
    
    missing_games <- nba_games %>% 
      dplyr::select(GAME_ID, SEASON) %>% 
      dplyr::mutate(aux = rnorm(n = nrow(.))) %>% 
      dplyr::arrange(desc(aux)) %>% 
      dplyr::filter(!GAME_ID %in% games_already_crawled) %>% 
      dplyr::mutate(link = paste0("https://www.nba.com/game/cle-vs-bos-", GAME_ID, "/play-by-play?period=All")) %>% 
      dplyr::mutate(row = dplyr::row_number())
    
    future::plan(future::multisession(), workers = future::availableCores())
    
    missing_games %>% 
      dplyr::group_split(row) %>% 
      furrr::future_map(.x = ., .f = extract_nba_game_page , path_save_files = files_path, .progress = TRUE)
    
    future::plan(future::sequential())
    
  }
  
}


#' Execute Extraction of NBA Website Pages
#' 
#' Method to execute the extraction of play-by-play pages for NBA games that haven't been extracted yet.
#'
#' @param nba_games A dataframe with data of NBA games, including 'GAME_ID' and 'SEASON' columns.
#' @param files_path The path to save the generated text files.
#'
#' @return None
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   nba_games_example <- data.frame(GAME_ID = c(1, 2, 3), SEASON = c(2020, 2020, 2021))
#'   files_path_example <- "/path/to/save/files"
#'   extract_nba_play_by_play_pages(nba_games_example, files_path_example)
#' }
#'
#' @importFrom dplyr select mutate arrange filter row_number
#' @importFrom furrr future_map
#' @importFrom stringr str_remove_all
#' 
#' @export
extract_nba_play_by_play_pages  <- function(nba_games, files_path){
  
  
  # Identify games that have already been crawled
  games_already_crawled <- dir(files_path) %>%
    stringr::str_remove_all(., ".txt") %>%
    unique()
  
  # Identify games that are missing
  missing_games <- nba_games %>%
    dplyr::select(GAME_ID, SEASON) %>%
    dplyr::mutate(aux = runif(n = nrow(.))) %>%
    dplyr::arrange(desc(aux)) %>%
    dplyr::filter(!GAME_ID %in% games_already_crawled) %>%
    dplyr::mutate(link = paste0("https://www.nba.com/game/cle-vs-bos-", GAME_ID, "/play-by-play?period=All")) %>%
    dplyr::mutate(row = dplyr::row_number())
  
  print(nrow(missing_games), "games to extract pages.")
  
  # Loop until all missing games are extracted
  while(nrow(missing_games) > 0) {
    
    # Identify games that have already been crawled (again)
    games_already_crawled <- dir(files_path) %>% 
      stringr::str_remove_all(., ".txt") %>% 
      unique()
    
    # Identify games that are missing (again)
    missing_games <- nba_games %>%
      dplyr::select(GAME_ID, SEASON) %>%
      dplyr::mutate(aux = runif(n = nrow(.))) %>%
      dplyr::arrange(desc(aux)) %>%
      dplyr::filter(!GAME_ID %in% games_already_crawled) %>%
      dplyr::mutate(link = paste0("https://www.nba.com/game/cle-vs-bos-", GAME_ID, "/play-by-play?period=All")) %>%
      dplyr::mutate(row = row_number())
    
    
    # Use futures for parallel extraction
    future::plan(future::multisession(), workers = future::availableCores())
    
    # Extract play-by-play pages for missing games
    missing_games %>%
      group_split(row) %>%
      furrr::future_map(.f = extract_nba_game_page, path_save_files = files_path, .progress = TRUE)
    
    future::plan(future::sequential())
  }
}


# Read NBA games data
games_nba <- readr::read_delim(
  file = "D:/Mestrado/NBA/nba/data/games_nba.csv",
  delim = ';',
  show_col_types = FALSE) %>% 
  dplyr::mutate(dplyr::across(.cols = c(HOME_TEAM_ID, AWAY_TEAM_ID), .fns = as.character),
                dplyr::across(.cols = c(HOME_PTS, AWAY_PTS), .fns = as.integer))

# Extract play-by-play pages
extract_nba_play_by_play_pages(nba_games = games_nba, 
                               files_path = "D:/Mestrado/NBA/nba/data/crawler/html_nba_site/")


# games_already_crawled <- dir("D:/Mestrado/NBA/nba/data/crawler/html_nba_site/") %>% 
#   stringr::str_remove_all(., ".txt") %>% 
#   unique()
# 
# missing_games <- games_nba %>% 
#   dplyr::select(GAME_ID, SEASON) %>% 
#   dplyr::mutate(aux = rnorm(n = nrow(.))) %>% 
#   dplyr::arrange(desc(aux)) %>% 
#   dplyr::filter(!GAME_ID %in% games_already_crawled) %>% 
#   dplyr::mutate(link = paste0("https://www.nba.com/game/cle-vs-bos-", GAME_ID, "/play-by-play?period=All")) %>% 
#   dplyr::mutate(row = dplyr::row_number())
