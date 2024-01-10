require(tidyverse)
require(future)
require(furrr)


#' @title extract_nba_website_page
#' 
#' @description Method to extract the html play by play page of the NBA games and save the raw text data
#' @param df dataframe with the GAME_ID and link of the game
#' @param path_save_files path to save the txt files
#'
#' @return
#' @export
#'
#' @examples
#' 
extract_nba_website_page <- function(df, path_save_files){
 

  tryCatch(expr = {
    
    obj <- xml2::read_html(x = df$link) %>% 
      as.character(x = .) %>% 
      gsub(pattern = "\\\\", replacement = "", x = .) %>% 
      gsub(pattern = "\"", replacement = "", x = .)
    
    path <- paste0(path_save_files, df$GAME_ID[1], ".txt")
    
    fileConn <- file(path)
    writeLines(obj, fileConn)
    close(fileConn)
    
  }, error = function(e){
    
    message("An error occurred")
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
      furrr::future_map(.x = ., .f = extract_nba_website_page, path_save_files = files_path, .progress = TRUE)
    
    future::plan(future::sequential())
    
  }
  
}

games_nba <- readr::read_delim(file = "D:/Mestrado/NBA/nba/data/games_nba.csv", 
                               delim = ';', show_col_types = FALSE) %>% 
  dplyr::mutate_at(.vars = c('HOME_TEAM_ID', 'AWAY_TEAM_ID'), .funs = as.character) %>% 
  dplyr::mutate_at(.vars = c('HOME_PTS', 'AWAY_PTS'), .funs = as.integer)

execute_extraction_nba_website_pages(nba_games = games_nba, 
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
