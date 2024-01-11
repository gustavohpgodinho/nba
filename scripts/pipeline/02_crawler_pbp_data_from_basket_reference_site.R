
require(tidyverse)
require(xml2)
require(rvest)
require(RSelenium)
require(wdman)
require(netstat)


#' @title extract_basket_reference_link_game_months
#'
#' @description extract the months where the games are played in NBA. We need this to know the link of the games.
#' @param season season of the NBA that we want to extract the scheduled games.
#' @param selenium_remote_driver remote driver of the browser.
#'
#' @return data frame with the link of the months that we have game in a specific season.
#' @export
#'
#' @examples
extract_basket_reference_link_game_months <- function(season, selenium_remote_driver){
  
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

#' @title get_basket_reference_nba_games
#'
#' @description extract the games' information from the link of the games.
#' @param df data frame with the link with the months where games were played.
#'
#' @return data frame with the games' information and the link where play by play data is.
#' @export
#'
#' @examples
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


#' @title get_basket_reference_games_data
#'
#' @description concatenate the functions extract_basket_reference_link_game_months and get_basket_reference_nba_games 
#' @param nba_seasons seasons of the NBA that we want to extract the scheduled games.
#'
#' @return data frame with the games' information. with the link where play by play data is.
#' @export
#'
#' @examples
get_basket_reference_games_data <- function(nba_seasons = 2009:2025){
  
  remote_driver <- RSelenium::rsDriver(browser = 'chrome', chromever = "119.0.6045.105", verbose = FALSE, port = 4564L)
  
  remDr <- remote_driver$client
  remDr$open()
  
  print("Extracting the link of the games")
  links_months_basket_reference <- plyr::llply(.data = nba_seasons,
                                               .fun = extract_basket_reference_link_game_months,
                                               selenium_remote_driver = remDr,
                                               .progress = 'time') %>% 
    dplyr::bind_rows()
  
  remote_driver$server$stop()
  
  print("Extracting the games data")
  games_basket_reference_data <- links_months_basket_reference %>% 
    dplyr::group_split(links) %>% 
    plyr::llply(.data = .,  .fun = get_basket_reference_nba_games, .progress = 'time') %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(game_id = stringr::str_remove_all(links, "\\/boxscores\\/|\\.html"), 
                  attendance = stringr::str_remove(attendance, ","), 
                  ind_ot = ifelse(ind_ot == "", NA_character_, ind_ot), 
                  notes = ifelse(notes == "", NA_character_, notes)) %>% 
    dplyr::select(season, game_id, date, hour_start_et, home_team, 
                  home_pts, away_pts, away_team, ind_ot, attendance, 
                  arena, notes) %>% 
    dplyr::mutate_at(.vars = c('home_pts', 'away_pts', 'attendance'), 
                     .funs = as.integer)
  
  return(games_basket_reference_data)
  
}

#' @title extract_basket_reference_page_pbp
#'
#' @description It's a method to extract the play by play data from the link of the game and save the txt file in a directory.
#' @param df data with pbp link to extract the data.
#' @param path_save_files path to save the files.
#'
#' @return None. Its a method to save the play by play data.
#' @export
#'
#' @examples
extract_basket_reference_page_pbp <- function(df, path_save_files){
  
  print(paste0(Sys.time(), "--", df$game_id[1]))
  
  url <- df$pbp
  
  xdf <- url %>% 
    xml2::read_html() %>% 
    as.character() %>% 
    gsub(pattern = "\\\\", replacement = "", x = .) %>% 
    gsub(pattern = "\"", replacement = "", x = .)
  
  if(rbinom(n = 1, size = 1, prob = 0.2)){
    
    Sys.sleep(rpois(n = 1, lambda = 10))  
  }
  
  path <- paste0(path_save_files, df$game_id[1], ".txt")
  
  fileConn <- file(path)
  writeLines(xdf, fileConn)
  close(fileConn)
  
}

#' @title match_games_nbasite_and_basketreference
#'
#' @description Match the games from NBA site and Basket Reference.
#' @param nbasite_games dataframe with the games from NBA site.
#' @param basket_reference_games dataframe with the games from Basket Reference.
#'
#' @return dataframe with the games from NBA site and Basket Reference matched.
#' @export
#'
#' @examples
match_games_nbasite_and_basketreference <- function(nbasite_games, basket_reference_games){
  
  df_aux_nba_games <- nbasite_games %>% 
    dplyr::mutate_at(.vars = c('HOME_TEAM', 'AWAY_TEAM'), 
                     .funs = stringr::str_replace, 
                     pattern = 'LA Clippers', 
                     replacement = 'Los Angeles Clippers') %>% 
    dplyr::select(GAME_DATE, GAME_ID, HOME_TEAM, 
                  HOME_PTS, AWAY_PTS, AWAY_TEAM) %>% 
    setNames(nm = c('DATE', 'GAME_ID', 'HOME_TEAM', 'HOME_PTS', 'AWAY_PTS', 'AWAY_TEAM'))
  
  df_basketreference_to_nbasite <- basket_reference_games %>% 
    dplyr::mutate_at(.vars = c('home_team', 'away_team'), 
                     .funs = stringr::str_replace, 
                     pattern = 'LA Clippers', 
                     replacement = 'Los Angeles Clippers') %>% 
    dplyr::mutate(dt = stringr::str_sub(game_id, 1, 8),
                  dt = lubridate::ymd(dt)) %>% 
    dplyr::select(dt, game_id, home_team, home_pts, away_pts, away_team) %>% 
    setNames(nm = c('DATE', 'BR_ID', 'HOME_TEAM', 'HOME_PTS', 'AWAY_PTS', 'AWAY_TEAM')) %>% 
    dplyr::full_join(df_aux_nba_games, by = c('DATE', 'HOME_TEAM', 'HOME_PTS', 
                                              'AWAY_PTS', 'AWAY_TEAM')) %>% 
    dplyr::filter(!is.na(BR_ID), !is.na(GAME_ID)) %>% 
    dplyr::arrange(GAME_ID)
  
  matched_games <- nbasite_games %>% 
    dplyr::left_join(df_basketreference_to_nbasite %>% 
                       dplyr::select(GAME_ID, BR_ID), by = 'GAME_ID') %>% 
    dplyr::select(SEASON, SEASON_TYPE, GAME_DATE, GAME_ID, BR_ID)  
  
  return(matched_games)
}

#' @title execute_extraction_basketreference_website_pages
#'
#' @description Method to extract the play by play data from the games in basketreference website.
#' @param basketreference_games the games from basketreference where we find the matched game in NBA site games.
#' @param files_path the path to save the files with the play by play data.
#'
#' @return
#' @export
#'
#' @examples
execute_extraction_basketreference_website_pages <- function(basketreference_games, files_path){
  
  games_already_crawled <- dir(files_path) %>% 
    stringr::str_remove_all(., ".txt") %>% 
    unique()
  
  missing_games <- basketreference_games %>% 
    dplyr::distinct(game_id, .keep_all = TRUE) %>% 
    dplyr::filter(!is.na(game_id)) %>% 
    dplyr::filter(!game_id %in% games_already_crawled) %>% 
    dplyr::mutate(row = rnorm(n = nrow(.))) %>% 
    dplyr::mutate(pbp = paste0("https://www.basketball-reference.com/boxscores/pbp/", game_id, '.html')) %>% 
    dplyr::arrange(row)
  
  print(paste0(nrow(missing_games), " to download the play by play data"))
  
  while(nrow(missing_games) > 0){
    
    games_already_crawled <- dir(files_path) %>% 
      stringr::str_remove_all(., ".txt") %>% 
      unique()
    
    missing_games <- basketreference_games %>% 
      dplyr::distinct(game_id, .keep_all = TRUE) %>% 
      dplyr::filter(!is.na(game_id)) %>% 
      dplyr::filter(!game_id %in% games_already_crawled) %>% 
      dplyr::mutate(row = rnorm(n = nrow(.))) %>% 
      dplyr::mutate(pbp = paste0("https://www.basketball-reference.com/boxscores/pbp/", game_id, '.html')) %>% 
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
            extract_basket_reference_page_pbp(df = ., path_save_files = files_path)
          
        }, error = function(e){
          
          print("Error. We need to wait 6 minutes to try request again")
          break
          
        })
        
      }, .progress = 'time')

  }
  
}

# selenium_object <- wdman::selenium(retcommand = TRUE, check = FALSE)
# binman::list_versions("chromedriver")
 

basket_reference_games_nba <- get_basket_reference_games_data()

save(basket_reference_games_nba, file = "D:/Mestrado/NBA/nba/data/basket_reference_games_nba.RData")

games_nba <- readr::read_delim(file = "D:/Mestrado/NBA/nba/data/games_nba.csv", 
                               delim = ';', show_col_types = FALSE) %>% 
  dplyr::mutate_at(.vars = c('HOME_TEAM_ID', 'AWAY_TEAM_ID'), .funs = as.character) %>% 
  dplyr::mutate_at(.vars = c('HOME_PTS', 'AWAY_PTS'), .funs = as.integer)

match_games_nbasite_to_basket_reference <- match_games_nbasite_and_basketreference(nbasite_games = games_nba, 
                                                                                   basket_reference_games = basket_reference_games_nba)

save(match_games_nbasite_to_basket_reference, file = "D:/Mestrado/NBA/nba/data/match_games_nbasite_to_basket_reference.RData")


load("D:/Mestrado/NBA/nba/data/basket_reference_games_nba.RData")
load("D:/Mestrado/NBA/nba/data/match_games_nbasite_to_basket_reference.RData")

basket_reference_games_nba %>% 
  dplyr::inner_join(match_games_nbasite_to_basket_reference, by = c('game_id' = 'BR_ID')) %>%
  execute_extraction_basketreference_website_pages(basketreference_games = .,
                                                   files_path = "D:/Mestrado/NBA/nba/data/crawler/html_basket_reference_site/")
  



