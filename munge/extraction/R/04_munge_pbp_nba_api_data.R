require(tidyverse)
require(future)
require(furrr)

#' @title pre_process_pbp_api_files
#'
#' @description pre-process play-by-play data extracted from nba api and save one RData file for each season
#' @param file csv file with play-by-play data extracted from nba api
#'
#' @return a data frame with play-by-play data pre-processed
#' @export
#'
#' @examples
pre_process_pbp_api_files <- function(file){
  
  season <- stringr::str_extract(file, "[0-9]{4}_[0-9]{2}") %>% 
    stringr::str_replace(., "_", "-")
  
  xdf <- readr::read_delim(file, delim = ';', show_col_types = FALSE) %>% 
    dplyr::mutate(aux = ifelse(!is.na(HOMEDESCRIPTION) & !is.na(VISITORDESCRIPTION), 1, 0),
                  aux = ifelse(!is.na(HOMEDESCRIPTION) & is.na(VISITORDESCRIPTION), 2, aux),
                  aux = ifelse(is.na(HOMEDESCRIPTION) & !is.na(VISITORDESCRIPTION), 3, aux),
                  aux = ifelse(is.na(HOMEDESCRIPTION) & is.na(VISITORDESCRIPTION), 4, aux)) %>% 
    dplyr::mutate(ind = ifelse(aux == 1 & stringr::str_detect(HOMEDESCRIPTION, 'BLOCK|STEAL'), 1, 0),
                  ind = ifelse(aux == 1 & stringr::str_detect(VISITORDESCRIPTION, 'BLOCK|STEAL'), 2, ind)) %>% 
    dplyr::mutate(DESCRIPTION = NA_character_) %>% 
    dplyr::mutate(DESCRIPTION = ifelse(ind == 1, paste0(VISITORDESCRIPTION, " ", HOMEDESCRIPTION), DESCRIPTION),
                  DESCRIPTION = ifelse(ind == 2, paste0(HOMEDESCRIPTION, " ", VISITORDESCRIPTION), DESCRIPTION),
                  DESCRIPTION = ifelse(aux == 2, HOMEDESCRIPTION, DESCRIPTION),
                  DESCRIPTION = ifelse(aux == 3, VISITORDESCRIPTION, DESCRIPTION),
                  DESCRIPTION = ifelse(aux == 4, NEUTRALDESCRIPTION, DESCRIPTION)) %>% 
    dplyr::select(-c(aux, ind)) %>% 
    dplyr::mutate_at(.vars = c('PERSON1TYPE', 'PERSON2TYPE', 'PERSON3TYPE'), 
                     .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::mutate(persons = paste0(PERSON1TYPE, PERSON2TYPE, PERSON3TYPE),
                  SEASON = season) %>% 
    dplyr::select(SEASON, GAME_ID, EVENTNUM, EVENTMSGTYPE, EVENTMSGACTIONTYPE, PERIOD,
                  WCTIMESTRING, PCTIMESTRING, DESCRIPTION, persons, PERSON1TYPE, 
                  PERSON2TYPE, PERSON3TYPE, PLAYER1_ID, PLAYER2_ID, PLAYER3_ID,
                  PLAYER1_NAME, PLAYER2_NAME, PLAYER3_NAME, PLAYER1_TEAM_ID, 
                  PLAYER2_TEAM_ID, PLAYER3_TEAM_ID, PLAYER1_TEAM_NICKNAME, 
                  PLAYER2_TEAM_NICKNAME, PLAYER3_TEAM_NICKNAME, PLAYER1_TEAM_CITY,
                  PLAYER2_TEAM_CITY, PLAYER3_TEAM_CITY, PLAYER1_TEAM_ABBREVIATION,
                  PLAYER2_TEAM_ABBREVIATION, PLAYER3_TEAM_ABBREVIATION, SCORE, 
                  SCOREMARGIN, VIDEO_AVAILABLE_FLAG)
  
  return(xdf)
}

FOLDER_API_DATA <- "D:/Mestrado/NBA/nba/data/temp/api/"
FOLDER_SAVE_API_PBP_SEASON_FILES <- "D:/Mestrado/NBA/nba/data/processed/pbp_season_files/api/"

future::plan(future::multisession(), 
             workers = future::availableCores())

list_pbp <- dir(FOLDER_API_DATA, full.names = TRUE) %>% 
  purrr::keep(.p = ~stringr::str_detect(., "[0-9]{4}_[0-9]{2}")) %>%
  furrr::future_map(.x = ., .f = pre_process_pbp_api_files, .progress = TRUE)

future::plan(future::sequential())

pbpa_2008_09 <- list_pbp[[1]]
save(pbpa_2008_09, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2008_09.RData"))
rm(pbpa_2008_09)

pbpa_2009_10 <- list_pbp[[2]]
save(pbpa_2009_10, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2009_10.RData"))
rm(pbpa_2009_10)

pbpa_2010_11 <- list_pbp[[3]]
save(pbpa_2010_11, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2010_11.RData"))
rm(pbpa_2010_11)

pbpa_2011_12 <- list_pbp[[4]]
save(pbpa_2011_12, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2011_12.RData"))
rm(pbpa_2011_12)

pbpa_2012_13 <- list_pbp[[5]]
save(pbpa_2012_13, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2012_13.RData"))
rm(pbpa_2012_13)

pbpa_2013_14 <- list_pbp[[6]]
save(pbpa_2013_14, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2013_14.RData"))
rm(pbpa_2013_14)

pbpa_2014_15 <- list_pbp[[7]]
save(pbpa_2014_15, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2014_15.RData"))
rm(pbpa_2014_15)

pbpa_2015_16 <- list_pbp[[8]]
save(pbpa_2015_16, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2015_16.RData"))
rm(pbpa_2015_16)

pbpa_2016_17 <- list_pbp[[9]]
save(pbpa_2016_17, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2016_17.RData"))
rm(pbpa_2016_17)

pbpa_2017_18 <- list_pbp[[10]]
save(pbpa_2017_18, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2017_18.RData"))
rm(pbpa_2017_18)

pbpa_2018_19 <- list_pbp[[11]]
save(pbpa_2018_19, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2018_19.RData"))
rm(pbpa_2018_19)

pbpa_2019_20 <- list_pbp[[12]]
save(pbpa_2019_20, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2019_20.RData"))
rm(pbpa_2019_20)

pbpa_2020_21 <- list_pbp[[13]]
save(pbpa_2020_21, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2020_21.RData"))
rm(pbpa_2020_21)

pbpa_2021_22 <- list_pbp[[14]]
save(pbpa_2021_22, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2021_22.RData"))
rm(pbpa_2021_22)

pbpa_2022_23 <- list_pbp[[15]]
save(pbpa_2022_23, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2022_23.RData"))
rm(pbpa_2022_23)

pbpa_2023_24 <- list_pbp[[16]]
save(pbpa_2023_24, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_2023_24.RData"))
rm(pbpa_2023_24)

pbpa_total <- dplyr::bind_rows(list_pbp) 
save(pbpa_total, file = paste0(FOLDER_SAVE_API_PBP_SEASON_FILES, "pbpa_total.RData"))
rm(pbpa_total)
