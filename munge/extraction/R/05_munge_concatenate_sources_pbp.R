
require(tidyverse)

#' @title concatenate_pbp_sources
#'
#' @param df_api dataframe with pbp extracted from nba api
#' @param df_crawler dataframe with pbp extracted from nba crawler
#'
#' @return a dataframe with pbp data from both sources
#' @export
#'
#' @examples
concatenate_pbp_sources <- function(df_api, df_crawler){
  
  df_api %>% 
    dplyr::left_join(df_crawler, 
                     by = c("SEASON" = "season", 
                            "GAME_ID" = "game_id", 
                            "EVENTNUM" = 'actionNumber', 
                            'PERIOD' = 'period')) %>% 
    dplyr::select(SEASON, GAME_ID, EVENTNUM, actionId, EVENTMSGTYPE, 
                  actionType, EVENTMSGACTIONTYPE, subType, PERIOD, 
                  clock, description, persons, location, shotDistance, 
                  xLegacy, yLegacy) %>% 
    dplyr::mutate(clock = stringr::str_remove(clock, "^PT"),
                  clock = stringr::str_replace(clock, "M", ":"),
                  clock = stringr::str_remove(clock, "S$"),
                  clock = stringr::str_replace(clock, "\\.", ":")) %>% 
    dplyr::mutate(location = ifelse(is.na(location), 'n', location)) %>% 
    setNames(nm = c('season', 'game_id', 'num_event', 'action_id', 'tp_event', 
                    'tp_action', 'tp_subevent', 'tp_subaction', 'period', 
                    'clock', 'description', 'persons', 'location', 
                    'shot_distance', 'xlegacy', 'ylegacy'))
  
}

FOLDER_PBP_SEASON_FILES <- "D:/Mestrado/NBA/nba/data/processed/pbp_season_files/"
FOLDER_PBP_SEASON_FILES_CRAWLER <- paste0(FOLDER_PBP_SEASON_FILES, "crawled_nbasite/")
FOLDER_PBP_SEASON_FILES_API <- paste0(FOLDER_PBP_SEASON_FILES, "api/")
FOLDER_PBP_SEASON_FILES_CONCATENATED <- paste0(FOLDER_PBP_SEASON_FILES, "concatenate_data/")


load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2008_09.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2008_09.RData"))
pbp2008_09 <- concatenate_pbp_sources(pbpa_2008_09, pbpc_2008_09)
save(pbp2008_09, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2008_09.RData"))
rm(pbpa_2008_09)
rm(pbpc_2008_09)
rm(pbp2008_09)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2009_10.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2009_10.RData"))
pbp2009_10 <- concatenate_pbp_sources(pbpa_2009_10, pbpc_2009_10)
save(pbp2009_10, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2009_10.RData"))
rm(pbpa_2009_10)
rm(pbpc_2009_10)
rm(pbp2009_10)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2010_11.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2010_11.RData"))
pbp2010_11 <- concatenate_pbp_sources(pbpa_2010_11, pbpc_2010_11)
save(pbp2010_11, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2010_11.RData"))
rm(pbpa_2010_11)
rm(pbpc_2010_11)
rm(pbp2010_11)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2011_12.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2011_12.RData"))
pbp2011_12 <- concatenate_pbp_sources(pbpa_2011_12, pbpc_2011_12)
save(pbp2011_12, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2011_12.RData"))
rm(pbpa_2011_12)
rm(pbpc_2011_12)
rm(pbp2011_12)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2012_13.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2012_13.RData"))
pbp2012_13 <- concatenate_pbp_sources(pbpa_2012_13, pbpc_2012_13)
save(pbp2012_13, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2012_13.RData"))
rm(pbpa_2012_13)
rm(pbpc_2012_13)
rm(pbp2012_13)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2013_14.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2013_14.RData"))
pbp2013_14 <- concatenate_pbp_sources(pbpa_2013_14, pbpc_2013_14)
save(pbp2013_14, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2013_14.RData"))
rm(pbpa_2013_14)
rm(pbpc_2013_14)
rm(pbp2013_14)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2014_15.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2014_15.RData"))
pbp2014_15 <- concatenate_pbp_sources(pbpa_2014_15, pbpc_2014_15)
save(pbp2014_15, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2014_15.RData"))
rm(pbpa_2014_15)
rm(pbpc_2014_15)
rm(pbp2014_15)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2015_16.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2015_16.RData"))
pbp2015_16 <- concatenate_pbp_sources(pbpa_2015_16, pbpc_2015_16)
save(pbp2015_16, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2015_16.RData"))
rm(pbpa_2015_16)
rm(pbpc_2015_16)
rm(pbp2015_16)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2016_17.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2016_17.RData"))
pbp2016_17 <- concatenate_pbp_sources(pbpa_2016_17, pbpc_2016_17)
save(pbp2016_17, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2016_17.RData"))
rm(pbpa_2016_17)
rm(pbpc_2016_17)
rm(pbp2016_17)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2017_18.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2017_18.RData"))
pbp2017_18 <- concatenate_pbp_sources(pbpa_2017_18, pbpc_2017_18)
save(pbp2017_18, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2017_18.RData"))
rm(pbpa_2017_18)
rm(pbpc_2017_18)
rm(pbp2017_18)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2018_19.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2018_19.RData"))
pbp2018_19 <- concatenate_pbp_sources(pbpa_2018_19, pbpc_2018_19)
save(pbp2018_19, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2018_19.RData"))
rm(pbpa_2018_19)
rm(pbpc_2018_19)
rm(pbp2018_19)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2019_20.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2019_20.RData"))
pbp2019_20 <- concatenate_pbp_sources(pbpa_2019_20, pbpc_2019_20)
save(pbp2019_20, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2019_20.RData"))
rm(pbpa_2019_20)
rm(pbpc_2019_20)
rm(pbp2019_20)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2020_21.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2020_21.RData"))
pbp2020_21 <- concatenate_pbp_sources(pbpa_2020_21, pbpc_2020_21)
save(pbp2020_21, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2020_21.RData"))
rm(pbpa_2020_21)
rm(pbpc_2020_21)
rm(pbp2020_21)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2021_22.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2021_22.RData"))
pbp2021_22 <- concatenate_pbp_sources(pbpa_2021_22, pbpc_2021_22)
save(pbp2021_22, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2021_22.RData"))
rm(pbpa_2021_22)
rm(pbpc_2021_22)
rm(pbp2021_22)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2022_23.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2022_23.RData"))
pbp2022_23 <- concatenate_pbp_sources(pbpa_2022_23, pbpc_2022_23)
save(pbp2022_23, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2022_23.RData"))
rm(pbpa_2022_23)
rm(pbpc_2022_23)
rm(pbp2022_23)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_2023_24.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_2023_24.RData"))
pbp2023_24 <- concatenate_pbp_sources(pbpa_2023_24, pbpc_2023_24)
save(pbp2023_24, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbp2023_24.RData"))
rm(pbpa_2023_24)
rm(pbpc_2023_24)
rm(pbp2023_24)

load(paste0(FOLDER_PBP_SEASON_FILES_API, "pbpa_total.RData"))
load(paste0(FOLDER_PBP_SEASON_FILES_CRAWLER, "pbpc_total.RData"))
pbptotal <- concatenate_pbp_sources(pbpa_total, pbpc_total)
save(pbptotal, file = paste0(FOLDER_PBP_SEASON_FILES_CONCATENATED, "pbptotal.RData"))
rm(pbpa_total)
rm(pbpc_total)
rm(pbptotal)

