require(tidyverse)

jogos_nba <- readr::read_delim(file = "D:/Mestrado/NBA/jogos_nba.csv", 
                               delim = ';') %>% 
  dplyr::mutate_at(.vars = c('HOME_TEAM_ID', 'AWAY_TEAM_ID'), 
                   .funs = as.character) %>% 
  dplyr::mutate_at(.vars = c('HOME_PTS', 'AWAY_PTS'), 
                   .funs = as.integer)

save(jogos_nba, file = "D:/Mestrado/NBA/jogos_nba.RData")
