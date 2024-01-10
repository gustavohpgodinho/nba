require(tidyverse)
require(future)
require(furrr)

load("D:/Mestrado/NBA/jogos_nba.RData")

extract_nba_website_page <- function(df){
  
  tryCatch(expr = {
    
    xdf <- xml2::read_html(x = df$link) %>% 
      as.character(x = .) %>% 
      gsub(pattern = "\\\\", replacement = "", x = .) %>% 
      gsub(pattern = "\"", replacement = "", x = .)
    
    path <- paste0("D:/Mestrado/NBA/data/dados_crawler/paginas_html_nba/", df$GAME_ID[1], ".txt")
    
    fileConn <- file(path)
    writeLines(xdf, fileConn)
    close(fileConn)
    
  }, error = function(e){
    
    message("An error occurred")
    print(e)
    
  })
  
  
}

games_already_crawled <- dir("D:/Mestrado/NBA/data/dados_crawler/paginas_html_nba") %>% 
  stringr::str_remove_all(., ".txt|.csv") %>% 
  unique()

missing_games <- jogos_nba %>% 
  dplyr::select(GAME_ID, SEASON) %>% 
  dplyr::mutate(aux = rnorm(n = nrow(.))) %>% 
  dplyr::arrange(desc(aux)) %>% 
  dplyr::filter(!GAME_ID %in% games_already_crawled) %>% 
  dplyr::mutate(link = paste0("https://www.nba.com/game/cle-vs-bos-", GAME_ID, "/play-by-play?period=All")) %>% 
  dplyr::mutate(linha = dplyr::row_number())

while(nrow(missing_games) > 0){
  
  games_already_crawled <- dir("D:/Mestrado/NBA/data/dados_crawler/paginas_html_nba") %>% 
    stringr::str_remove_all(., ".txt") %>% 
    unique()
  
  missing_games <- jogos_nba %>% 
    dplyr::select(GAME_ID, SEASON) %>% 
    dplyr::mutate(aux = rnorm(n = nrow(.))) %>% 
    dplyr::arrange(desc(aux)) %>% 
    dplyr::filter(!GAME_ID %in% games_already_crawled) %>% 
    dplyr::mutate(link = paste0("https://www.nba.com/game/cle-vs-bos-", GAME_ID, "/play-by-play?period=All")) %>% 
    dplyr::mutate(row = dplyr::row_number())
  
  future::plan(future::multisession(), 
               workers = future::availableCores())
  
  missing_games %>% 
    dplyr::group_split(row) %>% 
    furrr::future_map(.x = ., .f = extract_nba_website_page, .progress = TRUE)
  
  future::plan(future::sequential())
  
}
