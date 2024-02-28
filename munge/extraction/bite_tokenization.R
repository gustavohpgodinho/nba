require(tidyverse)

FOLDER_PROCESSED_DATA <- "D:/Mestrado/NBA/nba/data/processed/"

load(paste0(FOLDER_PROCESSED_DATA, "pbp.RData"))

count_tokens <- function(df1, df2){
  
  tokens_ <- df1 %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(pcod) %>%
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::bind_rows(df2 %>%
                       dplyr::ungroup() %>% 
                       dplyr::group_by(pcod) %>%
                       dplyr::summarise(n = dplyr::n())) %>% 
    dplyr::group_by(pcod) %>% 
    dplyr::summarise(cnt = sum(n)) %>% 
    dplyr::mutate(n = stringr::str_count(pcod, "_")) %>% 
    dplyr::mutate(cod = stringr::str_remove_all(pcod, "p[0-9]{3}")) %>% 
    dplyr::select(n, pcod, cnt, cod)
  
  tokens_ %>% 
    dplyr::arrange(n, cod)
}

count_merges <- function(df){

  df %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(lagpcod, pcod) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::filter(!is.na(lagpcod)) %>% 
    dplyr::arrange(desc(n))
}

repeat_group_plays <- function(df, merge_){
  
  df %>% 
    dplyr::mutate(i = 1) %>% 
    dplyr::mutate(lagpcod = ifelse(is.na(lagpcod), "", lagpcod)) %>% 
    dplyr::mutate(i = ifelse(lagpcod == merge_$lagpcod & pcod == merge_$pcod, 0, i),
                  i = ifelse(is.na(i), 1, i),
                  i = ifelse(dplyr::lag(i) == 0 & i == 0, 1, i)) %>%
    dplyr::group_by(season, game_id, period, clock) %>%
    dplyr::mutate(si = cumsum(i)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(season, game_id, period, clock, si) %>% 
    dplyr::summarise(pcod = paste0(pcod, collapse = ''), .groups = 'drop_last') %>% 
    dplyr::mutate(n = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(game_id, period, desc(clock), si) %>% 
    dplyr::mutate(lagpcod = dplyr::lag(pcod), 
                  lagclock = dplyr::lag(clock)) %>% 
    dplyr::mutate(lagpcod = ifelse(clock == lagclock, lagpcod, NA_character_)) %>% 
    dplyr::mutate(lagpcod = ifelse(is.na(lagpcod), "", lagpcod))
  
}

create_dataframe_merge <- function(df, merge_){
  
  obj <- df %>%
    dplyr::mutate(lagpcod = ifelse(is.na(lagpcod), "", lagpcod)) %>% 
    dplyr::filter(lagpcod == merge_$lagpcod & pcod == merge_$pcod) %>% 
    dplyr::distinct(season, game_id, period, clock) 
  
  list_done <- list()
  
  list_done[[length(list_done) + 1]] <- df %>% 
    dplyr::anti_join(obj, by = c('season', 'game_id', 'period', 'clock')) %>% 
    dplyr::mutate(lagpcod = ifelse(is.na(lagpcod), "", lagpcod))
  
  df_to_group <- df %>% 
    dplyr::inner_join(obj, by = c('season', 'game_id', 'period', 'clock'))
  
  while(nrow(obj) > 0){
    
    df_to_group <- df_to_group %>% 
      repeat_group_plays(merge_ = merge_)
    
    obj <- df_to_group %>%
      dplyr::filter(lagpcod == merge_$lagpcod & pcod == merge_$pcod) %>% 
      dplyr::distinct(season, game_id, period, clock) 
    
    list_done[[length(list_done) + 1]] <- df_to_group %>% 
      dplyr::anti_join(obj, by = c('season', 'game_id', 'period', 'clock')) %>% 
      dplyr::select(-c(si, lagclock))
    
    df_to_group <- df_to_group %>% 
      dplyr::inner_join(obj, by = c('season', 'game_id', 'period', 'clock'))
  }
  
  
  dplyr::bind_rows(list_done)
  
}

count_number_plays <- function(df){
  
  df %>% 
    dplyr::group_by(season, game_id, period, clock) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop')
  
}

tokenize_plays <- function(df, num_tokens){
  
  df0 <- df %>% 
    dplyr::group_split(season) %>% 
    furrr::future_map(.x = ., .f = count_number_plays, .progress = TRUE) %>% 
    dplyr::bind_rows()
  
  df1 <- df0 %>% 
    dplyr::filter(n == 1) %>% 
    dplyr::inner_join(df, 
                      by = c('season', 'game_id', 'period', 'clock')) %>%  
    dplyr::select(game_id, period, clock, pcod)
  
  df2 <- df0 %>% 
    dplyr::filter(n > 1) %>%
    dplyr::arrange(season, game_id, period, desc(clock)) %>% 
    dplyr::inner_join(df, by = c('season', 'game_id', 'period', 'clock')) %>% 
    dplyr::mutate(lagpcod = dplyr::lag(pcod), 
                  lagclock = dplyr::lag(clock)) %>% 
    dplyr::mutate(lagpcod = ifelse(clock == lagclock, lagpcod, NA_character_)) %>% 
    dplyr::select(season, game_id, period, clock, lagpcod, pcod, n)
  
  tokens_ <- count_tokens(df1, df2)
  
  list_tokens <- list()
  list_data <- list()
  list_tokens[[length(list_tokens) + 1]] <- tokens_
  list_data[[length(list_data) + 1]] <- dplyr::bind_rows(
    df1 %>% 
      dplyr::rename(token = pcod),
    df2 %>% 
      dplyr::select(game_id, period, clock, pcod) %>% 
      dplyr::rename(token = pcod)
  ) %>% 
    dplyr::arrange(game_id, period, desc(clock))
  
  merges <- count_merges(df2)

  count_num_tokens <- nrow(tokens_)
  
  num_tokens <- num_tokens[num_tokens > count_num_tokens]
  
  next_merge <- merges %>% 
    dplyr::slice(1)
  
  df3 <- create_dataframe_merge(df2, merge_ = next_merge)
  
  checkpoints <- num_tokens - count_num_tokens
  
  max(checkpoints) %>% 
    seq_len() %>% 
    plyr::l_ply(.data = ., .fun = function(i){
      
      append_df1 <- df3 %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(n == 1) %>% 
        dplyr::select(game_id, period, clock, pcod)
      
      if(nrow(append_df1) > 0){
        
        df1 <<- df1 %>% 
          dplyr::bind_rows(append_df1)
      }
      
      df2 <<- df3 %>% 
        dplyr::filter(n > 1) %>% 
        dplyr::arrange(game_id, period, desc(clock)) %>% 
        dplyr::mutate(lagpcod = dplyr::lag(pcod), 
                      lagclock = dplyr::lag(clock)) %>% 
        dplyr::mutate(lagpcod = ifelse(clock == lagclock, lagpcod, NA_character_)) %>% 
        dplyr::select(season, game_id, period, clock, lagpcod, pcod, n)
      
      merges <- count_merges(df2)
      
      next_merge <- merges %>% 
        dplyr::slice(1)
      
      df3 <<- create_dataframe_merge(df2, merge_ = next_merge)
      
      
      if(i %in% checkpoints){
        
        tokens_ <- count_tokens(df1, df2)
        list_tokens[[length(list_tokens) + 1]] <<- tokens_
        
        list_data[[length(list_data) + 1]] <<- dplyr::bind_rows(
          df1 %>% 
            dplyr::rename(token = pcod),
          df2 %>% 
            dplyr::select(game_id, period, clock, pcod) %>% 
            dplyr::rename(token = pcod)
        ) %>% 
          dplyr::arrange(game_id, period, desc(clock))
        
        print(tokens_%>% dplyr::slice_tail(n = 900), n = 900)
        
      }
      
      
  }, .progress = 'time')
  
  # tokens_ <- count_tokens(df1, df2)
  # 
  # 
  # tokens_ %>% 
  #   dplyr::filter(n == 1) %>% 
  #   print(n = 900)
  # 
  # tokens_ %>% 
  #   dplyr::filter(n > 1) %>% 
  #   print(n = 900)
  
  
  list(list_tokens, list_data)
}

list_ <- pbp %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod, "_18\\(.\\)"), stringr::str_replace(cod, "_18\\(.\\)", "_18(0)"), cod),
                pcod = ifelse(stringr::str_detect(pcod, "_18\\(.\\)"), stringr::str_replace(pcod, "_18\\(.\\)", "_18(0)"), pcod)) %>% 
  tokenize_plays(df = ., num_tokens = c(100, 300, 600, 900, 1200, 1500))

names_ <- list_[[1]] %>% 
  plyr::llply(.data = ., .fun = function(x){
    paste0("q", nrow(x), "tokens")
    }) %>% 
  unlist()

names(list_[[1]]) <- names_
names(list_[[2]]) <- stringr::str_replace_all(names_, "q", "data")


dplyr::bind_rows(
  list_[[1]]$q302tokens %>% 
    dplyr::filter(n == 1) %>% 
    dplyr::mutate(qtd_tokens = 'q0302'),
  list_[[1]]$q600tokens %>% 
    dplyr::filter(n == 1) %>% 
    dplyr::mutate(qtd_tokens = 'q0600'),
  list_[[1]]$q900tokens %>% 
    dplyr::filter(n == 1) %>% 
    dplyr::mutate(qtd_tokens = 'q0900'),
  list_[[1]]$q1200tokens %>% 
    dplyr::filter(n == 1) %>% 
    dplyr::mutate(qtd_tokens = 'q1200'),
  list_[[1]]$q1500tokens %>% 
    dplyr::filter(n == 1) %>% 
    dplyr::mutate(qtd_tokens = 'q1500')) %>% 
  tidyr::spread(qtd_tokens, cnt)

list_tokens <- list_[[1]]
list_data <- list_[[2]]

save(list_tokens, file = paste0(FOLDER_PROCESSED_DATA, "list_all_seasons_tokens.RData"))
save(list_data, file = paste0(FOLDER_PROCESSED_DATA, "list_all_seasons_data_tokens.RData"))

##################

list_ <- pbp %>% 
  dplyr::filter(season %in% c('2018-19', '2019-20', '2020-21', '2021-22', '2022-23', '2023-24')) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod, "_18\\(.\\)"), stringr::str_replace(cod, "_18\\(.\\)", "_18(0)"), cod),
                pcod = ifelse(stringr::str_detect(pcod, "_18\\(.\\)"), stringr::str_replace(pcod, "_18\\(.\\)", "_18(0)"), pcod)) %>% 
  tokenize_plays(df = ., num_tokens = c(100, 300, 600, 900, 1200, 1500))

names_ <- list_[[1]] %>% 
  plyr::llply(.data = ., .fun = function(x){
    paste0("q", nrow(x), "tokens")
  }) %>% 
  unlist()

names(list_[[1]]) <- names_
names(list_[[2]]) <- stringr::str_replace_all(names_, "q", "data")

list_tokens <- list_[[1]]
list_data <- list_[[2]]

save(list_tokens, file = paste0(FOLDER_PROCESSED_DATA, "list_last6_seasons_tokens.RData"))
save(list_data, file = paste0(FOLDER_PROCESSED_DATA, "list_last6_seasons_data_tokens.RData"))

##################

list_ <- pbp %>% 
  dplyr::filter(season %in% c('2022-23', '2023-24')) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod, "_18\\(.\\)"), stringr::str_replace(cod, "_18\\(.\\)", "_18(0)"), cod),
                pcod = ifelse(stringr::str_detect(pcod, "_18\\(.\\)"), stringr::str_replace(pcod, "_18\\(.\\)", "_18(0)"), pcod)) %>% 
  tokenize_plays(df = ., num_tokens = c(100, 300, 600, 900, 1200, 1500))

names_ <- list_[[1]] %>% 
  plyr::llply(.data = ., .fun = function(x){
    paste0("q", nrow(x), "tokens")
  }) %>% 
  unlist()

names(list_[[1]]) <- names_
names(list_[[2]]) <- stringr::str_replace_all(names_, "q", "data")

list_tokens <- list_[[1]]
list_data <- list_[[2]]

save(list_tokens, file = paste0(FOLDER_PROCESSED_DATA, "list_last2_seasons_tokens.RData"))
save(list_data, file = paste0(FOLDER_PROCESSED_DATA, "list_last2_seasons_data_tokens.RData"))

