require(tidyverse)
require(future)

extract_br_pbp_df <- function(file){
  
  gameid <- stringr::str_extract(file, "[0-9]+[A-Z]{3}")
  
  rvest::read_html(file) %>% 
    rvest::html_table(header = FALSE) %>% 
    '[['(1) %>% 
    dplyr::mutate(cod = dplyr::row_number()) %>% 
    tidyr::gather("column", "value", -cod) %>% 
    dplyr::arrange(cod, column) %>% 
    dplyr::filter(value != "") %>% 
    dplyr::group_by(cod) %>% 
    dplyr::mutate(n = dplyr::n_distinct(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(key = ifelse(n == 1, 'period', NA_character_),
                  key = ifelse(n == 2 & column != "X1", 'neutral', key),
                  key = ifelse(n == 2 & column == "X1", 'clock', key),
                  key = ifelse(n %in% c(3, 4) & column == "X1", 'clock', key),
                  key = ifelse(n %in% c(3, 4) & column == "X2", 'away', key),
                  key = ifelse(n %in% c(3, 4) & column == "X3", 'awaypts', key),
                  key = ifelse(n %in% c(3, 4) & column == "X4", 'score', key),
                  key = ifelse(n %in% c(3, 4) & column == "X5", 'homepts', key),
                  key = ifelse(n %in% c(3, 4) & column == "X6", 'home', key)) %>% 
    dplyr::select(cod, key, value) %>% 
    dplyr::distinct() %>% 
    tidyr::spread(key, value) %>% 
    dplyr::mutate(aux = ifelse(score == "Score", 1, 0),
                  aux = ifelse(is.na(aux), 0, aux)) %>% 
    dplyr::filter(aux == 0) %>% 
    dplyr::mutate(aux = ifelse(!is.na(period), 1, 0)) %>% 
    dplyr::mutate(aux = cumsum(aux)) %>% 
    dplyr::group_by(aux) %>% 
    dplyr::mutate(period = unique(na.omit(period))) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(!is.na(clock)) %>% 
    tidyr::gather("owner", "text", -c(cod, awaypts, clock, homepts, period, 
                                      score, aux)) %>% 
    dplyr::filter(!is.na(text)) %>% 
    dplyr::arrange(cod) %>% 
    tidyr::separate(period, c('period', 'period_type'), sep = "\\s+") %>% 
    dplyr::mutate(period = stringr::str_remove_all(period, "\\D")) %>% 
    dplyr::mutate(period = as.integer(period)) %>% 
    dplyr::mutate(period = ifelse(period_type == "Q", period, 4 + period)) %>% 
    dplyr::select(-c(aux, period_type)) %>% 
    dplyr::mutate(gameid = gameid) %>% 
    dplyr::select(gameid, cod, period, clock, score, owner, text, homepts, awaypts)
}

extract_br_person_id <- function(file){
  
  rvest::read_html(file) %>% 
    rvest::html_nodes("table") %>% 
    rvest::html_nodes("a") %>%
    plyr::ldply(.data = ., .fun = function(node){
      
      data.frame(name = node %>% rvest::html_text(), 
                 id = node %>% rvest::html_attr('href'),
                 stringsAsFactors = FALSE)
      
    }) %>% 
    dplyr::as_tibble() %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(type_person = stringr::str_replace(string = id, 
                                                     pattern = "(.+\\/)(.+?)(\\.html$)", 
                                                     replacement = "\\1")) %>% 
    dplyr::mutate(type_person = stringr::str_replace(string = id, 
                                                     pattern = "(\\/)(.+?)(\\/.+)", 
                                                     replacement = "\\2")) %>% 
    dplyr::mutate(person_id = stringr::str_replace(string = id, 
                                                   pattern = "(.+\\/)(.+\\/)(.+?)(\\.html$)", 
                                                   replacement = "\\3")) %>% 
    dplyr::select(person_id, type_person, name) %>% 
    dplyr::arrange(desc(name))
}

extract_br_teams <- function(file){
  
  rvest::read_html(file) %>% 
    rvest::html_table(header = FALSE) %>% 
    '[['(1) %>% 
    dplyr::mutate(cod = dplyr::row_number()) %>% 
    tidyr::gather("column", "value", -cod) %>% 
    dplyr::arrange(cod, column) %>% 
    dplyr::filter(value != "") %>% 
    dplyr::filter(cod == 2, column %in% c('X2', 'X6')) %>% 
    dplyr::pull(value)
}

extract_br_pbp <- function(file){
  
  df_person <- extract_br_person_id(file) %>% 
    dplyr::filter(name != "")
  
  list_teams <- extract_br_teams(file)
  
  pbp <- extract_br_pbp_df(file) %>% 
    dplyr::mutate(modtext = text)
  
  for(i in seq_len(nrow(df_person))){
    
    pbp <- pbp %>%
      dplyr::mutate(modtext = stringr::str_replace_all(string = modtext, 
                                                       pattern = df_person$name[i], 
                                                       replacement = paste0("[",df_person$type_person[i], "]")))
    
  }
  
  for(i in seq_len(length(list_teams))){
    
    pbp <- pbp %>%
      dplyr::mutate(modtext = stringr::str_replace_all(string = modtext, 
                                                       pattern = list_teams[i], 
                                                       replacement = "[teams]"))
    
  }
  
  pbp <- pbp %>% 
    dplyr::mutate(modtext = stringr::str_replace(modtext, ' from [0-9]+ ft', ' from [dist] ft'))
  
  return(pbp)
}

dataframe_br_cod_plays <- function(){
  
  data.frame(modtext =   c("[players] makes 2-pt dunk at rim",
                           "[players] makes 2-pt dunk from [dist] ft",
                           "[players] makes 2-pt dunk at rim (assist by [players])", 
                           "[players] makes 2-pt dunk from [dist] ft (assist by [players])",
                           "[players] makes 2-pt hook shot at rim",
                           "[players] makes 2-pt hook shot from [dist] ft",
                           "[players] makes 2-pt hook shot at rim (assist by [players])",
                           "[players] makes 2-pt hook shot from [dist] ft (assist by [players])", 
                           "[players] makes 2-pt layup at rim",  
                           "[players] makes 2-pt layup from [dist] ft", 
                           "[players] makes 2-pt layup at rim (assist by [players])",
                           "[players] makes 2-pt layup from [dist] ft (assist by [players])", 
                           "[players] makes 2-pt jump shot at rim",
                           "[players] makes 2-pt jump shot from [dist] ft", 
                           "[players] makes 2-pt jump shot at rim (assist by [players])",
                           "[players] makes 2-pt jump shot from [dist] ft (assist by [players])", 
                           "[players] makes 3-pt jump shot from [dist] ft", 
                           "[players] makes 3-pt jump shot from [dist] ft (assist by [players])",
                           "[players] makes 2-pt tip-in from [dist] ft", 
                           "[players] misses 2-pt dunk at rim",
                           "[players] misses 2-pt dunk from [dist] ft", 
                           "[players] misses 2-pt dunk at rim (block by [players])",
                           "[players] misses 2-pt dunk from [dist] ft (block by [players])", 
                           "[players] misses 2-pt hook shot at rim",
                           "[players] misses 2-pt hook shot from [dist] ft", 
                           "[players] misses 2-pt hook shot at rim (block by [players])", 
                           "[players] misses 2-pt hook shot from [dist] ft (block by [players])", 
                           "[players] misses 2-pt layup at rim",  
                           "[players] misses 2-pt layup from [dist] ft", 
                           "[players] misses 2-pt layup at rim (block by [players])", 
                           "[players] misses 2-pt layup from [dist] ft (block by [players])", 
                           "[players] misses 2-pt jump shot at rim", 
                           "[players] misses 2-pt jump shot from [dist] ft", 
                           "[players] misses 2-pt jump shot at rim (block by [players])",
                           "[players] misses 2-pt jump shot from [dist] ft (block by [players])", 
                           "[players] misses 3-pt jump shot from [dist] ft", 
                           "[players] misses 3-pt jump shot from [dist] ft (block by [players])", 
                           "[players] misses 2-pt tip-in from [dist] ft", 
                           "[players] makes free throw 1 of 1", 
                           "[players] misses free throw 1 of 1", 
                           "[players] makes free throw 1 of 2", 
                           "[players] misses free throw 1 of 2",
                           "misses free throw 1 of 2",
                           "[players] makes free throw 1 of 3", 
                           "[players] misses free throw 1 of 3", 
                           "[players] makes free throw 2 of 2", 
                           "[players] misses free throw 2 of 2", 
                           "[players] makes free throw 2 of 3", 
                           "[players] misses free throw 2 of 3", 
                           "[players] makes free throw 3 of 3", 
                           "[players] misses free throw 3 of 3",
                           "[players] makes technical free throw", 
                           "[players] misses technical free throw",
                           "[players] makes flagrant free throw 1 of 2", 
                           "[players] misses flagrant free throw 1 of 2", 
                           "[players] makes flagrant free throw 2 of 2", 
                           "[players] misses flagrant free throw 2 of 2", 
                           "[players] makes flagrant free throw 1 of 1",
                           "[players] makes clear path free throw 1 of 2",
                           "[players] misses clear path free throw 1 of 2", 
                           "[players] makes clear path free throw 2 of 2", 
                           "[players] misses clear path free throw 2 of 2", 
                           "[players] makes flagrant free throw 1 of 3", 
                           "[players] misses flagrant free throw 1 of 3",  
                           "[players] makes flagrant free throw 2 of 3", 
                           "[players] misses flagrant free throw 2 of 3", 
                           "[players] makes flagrant free throw 3 of 3", 
                           "[players] misses flagrant free throw 3 of 3", 
                           "Defensive rebound by [players]", 
                           "Offensive rebound by [players]", 
                           "Offensive rebound by Team",
                           "Defensive rebound by Team", 
                           "Turnover by  (turnover)", 
                           "Turnover by [players] (turnover)",
                           "Turnover by [players] (turnover; steal by [players])", 
                           "Turnover by [players] (bad pass)", 
                           "Turnover by [players] (bad pass; steal by [players])", 
                           "Turnover by [players] (traveling)",
                           "Turnover by [players] (offensive foul)", 
                           "Turnover by [players] (dbl dribble)", 
                           "Turnover by [players] (discontinued dribble)", 
                           "Turnover by [players] (3 sec)", 
                           "Turnover by [players] (inbound)", 
                           "Turnover by [players] (back court)", 
                           "Turnover by [players] (off goaltending)", 
                           "Turnover by [players] (lane violation)", 
                           "Turnover by [players] (illegal assist)",
                           "Turnover by [players] (palming)", 
                           "Turnover by [players] (punched ball)", 
                           "Turnover by [players] (step out of bounds)", 
                           "Turnover by [players] (out of bounds lost ball)", 
                           "Turnover by [players] (lost ball)", 
                           "Turnover by [players] (lost ball; steal by [players])", 
                           "Turnover by  (8 sec)", 
                           "Turnover by Team (shot clock)", 
                           "Turnover by  (5 sec)", 
                           "Turnover by [players] (5 sec)", 
                           "Technical foul by [players]", 
                           "Def 3 sec tech foul by Team", 
                           "Technical foul by [coaches]", 
                           "Technical foul by",
                           "Shooting foul by [players] (drawn by [players])", 
                           "Shooting foul by [players]", 
                           "Personal foul by [players] (drawn by [players])",
                           "Personal foul by [players]", 
                           "Offensive foul by [players] (drawn by [players])", 
                           "Offensive foul by", 
                           "Loose ball foul by [players] (drawn by [players])", 
                           "Personal take foul by [players] (drawn by [players])", 
                           "Flagrant foul type 1 by [players] (drawn by [players])", 
                           "Away from play foul by [players] (drawn by [players])",
                           "Clear path foul by [players]", 
                           "Flagrant foul type 2 by [players] (drawn by [players])", 
                           "Violation by Team (violation)", 
                           "Violation by Team (delay of game)", 
                           "Violation by Team (def goaltending)",
                           "Violation by Team (jump ball)", 
                           "Violation by [players] (jump ball)", 
                           "Violation by Team (kicked ball)", 
                           "Violation by [players] (double lane)", 
                           "[players] enters the game for [players]", 
                           "[teams] full timeout", 
                           "Jump ball: [players] vs. [players] ([players] gains possession)", 
                           "Jump ball:  vs.", 
                           "[players] ejected from game", 
                           "[coaches] ejected from game", 
                           "ejected from game", 
                           "Start of 1st quarter", 
                           "Start of 2nd quarter", 
                           "Start of 3rd quarter", 
                           "Start of 4th quarter", 
                           "Start of 1st overtime", 
                           "Start of 2nd overtime", 
                           "Start of 3rd overtime", 
                           "Start of 4th overtime", 
                           "End of 1st quarter", 
                           "End of 2nd quarter", 
                           "End of 3rd quarter", 
                           "End of 4th quarter", 
                           "End of 1st overtime", 
                           "End of 2nd overtime", 
                           "End of 3rd overtime", 
                           "End of 4th overtime", 
                           "Instant Replay (Request: Ruling Stands)", 
                           "Instant Replay (Challenge: Ruling Stands)", 
                           "Instant Replay (Challenge: Stands)", 
                           "Instant Replay (Request: Stands)", 
                           "Instant Replay"),
             cod =   c("_1(1)", "_1(1)", "_1(1a)", "_1(1a)", 
                       "_1(2)", "_1(2)", "_1(2a)", "_1(2a)", 
                       "_1(3)", "_1(3)", "_1(3a)", "_1(3a)", 
                       "_1(4)", "_1(4)", "_1(4a)", "_1(4a)", 
                       "_1(5)", "_1(5a)", 
                       "_1(3)", 
                       "_2(1)", "_2(1)", "_2(1b)", "_2(1b)", 
                       "_2(2)", "_2(2)", "_2(2b)", "_2(2b)", 
                       "_2(3)", "_2(3)", "_2(3b)", "_2(3b)", 
                       "_2(4)", "_2(4)", "_2(4b)", "_2(4b)", 
                       "_2(5)", "_2(5b)", 
                       "_2(3)", 
                       "_3(11c)", "_3(11w)", 
                       "_3(12c)", "_3(12w)", "_3(12w)", 
                       "_3(13c)", "_3(13w)", 
                       "_3(22c)", "_3(22w)", 
                       "_3(23c)", "_3(23w)",
                       "_3(33c)", "_3(33w)", 
                       "_3(40c)", "_3(40w)", 
                       "_3(51c)", "_3(51w)", 
                       "_3(52c)", "_3(52w)", 
                       "_3(61c)", 
                       "_3(71c)", "_3(71w)", 
                       "_3(72c)", "_3(72w)", 
                       "_3(81c)", "_3(81w)", 
                       "_3(82c)", "_3(82w)", 
                       "_3(83c)", "_3(83w)", 
                       "_4(0p)", "_4(0p)", 
                       "_4(0t)", "_4(0t)", 
                       "_5(00p)", "_5(00p)", 
                       "_5(01p)", 
                       "_5(34p)", "_5(11p)", 
                       "_5(12p)", 
                       "_5(13p)", 
                       "_5(14p)", 
                       "_5(15p)", 
                       "_5(16p)", 
                       "_5(17p)", 
                       "_5(18p)", 
                       "_5(19p)", 
                       "_5(20p)", 
                       "_5(23p)", 
                       "_5(24p)", 
                       "_5(27p)", 
                       "_5(31p)", 
                       "_5(32p)", 
                       "_5(33p)", 
                       "_5(35p)", 
                       "_5(36t)", 
                       "_5(37t)", 
                       "_5(38t)", 
                       "_5(41p)", 
                       "_6(01ap)", "_6(05at)", "_6(01ac)", "_6(01at)", 
                       "_6(11a)", "_6(11a)", 
                       "_6(10a)", "_6(10a)", 
                       "_6(13a)", "_6(13a)", 
                       "_6(12a)", 
                       "_6(21a)", 
                       "_6(19a)", 
                       "_6(15a)", 
                       "_6(17a)", 
                       "_6(20a)", 
                       "_7(0t)", 
                       "_7(1t)", 
                       "_7(2p)", 
                       "_7(4p)", "_7(4p)", 
                       "_7(5p)", 
                       "_7(6p)", 
                       "_8(0)", 
                       "_9(0)", 
                       "_10(1)", "_10(1)", 
                       "_11(0)", "_11(1)", "_11(0)", 
                       "_12(1)", "_12(2)", "_12(3)", "_12(4)", "_12(0)", "_12(0)", "_12(0)", "_12(0)", 
                       "_13(1)", "_13(2)", "_13(3)", "_13(4)", "_13(0)", "_13(0)", "_13(0)", "_13(0)", 
                       "_18(3)", "_18(6)", "_18(6)", "_18(3)", "_18(0)"),
             tp_event =  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                           2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                           3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                           3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5,
                           5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
                           5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
                           7, 7, 7, 7, 7, 7, 7, 8, 9, 10, 10, 11, 11, 11, 12, 12, 12,
                           12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 18,
                           18, 18, 18, 18))
  
  
  
  
  
  
  
}

write_br_pbp_rdata <- function(p){
  
  tryCatch(expr = {extract_br_pbp(p)}, 
           error = data.frame())
  
}

extract_espn_pbp <- function(file){

  readr::read_file(file) %>%
    stringr::str_extract(string = ., pattern = "sbpg:playbyplay\\,pbp\\:\\{playGrps:\\[\\[(.+?)\\]\\]\\,tms") %>% 
    stringr::str_remove_all(string = ., pattern = "sbpg\\:playbyplay\\,pbp\\:\\{playGrps\\:\\[\\[") %>% 
    stringr::str_remove_all(string = ., pattern = "\\]\\]\\,tms") %>% 
    stringr::str_replace_all(string = ., pattern = "(\\})(\\],\\[)(\\{)", "\\1;\\3") %>% 
    stringr::str_replace_all(string = ., pattern = "(\\})(\\,)(\\{)", "\\1;\\3") %>% 
    stringr::str_replace_all(string = ., 
                             pattern = "(period)(\\:\\{)(number\\:[0-9]{1}\\,)(displayValue)(\\:(.+?))(\\})", 
                             replacement = "\\1_\\3desc_period\\5") %>% 
    stringr::str_replace_all(string = ., 
                             pattern = "(clock)(\\:)(\\{displayValue)(\\:(.+?))(\\})", 
                             replacement = "\\1\\4") %>% 
    stringr::str_remove_all(string = ., pattern = "\\{|\\}") %>% 
    stringr::str_split(string = ., pattern = ";") %>% 
    unlist() %>% 
    data.frame(doc = ., stringsAsFactors = FALSE) %>% 
    dplyr::as_tibble() %>%
    dplyr::mutate(seq_id = dplyr::row_number()) %>% 
    dplyr::mutate(playid = stringr::str_extract(doc, "^id\\:[0-9]+")) %>%
    dplyr::mutate(doc = stringr::str_replace_all(doc, "\\,\\s+", " ")) %>% 
    dplyr::mutate(cols = stringr::str_split(doc, pattern = "\\,")) %>% 
    tidyr::unnest(cols) %>% 
    dplyr::select(-doc) %>% 
    tidyr::separate(data = ., col = "cols", into = c("key", "value"), sep = "\\:", extra = "merge", fill = "right") %>% 
    dplyr::distinct() %>% 
    tidyr::spread(key, value) %>% 
    dplyr::select(-playid) %>% 
    dplyr::mutate(gameid = stringr::str_replace(string = file, pattern = '(.+?)([0-9]+)(\\.txt$)', repl = "\\2")) 
  
  #%>% 
  #  dplyr::select(gameid, id, period_number, desc_period, clock, text, homeAway, 
  #                homeScore, awayScore, scoringPlay)
}

write_espn_pbp_rdata <- function(p){
  
  tryCatch(expr = {extract_espn_pbp(p)}, 
           error = data.frame())
  
}

dir("D:/Mestrado/NBA/data/dados_crawler/paginas_html_br", full.names = TRUE) %>%
  data.frame(path = ., stringsAsFactors = FALSE) %>%
  dplyr::as_tibble() %>%
  dplyr::distinct() %>%
  dplyr::mutate(aux = rnorm(nrow(.))) %>%
  dplyr::arrange(aux) %>%
  dplyr::mutate(row = dplyr::row_number()) %>%
  dplyr::mutate(row = row %/% 2000) %>%
  dplyr::select(-aux) %>%
  dplyr::group_split(row) %>%
  plyr::l_ply(.data = ., .fun = function(df){

    future::plan(future::multisession(),
                 workers = future::availableCores())


    xdf <- df$path %>%
      furrr::future_map(.x = ., .f = write_br_pbp_rdata, .progress = TRUE) %>%
      dplyr::bind_rows()

    filename <- Sys.time() %>%
      stringr::str_remove_all(., "\\D") %>%
      paste0("D:/Mestrado/NBA/data/dados_crawler/br/parcial/", ., ".RData")

    save(xdf, file = filename)

  }, .progress = 'time')

df_pbpbr <- dir("D:/Mestrado/NBA/data/dados_crawler/br/parcial", full.names = TRUE) %>% 
  plyr::ldply(.data = ., .fun = function(path){
    
    xx <- load(path)
    xx <- get(xx)
    
    return(xx)
  },  .progress = 'time') %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(modtext = ifelse(stringr::str_detect(modtext, "S.+Pippen"),
                                 stringr::str_replace(modtext, "S.+Pippen", "[players]"), modtext)) %>% 
  dplyr::mutate(gameid = stringr::str_replace(gameid, "PHO$", "PHX"),
                gameid = stringr::str_replace(gameid, "BRK$", "BKN"),
                gameid = stringr::str_replace(gameid, "CHO$", "CHA")) %>% 
  dplyr::rename(n = cod) %>% 
  dplyr::distinct(gameid, period, n, .keep_all = TRUE) %>% 
  dplyr::left_join(dataframe_br_cod_plays(), by = 'modtext')

df_time <- df_pbpbr %>% 
  dplyr::distinct(clock) %>% 
  tidyr::separate(clock, c('min', 'sec', 'dec'), sep = '\\:|\\.', remove = FALSE) %>% 
  dplyr::mutate_at(.vars = c('min', 'sec', 'dec'), .funs = as.integer) %>% 
  dplyr::mutate(t = 60 * min + sec + (dec/10)) %>% 
  dplyr::arrange(desc(t)) %>% 
  dplyr::select(clock, t)

df_pbpbr <- df_pbpbr %>% 
  dplyr::left_join(df_time, by = 'clock') %>% 
  dplyr::arrange(gameid, period, desc(t)) %>% 
  dplyr::group_by(gameid) %>% 
  dplyr::mutate(n = dplyr::row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-t)

list_pbp_br <- match_jogos_nba %>% 
  dplyr::group_split(SEASON) %>% 
  plyr::llply(.data = ., .fun = function(df){
    
    df %>% 
      dplyr::select(BR_ID) %>% 
      dplyr::inner_join(y = df_pbpbr, by = c('BR_ID' = 'gameid'))
    
  }, .progress = 'time')

pbpbr_2008_09 <- list_pbp_br[[1]]
save(pbpbr_2008_09, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2008_09.RData")
rm(pbpbr_2008_09)

pbpbr_2009_10 <- list_pbp_br[[2]]
save(pbpbr_2009_10, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2009_10.RData")
rm(pbpbr_2009_10)

pbpbr_2010_11 <- list_pbp_br[[3]]
save(pbpbr_2010_11, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2010_11.RData")
rm(pbpbr_2010_11)

pbpbr_2011_12 <- list_pbp_br[[4]]
save(pbpbr_2011_12, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2011_12.RData")
rm(pbpbr_2011_12)

pbpbr_2012_13 <- list_pbp_br[[5]]
save(pbpbr_2012_13, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2012_13.RData")
rm(pbpbr_2012_13)

pbpbr_2013_14 <- list_pbp_br[[6]]
save(pbpbr_2013_14, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2013_14.RData")
rm(pbpbr_2013_14)

pbpbr_2014_15 <- list_pbp_br[[7]]
save(pbpbr_2014_15, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2014_15.RData")
rm(pbpbr_2014_15)

pbpbr_2015_16 <- list_pbp_br[[8]]
save(pbpbr_2015_16, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2015_16.RData")
rm(pbpbr_2015_16)

pbpbr_2016_17 <- list_pbp_br[[9]]
save(pbpbr_2016_17, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2016_17.RData")
rm(pbpbr_2016_17)

pbpbr_2017_18 <- list_pbp_br[[10]]
save(pbpbr_2017_18, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2017_18.RData")
rm(pbpbr_2017_18)

pbpbr_2018_19 <- list_pbp_br[[11]]
save(pbpbr_2018_19, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2018_19.RData")
rm(pbpbr_2018_19)

pbpbr_2019_20 <- list_pbp_br[[12]]
save(pbpbr_2019_20, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2019_20.RData")
rm(pbpbr_2019_20)

pbpbr_2020_21 <- list_pbp_br[[13]]
save(pbpbr_2020_21, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2020_21.RData")
rm(pbpbr_2020_21)

pbpbr_2021_22 <- list_pbp_br[[14]]
save(pbpbr_2021_22, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2021_22.RData")
rm(pbpbr_2021_22)

pbpbr_2022_23 <- list_pbp_br[[15]]
save(pbpbr_2022_23, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2022_23.RData")
rm(pbpbr_2022_23)

pbpbr_2023_24 <- list_pbp_br[[16]]
save(pbpbr_2023_24, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_2023_24.RData")
rm(pbpbr_2023_24)


pbpbr_total <- dplyr::bind_rows(list_pbp_br) 
save(pbpbr_total, file = "D:/Mestrado/NBA/data/dados_crawler/br/pbpbr_total.RData")
rm(pbpbr_total)

load("D:/Mestrado/NBA/match_jogos_nba.RData")

dir("D:/Mestrado/NBA/data/dados_crawler/paginas_html_espn", full.names = TRUE) %>%
  data.frame(path = ., stringsAsFactors = FALSE) %>%
  dplyr::as_tibble() %>%
  dplyr::distinct() %>%
  dplyr::mutate(aux = rnorm(nrow(.))) %>%
  dplyr::arrange(aux) %>%
  dplyr::mutate(row = dplyr::row_number()) %>%
  dplyr::mutate(row = row %/% 2000) %>%
  dplyr::select(-aux) %>%
  dplyr::group_split(row) %>%
  plyr::l_ply(.data = ., .fun = function(df){
    
    future::plan(future::multisession(),
                 workers = future::availableCores())
    
    
    xdf <- df$path %>%
      furrr::future_map(.x = ., .f = write_espn_pbp_rdata, .progress = TRUE) %>%
      dplyr::bind_rows()
    
    filename <- Sys.time() %>%
      stringr::str_remove_all(., "\\D") %>%
      paste0("D:/Mestrado/NBA/data/dados_crawler/espn/parcial/", ., ".RData")
    
    save(xdf, file = filename)
    
  }, .progress = 'time')

df_pbpespn <- dir("D:/Mestrado/NBA/data/dados_crawler/espn/parcial", full.names = TRUE) %>% 
  plyr::ldply(.data = ., .fun = function(path){
    
    xx <- load(path)
    xx <- get(xx)
    
    return(xx)
  },  .progress = 'time') %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(gameid, desc_period, seq_id, id, clock, homeAway, 
                text, scoringPlay, homeScore, awayScore)


list_pbp_espn <- match_jogos_nba %>% 
  dplyr::group_split(SEASON) %>% 
  plyr::llply(.data = ., .fun = function(df){
    
    df %>% 
      dplyr::select(ESPN_ID, SEASON) %>%
      dplyr::inner_join(df_pbpespn, 
                        by = c('ESPN_ID' = 'gameid'))
    
  }, .progress = 'time')

pbpespn_2008_09 <- list_pbp_espn[[1]]
save(pbpespn_2008_09, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2008_09.RData")
rm(pbpespn_2008_09)

pbpespn_2009_10 <- list_pbp_espn[[2]]
save(pbpespn_2009_10, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2009_10.RData")
rm(pbpespn_2009_10)

pbpespn_2010_11 <- list_pbp_espn[[3]]
save(pbpespn_2010_11, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2010_11.RData")
rm(pbpespn_2010_11)

pbpespn_2011_12 <- list_pbp_espn[[4]]
save(pbpespn_2011_12, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2011_12.RData")
rm(pbpespn_2011_12)

pbpespn_2012_13 <- list_pbp_espn[[5]]
save(pbpespn_2012_13, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2012_13.RData")
rm(pbpespn_2012_13)

pbpespn_2013_14 <- list_pbp_espn[[6]]
save(pbpespn_2013_14, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2013_14.RData")
rm(pbpespn_2013_14)

pbpespn_2014_15 <- list_pbp_espn[[7]]
save(pbpespn_2014_15, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2014_15.RData")
rm(pbpespn_2014_15)

pbpespn_2015_16 <- list_pbp_espn[[8]]
save(pbpespn_2015_16, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2015_16.RData")
rm(pbpespn_2015_16)

pbpespn_2016_17 <- list_pbp_espn[[9]]
save(pbpespn_2016_17, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2016_17.RData")
rm(pbpespn_2016_17)

pbpespn_2017_18 <- list_pbp_espn[[10]]
save(pbpespn_2017_18, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2017_18.RData")
rm(pbpespn_2017_18)

pbpespn_2018_19 <- list_pbp_espn[[11]]
save(pbpespn_2018_19, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2018_19.RData")
rm(pbpespn_2018_19)

pbpespn_2019_20 <- list_pbp_espn[[12]]
save(pbpespn_2019_20, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2019_20.RData")
rm(pbpespn_2019_20)

pbpespn_2020_21 <- list_pbp_espn[[13]]
save(pbpespn_2020_21, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2020_21.RData")
rm(pbpespn_2020_21)

pbpespn_2021_22 <- list_pbp_espn[[14]]
save(pbpespn_2021_22, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2021_22.RData")
rm(pbpespn_2021_22)

pbpespn_2022_23 <- list_pbp_espn[[15]]
save(pbpespn_2022_23, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2022_23.RData")
rm(pbpespn_2022_23)

pbpespn_2023_24 <- list_pbp_espn[[16]]
save(pbpespn_2023_24, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_2023_24.RData")
rm(pbpespn_2023_24)


pbpespn_total <- dplyr::bind_rows(list_pbp_espn) 
save(pbpespn_total, file = "D:/Mestrado/NBA/data/dados_crawler/espn/pbpespn_total.RData")
rm(pbpespn_total)

xdf <- df_pbpespn %>% 
  dplyr::mutate(text = stringr::str_trim(text)) %>% 
  dplyr::group_by(text) %>% 
  dplyr::summarise(n = dplyr::n())

xdf1 <- xdf %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::mutate(sintaxe = 0) %>% 
  dplyr::mutate(modtext = text) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+offensive\\s+rebound$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+offensive\\s+rebound$", "[player] offensive rebound"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+offensive\\s+team\\s+rebound$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+offensive\\s+team\\s+rebound$", "[team] offensive team rebound"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+defensive\\s+team\\s+rebound$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+defensive\\s+team\\s+rebound$", "[team] defensive team rebound"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+Full\\s+timeout$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+Full\\s+timeout$", "[team] Full timeout"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+full\\s+timeout$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+full\\s+timeout$", "[team] full timeout"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+20\\s+Sec\\.\\s+timeout$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+20\\s+Sec\\.\\s+timeout$", "[team] 20 Sec. timeout"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+free\\s+throw\\s+1\\s+of\\s+1$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+free\\s+throw\\s+1\\s+of\\s+1$", "[player] makes free throw 1 of 1"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+free\\s+throw\\s+1\\s+of\\s+2$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+free\\s+throw\\s+1\\s+of\\s+2$", "[player] makes free throw 1 of 2"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+free\\s+throw\\s+2\\s+of\\s+2$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+free\\s+throw\\s+2\\s+of\\s+2$", "[player] makes free throw 2 of 2"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+free\\s+throw\\s+1\\s+of\\s+3$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+free\\s+throw\\s+1\\s+of\\s+3$", "[player] makes free throw 1 of 3"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+free\\s+throw\\s+2\\s+of\\s+3$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+free\\s+throw\\s+2\\s+of\\s+3$", "[player] makes free throw 2 of 3"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+free\\s+throw\\s+3\\s+of\\s+3$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+free\\s+throw\\s+3\\s+of\\s+3$", "[player] makes free throw 3 of 3"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+technical\\s+free\\s+throw$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+technical\\s+free\\s+throw$", "[player] makes technical free throw"), modtext)) %>%
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+free\\s+throw\\s+1\\s+of\\s+1$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+free\\s+throw\\s+1\\s+of\\s+1$", "[player] misses free throw 1 of 1"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+free\\s+throw\\s+1\\s+of\\s+2$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+free\\s+throw\\s+1\\s+of\\s+2$", "[player] misses free throw 1 of 2"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+free\\s+throw\\s+2\\s+of\\s+2$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+free\\s+throw\\s+2\\s+of\\s+2$", "[player] misses free throw 2 of 2"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+free\\s+throw\\s+1\\s+of\\s+3$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+free\\s+throw\\s+1\\s+of\\s+3$", "[player] misses free throw 1 of 3"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+free\\s+throw\\s+2\\s+of\\s+3$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+free\\s+throw\\s+2\\s+of\\s+3$", "[player] misses free throw 2 of 3"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+free\\s+throw\\s+3\\s+of\\s+3$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+free\\s+throw\\s+3\\s+of\\s+3$", "[player] misses free throw 3 of 3"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+technical\\s+free\\s+throw$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+technical\\s+free\\s+throw$", "[player] misses technical free throw"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+defensive\\s+rebound$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+defensive\\s+rebound$", "[player] defensive rebound"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+shooting\\s+foul$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+shooting\\s+foul$", "[player] shooting foul"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+personal\\s+foul$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+personal\\s+foul$", "[player] personal foul"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+enters\\s+the\\s+game\\s+for\\s+(.+?)$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+enters\\s+the\\s+game\\s+for\\s+(.+?)$", "[player] enters the game for [player]"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+driving\\s+layup$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+driving\\s+layup$", "[player] makes driving layup"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+layup$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+layup$", "[player] makes layup"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+two\\s+point\\s+shot$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+two\\s+point\\s+shot$", "[player] makes two point shot"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+[0-9]+\\-foot\\s+three\\s+point\\s+jumper$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+[0-9]+\\-foot\\s+three\\s+point\\s+jumper$", "[player] makes [dist]-foot three point jumper"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+[0-9]+\\-foot\\s+step\\s+back\\s+jumpshot$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+[0-9]+\\-foot\\s+step\\s+back\\s+jumpshot$", "[player] makes [dist]-foot step back jumpshot"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+driving\\s+floating\\s+jump\\s+shot$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+driving\\s+floating\\s+jump\\s+shot$", "[player] makes driving floating jump shot"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+driving\\s+dunk$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+driving\\s+dunk$", "[player] makes driving dunk"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+tip\\s+shot$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+tip\\s+shot$", "[player] makes tip shot"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+[0-9]+\\-foot\\s+jumper$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+[0-9]+\\-foot\\s+jumper$", "[player] makes [dist]-foot jumper"), modtext)) %>% 
  
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+driving\\s+layup$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+driving\\s+layup$", "[player] misses driving layup"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+layup$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+layup$", "[player] misses layup"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+two\\s+point\\s+shot$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+two\\s+point\\s+shot$", "[player] misses two point shot"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+[0-9]+\\-foot\\s+three\\s+point\\s+jumper$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+[0-9]+\\-foot\\s+three\\s+point\\s+jumper$", "[player] misses [dist]-foot three point jumper"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+[0-9]+\\-foot\\s+step\\s+back\\s+jumpshot$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+[0-9]+\\-foot\\s+step\\s+back\\s+jumpshot$", "[player] misses [dist]-foot step back jumpshot"), modtext)) %>%
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+driving\\s+floating\\s+jump\\s+shot$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+driving\\s+floating\\s+jump\\s+shot$", "[player] misses driving floating jump shot"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+driving\\s+dunk$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+driving\\s+dunk$", "[player] misses driving dunk"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+tip\\s+shot$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+tip\\s+shot$", "[player] misses tip shot"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+misses\\s+[0-9]+\\-foot\\s+jumper$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+misses\\s+[0-9]+\\-foot\\s+jumper$", "[player] misses [dist]-foot jumper"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+out\\s+of\\s+bounds\\s+bad\\s+pass\\s+turnover$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+out\\s+of\\s+bounds\\s+bad\\s+pass\\s+turnover$", "[player] out of bounds bad pass turnover"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+offensive\\s+foul$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+offensive\\s+foul$", "[player] offensive foul"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+makes\\s+dunk$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+dunk$", "[player] makes dunk"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+delay\\s+of\\s+game\\s+violation$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+delay\\s+of\\s+game\\s+violation$", "[team] delay of game violation"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(text, "^(.+?)\\s+loose\\s+ball\\s+foul$"), 1, sintaxe),
                modtext = ifelse(sintaxe == 1, stringr::str_replace(modtext, "^(.+?)\\s+loose\\s+ball\\s+foul$", "[player] loose ball foul"), modtext)) %>% 
  dplyr::mutate(sintaxe = ifelse(modtext %in% c("End of the 1st Quarter", "End of the 2nd Quarter", "End of the 3rd Quarter", "End of the 4th Quarter", 
                                                "Official timeout", "shot clock turnover", "End of Game", "Start of the 2nd Quarter", "Start of the 3rd Quarter", 
                                                "Start of the 4th Quarter", "Start of the 1st Quarter", "End Game", "shot clock violation", "Full timeout", 
                                                "full timeout", "End of the 1st  Overtime", "delay techfoul", "End of the 1st Overtime", "Start of the 1st Overtime",
                                                "turnover", "5 sec inbound turnover", "5 second violation", "8 second turnover", "back court turnover", "foul"), 1, sintaxe))

xdf1 %>% 
  dplyr::group_by(sintaxe) %>% 
  dplyr::summarise(n = sum(n), .groups = 'drop') %>% 
  dplyr::mutate(perc = 100 * n / sum(n)) %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100)

xdf1 %>% 
  dplyr::filter(sintaxe == 1) %>% 
  dplyr::group_by(modtext, sintaxe) %>% 
  dplyr::summarise(n = sum(n), .groups = 'drop') %>% 
  dplyr::arrange(modtext) %>% 
  print(., n = 100)

xdf1 %>% 
  dplyr::filter(sintaxe == 0) %>% 
  dplyr::group_by(modtext, sintaxe) %>% 
  dplyr::summarise(n = sum(n), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100)


xdf1 <- xdf %>% 
  dplyr::mutate(tp = 0,
                tp = ifelse(stringr::str_detect(text, "[Ff]ree [Tt]hrow"), 3, tp),
                tp = ifelse(stringr::str_detect(text, "\\s+makes") & tp == 0, 1, tp),
                tp = ifelse(stringr::str_detect(text, "\\s+misses") & tp == 0, 2, tp),
                tp = ifelse(stringr::str_detect(text, "makes\\s+") & tp == 0, 1, tp),
                tp = ifelse(stringr::str_detect(text, "blocks\\s+") & tp == 0, 2, tp),
                tp = ifelse(stringr::str_detect(text, "misses\\s+") & tp == 0, 2, tp),
                tp = ifelse(stringr::str_detect(text, "rebound"), 4, tp),
                tp = ifelse(stringr::str_detect(text, "turnover"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "Turnover"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "traveling"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "travelling"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "bad pass"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "backcourt"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "illegal defense"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "lost ball"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "offensive goaltending"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "discontinue dribble"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "double dribble"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "10\\s+second"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "8\\s+second"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "5\\s+second"), 5, tp),
                tp = ifelse(stringr::str_detect(text, "foul"), 6, tp),
                tp = ifelse(stringr::str_detect(text, "offensive charge"), 6, tp),
                tp = ifelse(stringr::str_detect(text, "3\\s+second$"), 6, tp),
                tp = ifelse(stringr::str_detect(text, "3\\-seconds"), 6, tp),
                tp = ifelse(stringr::str_detect(text, "inbound"), 6, tp),
                tp = ifelse(stringr::str_detect(text, "Technical"), 6, tp),
                tp = ifelse(stringr::str_detect(text, "violation"), 7, tp),
                tp = ifelse(stringr::str_detect(text, "defensive goaltending"), 7, tp),
                tp = ifelse(stringr::str_detect(text, "delay of game"), 7, tp),
                tp = ifelse(stringr::str_detect(text, "kicked ball"), 7, tp),
                tp = ifelse(stringr::str_detect(text, "enters the game for"), 8, tp),
                tp = ifelse(stringr::str_detect(text, "timeout"), 9, tp),
                tp = ifelse(stringr::str_detect(text, "Challenge"), 9, tp),
                tp = ifelse(stringr::str_detect(text, "jumpball"), 10, tp),
                tp = ifelse(stringr::str_detect(text, "jump ball"), 10, tp),
                tp = ifelse(stringr::str_detect(text, "vs\\."), 10, tp),
                tp = ifelse(stringr::str_detect(text, "ejected"), 11, tp),
                tp = ifelse(stringr::str_detect(text, "Start of"), 12, tp),
                tp = ifelse(stringr::str_detect(text, "End of"), 13, tp),
                tp = ifelse(stringr::str_detect(text, "End Game"), 13, tp),
                tp = ifelse(stringr::str_detect(text, "Instant"), 18, tp),
                tp = ifelse(stringr::str_detect(text, "REPLAY"), 18, tp)) %>% 
  dplyr::mutate(cod = "") %>% 
  dplyr::mutate(modtext = text) %>% 
  dplyr::mutate(cod = ifelse(tp == 1 & stringr::str_detect(modtext, "[Dd]unk"), '_1(1', cod),
                cod = ifelse(tp == 1 & stringr::str_detect(modtext, "[Hh]ook"), '_1(2', cod),
                cod = ifelse(tp == 1 & stringr::str_detect(modtext, "[Ll]ayup|[Tt]ip"), '_1(3', cod),
                cod = ifelse(tp == 1 & stringr::str_detect(modtext, "[Tt]hree|3\\-pt|3\\-point|3\\s+point"), '_1(5', cod),
                cod = ifelse(tp == 1 & cod == "", '_1(4', cod),
                cod = ifelse(tp == 1 & stringr::str_detect(modtext, '\\s+assists'), paste0(cod, 'a'), cod),
                cod = ifelse(tp == 1, paste0(cod, ')'), cod)) %>% 
  dplyr::mutate(cod = ifelse(tp == 2 & stringr::str_detect(modtext, "[Dd]unk"), '_2(1', cod),
                cod = ifelse(tp == 2 & stringr::str_detect(modtext, "[Hh]ook"), '_2(2', cod),
                cod = ifelse(tp == 2 & stringr::str_detect(modtext, "[Ll]ayup|[Tt]ip"), '_2(3', cod),
                cod = ifelse(tp == 2 & stringr::str_detect(modtext, "[Tt]hree|3\\-pt|3\\-point|3\\s+point"), '_2(5', cod),
                cod = ifelse(tp == 2 & cod == "", '_2(4', cod),
                cod = ifelse(tp == 2 & stringr::str_detect(modtext, '\\s+blocks'), paste0(cod, 'b'), cod),
                cod = ifelse(tp == 2, paste0(cod, ')'), cod)) %>% 
  dplyr::mutate(cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow 1 of 1"), '_3(11c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow 1 of 1"), '_3(11w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow 1 of 2"), '_3(12c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow 1 of 2"), '_3(12w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow 1 of 3"), '_3(13c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow 1 of 3"), '_3(13w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow 2 of 2"), '_3(22c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow 2 of 2"), '_3(22w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow 2 of 3"), '_3(23c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow 2 of 3"), '_3(23w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow 3 of 3"), '_3(33c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow 3 of 3"), '_3(33w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Tt]echnical [Ff]ree [Tt]hrow"), '_3(40c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Tt]echnical [Ff]ree [Tt]hrow"), '_3(40c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Tt]echincal [Ff]ree [Tt]hrow"), '_3(40c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Tt]echincal [Ff]ree [Tt]hrow"), '_3(40c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow [Ff]lagrant 1 of 2"), '_3(51c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow [Ff]lagrant 1 of 2"), '_3(51w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow [Ff]lagrant 2 of 2"), '_3(52c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow [Ff]lagrant 2 of 2"), '_3(52w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]lagrant [Ff]ree [Tt]hrow 1 of 2"), '_3(51c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]lagrant [Ff]ree [Tt]hrow 1 of 2"), '_3(51w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]lagrant [Ff]ree [Tt]hrow 2 of 2"), '_3(52c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]lagrant [Ff]ree [Tt]hrow 2 of 2"), '_3(52w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow [Ff]lagrant 1 of 1"), '_3(61c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow [Ff]lagrant 1 of 1"), '_3(61w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow [Cc]lear [Pp]ath 1 of 2"), '_3(71c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow [Cc]lear [Pp]ath 1 of 2"), '_3(71w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Cc]lear [Pp]ath [Ff]ree [Tt]hrow 1 of 2"), '_3(71c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Cc]lear [Pp]ath [Ff]ree [Tt]hrow 1 of 2"), '_3(71w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow [Cc]lear [Pp]ath 2 of 2"), '_3(72c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow [Cc]lear [Pp]ath 2 of 2"), '_3(72w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Cc]lear [Pp]ath [Ff]ree [Tt]hrow 2 of 2"), '_3(72c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Cc]lear [Pp]ath [Ff]ree [Tt]hrow 2 of 2"), '_3(72w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow [Ff]lagrant 1 of 3"), '_3(81c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow [Ff]lagrant 1 of 3"), '_3(81w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow [Ff]lagrant 2 of 3"), '_3(82c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow [Ff]lagrant 2 of 3"), '_3(82w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow [Ff]lagrant 3 of 3"), '_3(83c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow [Ff]lagrant 3 of 3"), '_3(83w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]lagrant [Ff]ree [Tt]hrow 1 of 3"), '_3(81c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]lagrant [Ff]ree [Tt]hrow 1 of 3"), '_3(81w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]lagrant [Ff]ree [Tt]hrow 2 of 3"), '_3(82c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]lagrant [Ff]ree [Tt]hrow 2 of 3"), '_3(82w)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "makes [Ff]lagrant [Ff]ree [Tt]hrow 3 of 3"), '_3(83c)', cod),
                cod = ifelse(tp == 3 & stringr::str_detect(modtext, "misses [Ff]lagrant [Ff]ree [Tt]hrow 3 of 3"), '_3(83w)', cod),
                cod = ifelse(tp == 3 & cod == "" & stringr::str_detect(modtext, "makes [Ff]ree [Tt]hrow"), '_3(00c)', cod),
                cod = ifelse(tp == 3 & cod == "" & stringr::str_detect(modtext, "misses [Ff]ree [Tt]hrow"), '_3(00w)', cod)) %>% 
  dplyr::mutate(cod = ifelse(tp == 4 & stringr::str_detect(text, "team rebound"), '_4(1t)', cod),
                cod = ifelse(tp == 4 & cod == "" & stringr::str_detect(text, "rebound"), '_4(0p)', cod)) %>% 
  dplyr::mutate(cod = ifelse(tp == 5 & stringr::str_detect(text, "[Bb]ad [Pp]ass") & stringr::str_detect(text, "\\s+steals"), '_5(11p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Tt]raveling|[Tt]ravelling"), '_5(12p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Dd]ouble [Dd]ribble"), '_5(14p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Dd]isc [Dd]ribble|[Dd]iscontinue [Dd]ribble"), '_5(15p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "3\\s+[Ss]econd\\s+[Tt]urnover"), '_5(16p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Ii]nbound"), '_5(17p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "5\\s+[Ss]econd"), '_5(17p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Bb]ackcourt|[Bb]ack [Cc]ourt"), '_5(18p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Oo]ffensive\\s+[Gg]oaltending"), '_5(19p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Ii]llegal\\s+[Aa]ssist"), '_5(23p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Pp]alming"), '_5(24p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Pp]unched\\s+[Bb]all"), '_5(27p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Ss]teps\\s+[Oo]ut\\s+[Oo]f\\s+[Bb]ounds\\s+[Tt]urnover|[Ss]tep\\s+[Oo]ut\\s+[Oo]f\\s+[Bb]ounds\\s+[Tt]urnover"), '_5(31p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Oo]ut\\s+[Oo]f\\s+[Bb]ounds\\s+[Ll]ost\\s+[Bb]all"), '_5(32p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Pp]ossession\\s+[Ll]ost\\s+[Bb]all|[Pp]ost\\s+[Ll]ost\\s+[Bb]all"), '_5(33p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Bb]ad\\s+[Pp]ass") & cod == "", '_5(34p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Oo]ut\\s+[Oo]f\\s+[Bb]ounds\\s+[Bb]ad\\s+[Pp]ass"), '_5(34p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Ll]ost\\s+[Bb]all") & stringr::str_detect(text, "\\s+steals"), '_5(35p)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "8\\s+[Ss]econd"), '_5(36t)', cod),
                cod = ifelse(tp == 5 & stringr::str_detect(text, "[Ss]hot [Cc]lock"), '_5(37t)', cod),
                cod = ifelse(tp == 5 & cod == "", '_5(00p)', cod)) %>% 
  dplyr::group_split(tp)

xdf2 <- xdf1[[7]] %>% 
  dplyr::mutate(cod = ifelse(tp == 6 & stringr::str_detect(text, "personal foul"), "_6(10a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "technical foul"), "_6(01a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "3\\-second"), "_6(05a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "defensive 3\\-seconds"), "_6(05a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "shooting foul"), "_6(11a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "shooting block foul"), "_6(11a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "loose ball foul"), "_6(12a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "offensive foul"), "_6(13a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "offensive charge"), "_6(13a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "clear path foul"), "_6(17a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "flagrant foul type 1"), "_6(19a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "flagrant foul type 2"), "_6(20a)", cod),
                cod = ifelse(tp == 6 & stringr::str_detect(text, "personal take foul"), "_6(21a)", cod),
                )
  


xdf2 %>% 
  dplyr::group_split(cod)

xdf2 %>% 
  dplyr::filter(cod == "") %>% 
  dplyr::mutate(modtext = ifelse(tp == 1, stringr::str_replace(modtext, "[0-9]+\\-foot", "[dist]-foot"), modtext),
                modtext = ifelse(tp == 1, stringr::str_replace(modtext, "^(.+?)\\s+makes\\s+", "[player] makes "), modtext),
                modtext = ifelse(tp == 1, stringr::str_replace(modtext, "(.+?)\\s+\\((.+?)\\s+assists\\)", "\\1 ([player] assists)"), modtext),
                modtext = ifelse(tp == 1, stringr::str_replace(modtext, "(.+?)\\s+\\((.+?)\\s+assists\\s+\\)", "\\1 ([player] assists )"), modtext)) %>% 
  dplyr::group_by(modtext) %>% 
  dplyr::summarise(n = sum(n)) %>% 
  dplyr::arrange(modtext) %>% 
  print(., n = 300)

xdf2 %>% 
  dplyr::arrange(modtext) %>% 
  print(., n = 500)

dplyr::mutate(modtext = ifelse(tp == 1, stringr::str_replace(modtext, "three point jumper|driving floating jump shot|hook shot|three pointer|two point shot|driving layup|tip shot|step back jumpshot|pullup jump shot|dunk|layup|jumper", '[shot]'), modtext)) %>% 
xdf1 %>% 
  dplyr::filter(tp == 0) %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100)

xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "dunk"), stringr::str_detect(text, "assists", negate = TRUE))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "dunk"), stringr::str_detect(text, "assists"))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "hook"), stringr::str_detect(text, "assists", negate = TRUE))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "hook"), stringr::str_detect(text, "assists"))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "layup"), stringr::str_detect(text, "assists", negate = TRUE))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "layup"), stringr::str_detect(text, "assists"))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "three"), stringr::str_detect(text, "assists", negate = TRUE))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "three"), stringr::str_detect(text, "assists"))

xdf %>% dplyr::filter(stringr::str_detect(text, "misses"), stringr::str_detect(text, "dunk"))
xdf %>% dplyr::filter(stringr::str_detect(text, "blocks"), stringr::str_detect(text, "dunk"))
xdf %>% dplyr::filter(stringr::str_detect(text, "misses"), stringr::str_detect(text, "hook"))
xdf %>% dplyr::filter(stringr::str_detect(text, "blocks"), stringr::str_detect(text, "hook"))
xdf %>% dplyr::filter(stringr::str_detect(text, "misses"), stringr::str_detect(text, "layup"))
xdf %>% dplyr::filter(stringr::str_detect(text, "blocks"), stringr::str_detect(text, "layup"))
xdf %>% dplyr::filter(stringr::str_detect(text, "misses"), stringr::str_detect(text, "three"))
xdf %>% dplyr::filter(stringr::str_detect(text, "blocks"), stringr::str_detect(text, "three"))

xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "free throw 1 of 1"))
xdf %>% dplyr::filter(stringr::str_detect(text, "misses"), stringr::str_detect(text, "free throw 1 of 1"))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "free throw 1 of 2"))
xdf %>% dplyr::filter(stringr::str_detect(text, "misses"), stringr::str_detect(text, "free throw 1 of 2"))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "free throw 1 of 3"))
xdf %>% dplyr::filter(stringr::str_detect(text, "misses"), stringr::str_detect(text, "free throw 1 of 3"))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "free throw 2 of 2"))
xdf %>% dplyr::filter(stringr::str_detect(text, "misses"), stringr::str_detect(text, "free throw 2 of 2"))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "free throw 2 of 3"))
xdf %>% dplyr::filter(stringr::str_detect(text, "misses"), stringr::str_detect(text, "free throw 2 of 3"))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "free throw 3 of 3"))
xdf %>% dplyr::filter(stringr::str_detect(text, "misses"), stringr::str_detect(text, "free throw 3 of 3"))
xdf %>% dplyr::filter(stringr::str_detect(text, "makes"), stringr::str_detect(text, "technical free throw"))
xdf %>% dplyr::filter(stringr::str_detect(text, "misses"), stringr::str_detect(text, "technical free throw"))

xdf %>% dplyr::filter(stringr::str_detect(text, "team rebound"))
xdf %>% dplyr::filter(stringr::str_detect(text, "team rebound", negate = T), stringr::str_detect(text, "rebound"))

xdf %>% dplyr::filter(stringr::str_detect(text, "turnover"))
xdf %>% dplyr::filter(stringr::str_detect(text, "bad pass"))
xdf %>% dplyr::filter(stringr::str_detect(text, "travelling"))

xdf %>% dplyr::filter(stringr::str_detect(text, "personal foul"))
xdf %>% dplyr::filter(stringr::str_detect(text, "shooting foul"))
xdf %>% dplyr::filter(stringr::str_detect(text, "loose ball foul"))
xdf %>% dplyr::filter(stringr::str_detect(text, "offensive foul"))
xdf %>% dplyr::filter(stringr::str_detect(text, "foul"))

xdf %>% dplyr::filter(stringr::str_detect(text, "violation"))

xdf %>% dplyr::filter(stringr::str_detect(text, "enters the game for"))

xdf %>% dplyr::filter(stringr::str_detect(text, "timeout"))

xdf %>% dplyr::filter(stringr::str_detect(text, "Jump ball"))
xdf %>% dplyr::filter(stringr::str_detect(text, "Jumpball"))

xdf %>% dplyr::filter(stringr::str_detect(text, "ejected"))

xdf %>% dplyr::filter(stringr::str_detect(text, "Start of"))

xdf %>% dplyr::filter(stringr::str_detect(text, "End of"))

xdf %>% dplyr::filter(stringr::str_detect(text, "Instant"))
xdf %>% dplyr::filter(stringr::str_detect(text, "Replay"))




