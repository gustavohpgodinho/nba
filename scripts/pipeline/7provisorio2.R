
# git token
#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

require(tidyverse)
load("D:/Mestrado/NBA/nba/data/pbp.RData")

put_features_cod <- function(df){
  
  df %>% 
    dplyr::mutate(fullcod = paste0(cod, "[", row, ",", importance, "]")) %>% 
    dplyr::group_by(joincod) %>%
    dplyr::mutate(fullcod = paste0(fullcod, collapse = '')) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(before_cod = stringr::str_extract(fullcod, paste0("^.+\\[", row, "\\,.\\]")),
                  before_cod = stringr::str_remove(before_cod, paste0("_[0-9]+\\(.{1,3}\\)\\[", row, "\\,.\\]")),
                  after_cod = stringr::str_extract(fullcod, paste0("_[0-9]+\\(.{1,3}\\)\\[", row, ".+$")),
                  after_cod = stringr::str_remove(after_cod, paste0("_[0-9]+\\(.{1,3}\\)\\[", row, "\\,.\\]"))) %>% 
    dplyr::mutate(before_cod = stringr::str_remove_all(before_cod, "_[0-9]+\\(.{1,3}\\)\\[.{1,2}\\,0\\]"),
                  before_cod = stringr::str_remove_all(before_cod, "\\[.{1,2}\\,.\\]"),
                  after_cod = stringr::str_remove_all(after_cod, "_[0-9]+\\(.{1,3}\\)\\[.{1,2}\\,0\\]"),
                  after_cod = stringr::str_remove_all(after_cod, "\\[.{1,2}\\,.\\]"),
                  fullcod = stringr::str_replace_all(fullcod, "\\[.{1,2}\\,(0)\\]", "[0]"),
                  fullcod = stringr::str_replace_all(fullcod, "\\[(.{1,2})\\,1\\]", "[\\1]")) %>% 
    dplyr::mutate(valcod = stringr::str_remove_all(fullcod, "_[0-9]+\\(.{1,3}\\)\\[0\\]"),
                  valcod = stringr::str_remove_all(valcod, "\\[.{1,2}\\]")) %>% 
    dplyr::mutate(last_cod = stringr::str_extract(before_cod, "_[0-9]+\\(.{1,3}\\)$"),
                  next_cod = stringr::str_extract(after_cod, "^_[0-9]+\\(.{1,3}\\)")) %>% 
    dplyr::mutate(last_cod = ifelse(is.na(last_cod), "", last_cod), 
                  next_cod = ifelse(is.na(next_cod), "", next_cod))
  
}

fix_plays_after_and_before_period <- function(df){
  
  df %>% 
    dplyr::mutate(aux = ifelse(stringr::str_detect(cod, "_13"), row, 1000)) %>% 
    dplyr::group_by(joincod) %>% 
    dplyr::mutate(aux = min(aux, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(importance = ifelse(stringr::str_detect(modcod, "_13.") & row > aux, 0, importance)) %>% 
    dplyr::mutate(aux = ifelse(stringr::str_detect(cod, "_12"), row, 0)) %>% 
    dplyr::group_by(joincod) %>% 
    dplyr::mutate(aux = max(aux, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(importance = ifelse(stringr::str_detect(modcod, "._12") & row < aux, 0, importance)) %>% 
    dplyr::select(-aux)
  
}

nulling_cods_always_null <- function(df){
  
  df %>% 
    dplyr::mutate(importance = ifelse(stringr::str_detect(cod, "_(7|8|9|11|18)"), 0, importance),
                  importance = ifelse(stringr::str_detect(cod, "_6\\(4.{2}\\)"), 0, importance),
                  importance = ifelse(stringr::str_detect(cod, "_6\\(18a\\)"), 0, importance)) %>% 
    dplyr::mutate(importance = ifelse(stringr::str_detect(cod, "_7\\(4") & stringr::str_detect(joincod, "_1[02]", neg = T), 1, importance))
}

nulling_cods_should_be_null <- function(df){
  
  df %>% 
    dplyr::mutate(importance = ifelse(stringr::str_detect(cod, "_10\\(0\\)"), 0, importance),
                  importance = ifelse(stringr::str_detect(cod, "_5\\(13p\\)"), 0, importance),
                  importance = ifelse(stringr::str_detect(cod, "_5\\(00p\\)"), 0, importance),
                  importance = ifelse(stringr::str_detect(cod, "_5\\(26p\\)"), 0, importance),
                  importance = ifelse(stringr::str_detect(cod, "_3"), 0, importance),
                  importance = ifelse(cod == '_5(37t)' & stringr::str_detect(last_cod, "_4"), 0, importance),
                  importance = ifelse(stringr::str_detect(cod, '_4') & stringr::str_detect(last_cod, "_10"), 0, importance),
                  aux = ifelse(stringr::str_detect(cod, '_4') & stringr::str_detect(last_cod, "_3") & stringr::str_detect(next_cod, "_3"), 1, 0),
                  aux = ifelse(stringr::str_detect(cod, '_4') & stringr::str_detect(last_cod, "_3\\((12|13|23|51|71|81|82)") & stringr::str_detect(next_cod, "_5\\((19|20)p\\)"), 1, aux),
                  aux = ifelse(stringr::str_detect(cod, '_4') & stringr::str_detect(last_cod, "_3\\((12|13|23|51|71|81|82)") & stringr::str_detect(next_cod, "_6\\(0"), 1, aux),
                  importance = ifelse(aux == 1, 0, importance)) %>% 
    dplyr::select(-aux)
}

group_plays_around_jumpball <- function(df){
  
  df %>% 
    dplyr::mutate(join = 1) %>% 
    dplyr::mutate(join = ifelse(stringr::str_detect(cod, "_(8|9|18)"), 0, join), 
                  join = ifelse(stringr::str_detect(cod, "_5\\(26"), 0, join),
                  join = ifelse(stringr::str_detect(cod, "_5\\(21"), 0, join),
                  join = ifelse(stringr::str_detect(cod, "_6\\((18a|4.{2})"), 0, join),
                  join = ifelse(stringr::str_detect(cod, "_7\\((0|1|4)"), 0, join)) %>% 
    dplyr::mutate(join = ifelse(stringr::str_detect(cod, "_7\\(4") & stringr::str_detect(joincod, "_1[02]", neg = T), 1, join)) %>% 
    dplyr::group_by(joincod) %>%
    dplyr::mutate(sum_join = cumsum(join)) %>% 
    dplyr::mutate(ref = ifelse(stringr::str_detect(cod, "_10|_7") & importance == 1, sum_join, 1000)) %>% 
    dplyr::arrange(joincod, desc(row)) %>% 
    dplyr::mutate(ref = cummin(ref),
                  max_si = max(si)) %>% 
    dplyr::arrange(joincod, row) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(si = ifelse(ref < 1000 & join == 0 & (sum_join + 1) == ref & sum_join > 0 & si < max_si, si + 1, si)) %>% 
    dplyr::select(-c(join, sum_join, ref))
  
}

group_plays_around_fouls <- function(df){
  
  df %>%
    put_features_cod() %>%
    dplyr::mutate(join = importance,
                  join = ifelse(stringr::str_detect(cod, "_(3|10|11|4|7\\(2|7\\(3|7\\(4|7\\(6)"), 1, join),
                  join = ifelse(stringr::str_detect(cod, "_5\\(13|_5\\(00|_5\\(37"), 1, join)) %>%
    dplyr::group_by(joincod) %>%
    dplyr::mutate(sum_join = cumsum(join)) %>%
    dplyr::mutate(ref = ifelse(stringr::str_detect(cod, "_6\\((0|10|11|12|13|14|15|16|17|19|20|21|22)") & importance == 1, sum_join, 1000)) %>% 
    dplyr::arrange(joincod, desc(row)) %>% 
    dplyr::mutate(ref = cummin(ref),
                  max_si = max(si)) %>% 
    dplyr::arrange(joincod, row) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(si = ifelse(ref < 1000 & join == 0 & (sum_join + 1) == ref & sum_join > 0 & si < max_si, si + 1, si)) %>% 
    dplyr::select(-c(join, sum_join, ref, max_si))
  
}

group_plays_around_turnovers <- function(df){
  
  df %>% 
    dplyr::mutate(join = 1, 
                  join = ifelse(stringr::str_detect(cod, "_(8|9|11|18)"), 0, join), 
                  join = ifelse(stringr::str_detect(cod, "_6\\((18a|4.{2})"), 0, join),
                  join = ifelse(stringr::str_detect(cod, "_7\\((0|1|4|5)"), 0, join)) %>% 
    dplyr::group_by(joincod) %>%
    dplyr::mutate(sum_join = cumsum(join)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(ref = ifelse(stringr::str_detect(cod, "_5"), sum_join, 1000),
                  ref = ifelse(ref < 1000 & stringr::str_detect(last_cod, "_(6|10)"), 1000, ref),
                  ref = ifelse(importance == 0, 1000, ref)) %>% 
    dplyr::arrange(joincod, desc(row)) %>% 
    dplyr::group_by(joincod) %>%
    dplyr::mutate(ref = cummin(ref),
                  max_si = max(si)) %>% 
    dplyr::arrange(joincod, row) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(si = ifelse(ref < 1000 & join == 0 & (sum_join + 1) == ref & sum_join > 0 & si < max_si, si + 1, si)) %>% 
    dplyr::select(-c(join, sum_join, ref, max_si))
  
}

assign_specific_turnovers_situations <- function(df){
  
  df  %>%
    dplyr::mutate(aux = ifelse(stringr::str_detect(cod, '_5\\((19|20)') & last_cod %in% c('_4(1t)', '_4(0u)'), 5, aux),
                  aux = ifelse(stringr::str_detect(cod, '_5\\((19|20)') & stringr::str_detect(last_cod, "_6"), 6, aux),
                  aux = ifelse(stringr::str_detect(cod, '_5\\((19|20)') & stringr::str_detect(last_cod, "_10"), 7, aux),
                  aux = ifelse(stringr::str_detect(cod, '_5\\(21') & stringr::str_detect(joincod, "_5\\(21p\\)(.*?)_10"), 8, aux),
                  aux = ifelse(stringr::str_detect(cod, '_5\\((36|39|40)') & stringr::str_detect(next_cod, "_6\\(0"), 9, aux),
                  aux = ifelse(stringr::str_detect(cod, '_5\\((36|39|40)') & stringr::str_detect(last_cod, "_6\\(0"), 10, aux),
                  aux = ifelse(stringr::str_detect(cod, '_13') & stringr::str_detect(last_cod, '_1(3|8)'), 10, aux)) 
  
}

assign_specific_tech_fouls_situations <- function(df){
  
  df %>% 
    dplyr::mutate(aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_33(_3_|_3$)"), 11, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_5_16_33(_3_|_3$)"), 12, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_33_4(_3_|_3$)"), 13, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_3_16_33(_3_|_3$)"), 14, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_3_16_33_4(_3_|_3$)"), 15, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_3_4_16_33(_3_|_3$)"), 16, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_3_4_16_33_4(_3_|_3$)"), 17, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_16_33_33(_3_|_3$)"), 18, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_16_33_33_4(_3_|_3$)"), 19, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_33_16_33(_3_|_3$)"), 20, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_3_3_16_33(_3_|_3$)"), 21, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_16_33_4_33(_3_|_3$)"), 22, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_16_16_33_33_33_4(_3_|_3$)"), 23, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_33_4_16_33(_3_|_3$)"), 24, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_33_16_33_16_33(_3_|_3$)"), 25, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_16_33_4_33_4(_3_|_3$)"), 26, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_3_3_4_16_33(_3_|_3$)"), 27, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_16_16_33_4_33_33(_3_|_3$)"), 28, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_3_16_16_33_33_4(_3_|_3$)"), 29, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_16_16_33(_3_|_3$)"), 30, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_16_16_16_33(_3_|_3$)"), 31, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(modcod, "_6_16_33_16_33_4(_3_|_3$)"), 32, aux)) %>% 
    dplyr::mutate(aux = ifelse(stringr::str_detect(cod, "_6\\((1|2)") & stringr::str_detect(last_cod, "_6\\(0") & stringr::str_detect(modcod, "_16_33_6(_3_|_3$)"), 33, aux),
                  aux = ifelse(stringr::str_detect(cod, '_6\\(0') & stringr::str_detect(before_cod, "_6\\(0"), 34, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(last_cod, "_6\\((1|2)") & stringr::str_detect(modcod, "_16(_3$|_3_)"), 35, aux)) 
  
  
}

assign_specific_modcod_situations <- function(df){
  
  df %>% 
    dplyr::mutate(aux = ifelse(stringr::str_detect(modcod, "_16") & stringr::str_detect(modcod, "_3", neg = T) & stringr::str_detect(cod, '_6\\(0'), 41, aux),
                  aux = ifelse(stringr::str_detect(modcod, "_16_16") & stringr::str_detect(modcod, "_33", neg = T) & stringr::str_detect(modcod, "_3") & stringr::str_detect(cod, '_6\\(0'), 42, aux),
                  aux = ifelse(stringr::str_detect(modcod,'_6_3_4_5') & stringr::str_detect(cod, '_4') & stringr::str_detect(modcod, '_1$|_1_', neg = T) & stringr::str_detect(after_cod, "_5\\(19|20"), 43, aux),
                  aux = ifelse(stringr::str_detect(modcod,'_6_3_4_10') & stringr::str_detect(cod, '_4') & stringr::str_detect(modcod, '_1_|_1$', neg = T) & stringr::str_detect(next_cod, "_10"), 44, aux),
                  aux = ifelse(stringr::str_detect(modcod,'_6_3_4_5') & stringr::str_detect(cod, '_4') & after_cod == '', 45, aux),
                  aux = ifelse(stringr::str_detect(modcod,'_6_3_4$') & stringr::str_detect(cod, '_4') & stringr::str_detect(joincod, "_3\\(11", neg = T) & stringr::str_count(modcod, "_3") == 1 & after_cod == '', 46, aux),
                  aux = ifelse(modcod %in% c("_12_33", "_12_33_4") & stringr::str_detect(cod, '_3'), 50, aux),
                  aux = ifelse(modcod %in% c("_1_3_6", "_4_3_6_3", "_4_3_3_6") & stringr::str_detect(cod, '_3\\(1'), 51, aux),
                  aux = ifelse(modcod %in% c("_1_3_6", "_4_3_6_3", "_4_3_3_6") & stringr::str_detect(cod, '_6'), 52, aux),
                  aux = ifelse(modcod %in% c("_12_16_33_10") & stringr::str_detect(cod, '_6'), 53, aux),
                  aux = ifelse(modcod %in% c("_3_4_6_3") & stringr::str_detect(cod, '_4'), 54, aux),
                  aux = ifelse(modcod %in% c("_6_6_3_3_3_4_3_4", "_6_6_3_3_3_4_3") & stringr::str_detect(cod, '_6') & stringr::str_detect(last_cod, "_6"), 55, aux),
                  aux = ifelse(modcod %in% c("_2_4_33_16") & stringr::str_detect(cod, '_3'), 56, aux),
                  aux = ifelse(modcod %in% c("_5_1", "_5_2") & stringr::str_detect(cod, '_5'), 57, aux))
}

simplify_cod <- function(df){
  
  df %>% 
    dplyr::mutate(codf = NA_character_,
                  codf = ifelse(clean_cod %in% c(""), clean_cod, codf),
                  
                  codf = ifelse(clean_cod %in% c("_12(1)_10(0)", "_12(1)"), clean_cod, codf),
                  codf = ifelse(clean_cod %in% c("_10(0)_12(1)"), "_12(1)_10(0)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_12\\(2\\)"), "_12(2)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_12\\(3\\)"), "_12(3)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_12\\(4\\)"), "_12(4)", codf),
                  codf = ifelse(clean_cod %in% c("_12(0)_10(0)", "_12(0)"), clean_cod, codf),
                  codf = ifelse(clean_cod %in% c("_10(0)_12(0)", "_12(0)_6(01p)_3(40c)_10(0)", "_6(01c)_3(40c)_12(0)_10(0)", 
                                                 "_6(01p)_3(40c)_12(0)_10(0)"), "_12(0)_10(0)", codf),
                  
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_13\\(1\\)"), "_13(1)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_13\\(2\\)"), "_13(2)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_13\\(3\\)"), "_13(3)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_13\\(4\\)"), "_13(4)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_13\\(0\\)"), "_13(0)", codf),
                  
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_1\\(1\\)"), "_1(1)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_1\\(1a\\)"), "_1(1a)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_1\\(2\\)"), "_1(2)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_1\\(2a\\)"), "_1(2a)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_1\\(3\\)"), "_1(3)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_1\\(3a\\)"), "_1(3a)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_1\\(4\\)"), "_1(4)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_1\\(4a\\)"), "_1(4a)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_1\\(5\\)"), "_1(5)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_1\\(5a\\)"), "_1(5a)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_2\\(1\\)"), "_2(1)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_2\\(1b\\)"), "_2(1b)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_2\\(2\\)"), "_2(2)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_2\\(2b\\)"), "_2(2b)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_2\\(3\\)"), "_2(3)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_2\\(3b\\)"), "_2(3b)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_2\\(4\\)"), "_2(4)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_2\\(4b\\)"), "_2(4b)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_2\\(5\\)"), "_2(5)", codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(clean_cod, "_2\\(5b\\)"), "_2(5b)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_4(0p)", "_4(0u)"), "_4(0p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_4(0t)", "_4(0t)_6(01p)", "_4(0t)_6(01c)"), "_4(0t)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_4(1t)"), "_4(1t)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(01p)"), "_5(01p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(11p)"), "_5(11p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(12p)"), "_5(12p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_10(1)_5(13p)", "_6(13a)_5(13p)", "_6(12a)_5(13p)", "_6(10a)_5(00p)", 
                                                               "_6(12a)_5(00p)", "_5(00p)", "_6(10p)_5(00p)", "_5(13p)_6(13a)", "_5(13p)", 
                                                               "_6(10a)_5(13p)", "_6(13a)_5(13p)_6(01p)_6(01p)", "_5(13p)_6(10a)", 
                                                               "_6(12p)_5(00p)", "_6(10p)_5(13p)", "_5(13p)_6(12a)", "_6(12p)_5(13p)", 
                                                               "_6(13a)_5(00p)", "_6(05p)_3(40c)_6(13a)_5(13p)", "_6(01p)_3(40c)_6(13a)_5(13p)", 
                                                               "_6(05p)_3(40w)_6(13a)_5(13p)", "_6(01c)_3(40c)_6(13a)_5(13p)", 
                                                               "_6(13a)") , "_6(13a)_5(13p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(14p)"), "_5(14p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(15p)"), "_5(15p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(16p)"), "_5(16p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(17p)"), "_5(17p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(18p)"), "_5(18p)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(19p)", "_6(11a)_3(12c)_5(19p)", "_6(11a)_3(12w)_5(19p)", 
                                                               "_6(11p)_3(13w)_3(23c)_5(19p)", "_6(11a)_5(19p)", "_6(11p)_3(12c)_5(19p)", 
                                                               "_6(21p)_3(11c)_5(19p)", "_6(10p)_3(11w)_5(19p)", "_6(11a)_3(11c)_5(19p)", 
                                                               "_6(11a)_3(11w)_5(19p)", "_6(11a)_3(13c)_3(23c)_5(19p)", "_6(11p)_3(12w)_5(19p)", 
                                                               "_6(11p)_3(11c)_5(19p)", "_6(10p)_3(11c)_5(19p)", "_6(10p)_3(12c)_5(19p)",
                                                               "_6(11p)_5(19p)"), "_5(20p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(11a)_3(12c)_5(20p)", "_6(11a)_3(12w)_5(20p)", "_6(11a)_5(20p)", 
                                                               "_6(10p)_3(12c)_5(20p)", "_6(11p)_3(12c)_5(20p)", "_6(10p)_3(12w)_5(20p)", 
                                                               "_6(11p)_3(12w)_5(20p)", "_6(11p)_5(20p)", "_6(12p)_3(12c)_5(20p)", "_5(20p)", 
                                                               "_6(12p)_3(12w)_5(20p)", "_6(11a)_3(11w)_5(20p)", "_6(21p)_3(12w)_5(20p)", 
                                                               "_6(21p)_3(12c)_5(20p)", "_6(10p)_3(11c)_5(20p)", "_6(11p)_3(13c)_3(23c)_5(20p)", 
                                                               "_6(10p)_5(20p)_3(22c)", "_6(11a)_3(13c)_3(23c)_5(20p)", "_6(11p)_5(20p)_3(22w)", 
                                                               "_6(11a)_5(20p)_3(11c)", "_6(11a)_5(20p)_3(22c)", "_6(10a)_5(20p)", "_6(15a)_5(00p)", 
                                                               "_6(15p)_5(00p)", "_6(11a)_3(12c)_5(00p)", "_6(11p)_3(12c)_5(00p)", 
                                                               "_6(11a)_3(13w)_3(23c)", "_6(15p)_3(12c)", 
                                                               "_6(12a)_3(12w)"), "_5(20p)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(11a)_3(12c)", "_6(11a)", "_6(10p)_3(12w)", "_6(10p)_3(12c)", "_6(11p)_3(12w)", 
                                                               "_6(11p)_3(12c)", "_6(11a)_3(12w)", "_6(11p)", "_6(12p)_3(12c)", "_6(11a)_3(11c)", 
                                                               "_6(11a)_3(13c)_3(23c)", "_6(11p)_3(11c)", 
                                                               "_6(21p)_3(12c)") & stringr::str_detect(cods, '_7\\(6'), "_5(20p)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(21p)", "_5(21p)_10(1)"), "_5(21p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(22p)"), "_5(22p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(23p)"), "_5(23p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(24p)"), "_5(24p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(25t)"), "_5(25t)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(26p)"), "_5(26p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(27p)"), "_5(27p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(28p)"), "_5(28p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(29p)"), "_5(29p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(30p)"), "_5(30p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(31p)"), "_5(31p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(32p)"), "_5(32p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(33p)"), "_5(33p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(34p)"), "_5(34p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(35p)"), "_5(35p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(36t)"), "_5(36t)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_4(0t)_5(37t)", "_5(37t)", "_4(0p)_5(37t)", "_4(1t)_5(37t)"), "_5(37t)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(38t)"), "_5(38t)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(41p)"), "_5(41p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_5(43p)"), "_5(43p)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_10(1)", "_10(1)_4(0t)", "_10(1)_4(0p)", "_10(1)_4(1t)",
                                                               "_10(1)_5(20p)", "_10(1)_5(26p)", "_5(26p)_10(1)", "_10(2)"), "_10", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(01p)_6(01p)", "_6(01p)_6(01p)_6(01p)_6(01c)", "_6(01p)_6(01t)",
                                                               "_6(01p)_6(01p)_6(01p)_6(01p)", "_6(05p)_6(01p)", "_6(05p)_6(01c)",
                                                               "_6(01p)_6(01c)") , "_6(0x)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(10a)", "_6(10p)", "_6(16a)", "_6(01c)_3(40w)_6(10a)", 
                                                               "_6(06t)_3(40c)_6(10a)", "_6(01c)_3(40c)_6(10a)", "_6(05p)_3(40c)_6(10a)",
                                                               "_6(10a)_6(01p)_6(01p)", "_6(01p)_3(40w)_6(10a)", 
                                                               "_6(01p)_3(40c)_6(10a)") , "_6(10)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(11a)", "_6(11p)") , "_6(11)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(12a)", "_6(12p)") , "_6(12)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(14a)"), "_6(14)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(15a)"), "_6(15)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(17a)"), "_6(17)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(19a)"), "_6(19)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(21p)", "_6(21a)"), "_6(21)", codf),
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(22a)"), "_6(22)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(10a)_3(11c)", "_3(11c)_6(10a)", "_6(10a)_5(00p)_3(11c)", "_6(10p)_3(11c)",
                                                               "_6(10p)_5(00p)_3(11c)", "_3(11c)_6(11a)", "_6(11a)_3(22c)", "_3(11c)_6(11p)",
                                                               "_6(12a)_3(11c)", "_6(12a)_5(00p)_3(11c)", "_6(12p)_3(11c)", "_6(12p)_5(00p)_3(11c)",
                                                               "_6(12p)_5(13p)_3(11c)", "_6(14a)_3(11c)", "_6(14p)_3(11c)", "_6(15a)_3(11c)",
                                                               "_6(15a)_5(00p)_3(11c)", "_6(15a)_5(13p)_3(11c)", "_6(15p)_3(11c)",
                                                               "_6(15p)_5(00p)_3(11c)", "_6(15p)_5(13p)_3(11c)", "_6(21p)_3(11c)", "_6(11a)_3(11c)",
                                                               "_6(11a)_3(12c)", "_6(11p)_3(11c)", "_6(11p)_3(12c)", "_6(10p)_3(12c)",
                                                               "_6(12p)_3(12c)", "_6(21p)_3(12c)"), "_6_3(11c)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(22a)_3(11c)", "_6(22p)_3(11c)"), "_6(22)_3(11c)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(10a)_3(11w)", "_6(10a)_5(00p)_3(11w)", "_6(10p)_3(11w)", "_6(11a)_3(11w)",
                                                               "_6(11p)_3(11w)", "_6(12a)_3(11w)", "_6(12p)_3(11w)", "_6(12p)_5(00p)_3(11w)",
                                                               "_6(13a)_3(11w)", "_6(14a)_3(11w)", "_6(15a)_3(11w)", "_6(15a)_5(00p)_3(11w)",
                                                               "_6(15a)_5(13p)_3(11w)", "_6(15p)_3(11w)", "_6(15p)_5(00p)_3(11w)", "_6(21p)_3(11w)",
                                                               "_6(10p)_3(12w)", "_6(11p)_3(12w)", "_6(11a)_3(12w)", "_6(14p)_3(11w)"), "_6_3(11w)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(22a)_3(11w)", "_6(22p)_3(11w)"), "_6(22)_3(11w)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(10a)_6(01p)_3(40c)_3(11c)", "_6(10p)_6(01p)_3(40c)_3(11c)", 
                                                               "_6(11a)_6(01c)_3(40c)_3(11c)", "_6(11a)_6(01p)_3(40c)_3(11c)", 
                                                               "_6(11a)_6(01c)_3(40w)_3(11c)", "_6(11a)_6(01p)_3(40w)_3(11c)",
                                                               "_6(11a)_6(06t)_3(40c)_3(11c)", "_6(11a)_6(06t)_3(40w)_3(11c)",
                                                               "_6(11a)_6(01p)_6(01p)_3(40w)_3(40c)_3(11c)", 
                                                               "_6(11a)_6(01p)_6(01p)_3(40c)_3(40c)_3(11c)",
                                                               "_6(11a)_6(01p)_6(01c)_3(40w)_3(40c)_3(11c)", 
                                                               "_6(11a)_6(01c)_6(01c)_3(40c)_3(40c)_3(11c)",
                                                               "_6(11a)_6(01p)_3(40c)_6(01c)_3(40c)_3(11c)", 
                                                               "_6(11p)_6(01c)_3(40c)_3(11c)", "_6(11p)_6(01c)_3(40w)_3(11c)",
                                                               "_6(11p)_6(01p)_3(40c)_3(11c)", "_6(11p)_6(01p)_3(40w)_3(11c)", 
                                                               "_6(01p)_3(40c)_6(11p)_3(11c)", "_6(11p)_6(06t)_3(40c)_3(11c)", 
                                                               "_6(11p)_6(01c)_6(01c)_3(40c)_3(40c)_3(11c)", "_6(12a)_6(01p)_3(40c)_3(11c)",
                                                               "_6(12p)_6(01c)_3(40c)_3(11c)", "_6(12p)_6(01p)_3(40c)_3(11c)", 
                                                               "_6(01p)_3(40c)_6(14a)_3(11c)", "_6(15a)_6(01p)_3(40c)_3(11c)", 
                                                               "_6(15a)_6(01p)_6(01p)_3(40c)_3(40c)_3(11c)", "_6(01p)_3(40c)_6(15a)_3(11c)",
                                                               "_6(01c)_3(40c)_6(15a)_3(11c)", "_6(15p)_6(01p)_3(40c)_3(11c)", 
                                                               "_6(05p)_3(40c)_6(15p)_3(11c)"), "_6_3(40x)_3(11c)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(22a)_6(01p)_3(40c)_3(11c)", "_6(22a)_6(01t)_3(40c)_3(11c)", 
                                                               "_6(22p)_6(01p)_3(40c)_3(11c)"), "_6(22)_3(40x)_3(11c)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(11a)_6(06t)_3(40c)_3(11w)", "_6(11a)_6(01t)_3(40c)_3(11w)", 
                                                               "_6(11p)_6(01p)_6(01t)_3(40c)_3(40c)_3(11w)", "_6(11a)_6(01c)_3(40w)_3(11w)", 
                                                               "_6(11a)_6(01c)_3(40c)_3(11w)", "_6(11a)_6(01p)_3(40w)_3(11w)",
                                                               "_6(11a)_6(01p)_3(40c)_6(01p)_3(40c)_3(11w)", 
                                                               "_6(11a)_6(01p)_6(01p)_3(40c)_3(40c)_3(11w)",
                                                               "_6(11a)_6(06t)_3(40w)_3(11w)", "_6(11p)_6(01c)_3(40c)_3(11w)", 
                                                               "_6(11p)_6(01p)_3(40w)_3(11w)", "_6(11p)_6(01p)_3(40c)_3(11w)", 
                                                               "_6(11p)_6(01c)_6(01c)_3(40w)_3(40c)_3(11w)", 
                                                               "_6(11p)_6(01c)_6(01c)_3(40c)_3(40c)_3(11w)", 
                                                               "_6(11p)_6(01p)_6(01p)_3(40c)_3(40c)_3(11w)",
                                                               "_6(11p)_6(01p)_3(40c)_6(01p)_3(40c)_3(11w)", "_6(12p)_6(01p)_3(40c)_3(11w)",
                                                               "_6(15a)_6(01c)_3(40c)_3(11w)", "_6(15a)_6(01p)_3(40w)_3(11w)", 
                                                               "_6(15a)_6(01p)_3(40c)_3(11w)","_6(11a)_6(01p)_3(40c)_3(11w)"), "_6_3(40x)_3(11w)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(22a)_6(01p)_3(40c)_3(11w)", "_6(22a)_6(01t)_3(40c)_3(11w)", 
                                                               "_6(22p)_6(01p)_3(40c)_3(11w)"), "_6(22)_3(40x)_3(11w)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_3(40c)", "_3(40c)_6(01c)", "_6(01c)_3(40c)", "_6(01p)_3(11c)", 
                                                               "_3(40c)_6(01p)", "_6(01p)_3(40c)", "_5(36t)_6(01p)_3(40c)", "_6(01t)_3(40c)", 
                                                               "_6(01u)_3(40c)", "_5(40t)_6(01u)_3(40c)", "_6(05p)_3(11c)", "_6(05p)_3(40c)", 
                                                               "_6(06t)_3(11c)", "_6(06t)_3(40c)", "_6(06u)_3(40c)", "_6(08t)_5(39t)_3(40c)",
                                                               "_5(39t)_6(08t)_3(40c)", "_6(08u)_5(39t)_3(40c)", "_6(09t)_3(40c)", 
                                                               "_6(09t)_3(40c)_5(40t)", "_6(09t)_5(40t)_3(40c)", "_6(01c)_6(01c)_3(40w)_3(40c)", 
                                                               "_6(01c)_6(01c)_3(40c)_3(40c)", "_6(01c)_6(01p)_3(40c)_3(40c)", 
                                                               "_6(01c)_3(40c)_6(01c)_3(40c)", "_6(01c)_6(01p)_3(40w)_3(40c)",
                                                               "_6(01c)_3(40w)_6(01c)_3(40c)", "_6(01c)_3(40c)_6(01p)_3(40c)", 
                                                               "_6(01c)_6(01c)_6(01p)_3(40c)_3(40c)_3(40c)", "_6(01c)_3(40c)_6(01p)_3(40c)_6(01p)_3(40c)", 
                                                               "_6(01p)_6(01p)_3(40w)_3(40c)", "_6(01p)_6(01c)_3(40c)_3(40c)",
                                                               "_6(01p)_6(01c)_3(40w)_3(40c)", "_6(01p)_6(01c)_6(01c)_3(40c)_3(40c)_3(40c)", 
                                                               "_6(01p)_6(01p)_3(40c)_3(40c)", "_6(01p)_3(40c)_6(01c)_3(40c)", "_6(01p)_3(40c)_6(01p)_3(40c)", 
                                                               "_6(01p)_3(40w)_6(01c)_3(40c)", "_6(01p)_3(40w)_6(01p)_3(40c)", "_6(01p)_3(40c)_6(01t)_3(40c)", 
                                                               "_6(01p)_3(40c)_6(01c)_3(40c)_6(01c)_3(40c)", "_6(01p)_3(40c)_6(01p)_3(40c)_6(01p)_3(40c)", 
                                                               "_6(05p)_6(01p)_3(40c)_3(40c)", "_6(05p)_6(01c)_3(40w)_3(40c)", "_6(05p)_6(01c)_3(40c)_3(40c)", 
                                                               "_6(05p)_3(40c)_6(01p)_3(40c)", "_6(05p)_3(40c)_6(01c)_3(40c)", "_6(05p)_3(40w)_6(01p)_3(40c)", 
                                                               "_6(05p)_3(40w)_6(05p)_3(40c)", "_6(06t)_6(01p)_3(40c)_3(40c)", "_6(06t)_3(40c)_6(01p)_3(40c)", 
                                                               "_6(06t)_3(40c)_6(06t)_3(40c)", "_6(09t)_3(40c)_6(01c)_3(40c)", 
                                                               "_5(40t)_6(01t)_3(40c)"), "_6(0x)_3(40c)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_3(40w)", "_6(01c)_3(40w)", "_6(01p)_3(40w)", "_5(36t)_6(01p)_3(40w)", "_6(01t)_3(40w)", 
                                                               "_6(01t)_5(25t)_3(40w)", "_6(01u)_3(40w)", "_6(05p)_3(40w)", "_6(06t)_3(40w)", "_6(06u)_3(40w)", 
                                                               "_6(08t)_3(40w)", "_6(08t)_5(39t)_3(40w)", "_6(09t)_5(40t)_3(40w)", "_6(01c)_6(01c)_3(40c)_3(40w)", 
                                                               "_6(01c)_6(01p)_3(40c)_3(40w)", "_6(01c)_3(40c)_6(01c)_3(40w)", "_6(01c)_3(40c)_6(01p)_3(40w)", 
                                                               "_6(01p)_3(40w)_6(01c)_3(40w)", "_6(01p)_3(40w)_6(01p)_3(40w)", "_6(01p)_6(01p)_3(40c)_3(40w)", 
                                                               "_6(01p)_3(40c)_6(01c)_3(40w)", "_6(01p)_3(40c)_6(01p)_3(40w)", "_6(05p)_3(40w)_6(01p)_3(40w)", 
                                                               "_6(05p)_3(40c)_6(01p)_3(40w)", "_6(05p)_6(01p)_3(40c)_3(40w)",
                                                               "_6(06t)_6(01p)_3(40c)_3(40w)"), "_6(0x)_3(40w)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(10a)_3(71c)_3(72c)", "_6(10a)_3(71w)_3(72c)", "_6(17a)_3(12c)_3(22c)", 
                                                               "_6(17a)_3(71w)_3(72c)", "_6(17a)_3(71c)_3(72c)", "_6(17p)_3(71w)_3(72c)", "_6(17p)_3(71c)_3(72c)",
                                                               "_6(19a)_3(51c)", "_6(19a)_3(61c)", "_6(19a)_5(00p)_3(61c)", "_6(19a)_3(51w)_3(52c)", 
                                                               "_6(19a)_3(12c)_3(22c)", "_6(19a)_3(51c)_3(52c)", "_3(61c)_3(52c)_6(19a)", "_6(19a)_5(00p)_3(51w)_3(52c)", 
                                                               "_6(19a)_5(00p)_3(12c)_3(22c)", "_6(19a)_5(00p)_3(51c)_3(52c)", "_6(19a)_5(13p)_3(51w)_3(52c)", 
                                                               "_6(19a)_5(13p)_3(51c)_3(52c)", "_6(19a)_5(28p)_3(51c)_3(52c)", "_6(19a)_3(81w)_3(82c)_3(83c)", 
                                                               "_6(19a)_3(81w)_3(82w)_3(83c)", "_6(19a)_3(81w)_3(23c)_3(33c)", "_6(19a)_3(81c)_3(82c)_3(83c)", 
                                                               "_6(19a)_3(81c)_3(82w)_3(83c)", "_6(19a)_3(81c)_3(23c)_3(33c)", "_6(19a)_3(51c)_3(52c)_3(33c)", 
                                                               "_6(19a)_3(51c)_3(52c)_3(11c)", "_6(19a)_3(51c)_3(52c)_3(61c)", "_6(11a)_6(19a)_3(12c)_3(22c)_3(51w)_3(52c)", 
                                                               "_6(19p)_3(51c)", "_6(19p)_3(61c)", "_6(19p)_5(13p)_3(61c)", "_6(19p)_3(51w)_3(52c)", 
                                                               "_6(19p)_3(12c)_3(22c)_3(61c)", "_6(19p)_5(00p)_3(51w)_3(52c)", "_5(13p)_6(19p)_3(51w)_3(52c)", 
                                                               "_6(19p)_3(51c)_3(52c)", "_6(19p)_3(61c)_3(52c)", "_6(19p)_5(00p)_3(51c)_3(52c)", "_6(19p)_5(13p)_3(51c)_3(52c)", 
                                                               "_6(19p)_5(13p)_3(51w)_3(52c)", "_6(19p)_3(81c)_3(82w)_3(83c)", "_6(19p)_3(81c)_3(82c)_3(83c)", 
                                                               "_6(19p)_3(81c)_3(23c)_3(33c)", "_6(19p)_3(81w)_3(82c)_3(83c)", "_6(20a)_3(51c)", "_6(20a)_3(61c)", 
                                                               "_6(20a)_3(51w)_3(52c)", "_6(20a)_5(00p)_3(51w)_3(52c)", "_6(20a)_5(13p)_3(51w)_3(52c)", "_6(20a)_3(51c)_3(52c)",
                                                               "_6(20a)_5(00p)_3(51c)_3(52c)", "_6(20a)_5(13p)_3(51c)_3(52c)", "_6(11a)_6(20a)_3(11c)_3(22c)_3(51w)_3(52c)",
                                                               "_6(20p)_3(61c)", "_6(20p)_3(51w)_3(52c)", "_6(20p)_3(51c)_3(52c)", "_6(20p)_5(00p)_3(51c)_3(52c)",
                                                               "_6(20p)_5(13p)_3(51c)_3(52c)", "_6(20p)_5(28p)_3(51c)_3(52c)") , "_6(19x)_3(51x)_3(52c)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(17a)_3(71w)_3(72w)", "_6(17a)_3(71c)_3(72w)", "_6(17a)_3(71c)_4(0t)_3(72w)", 
                                                               "_6(17p)_3(71w)_3(72w)", "_6(17p)_3(71c)_3(72w)", "_6(11a)_3(51c)_3(52w)", "_6(19a)_3(12w)_3(52w)", 
                                                               "_6(19a)_3(51w)_3(52w)", "_6(19a)_3(51c)_3(52w)", "_6(19a)_5(00p)_3(51c)_3(52w)", 
                                                               "_6(19a)_3(81c)_3(82c)_3(83w)", "_6(19a)_5(00p)_3(51w)_3(52w)", "_6(19a)_5(13p)_3(51w)_3(52w)", 
                                                               "_6(19a)_3(81w)_3(82c)_3(83w)", "_6(19a)_5(13p)_3(51c)_3(52w)", "_6(19a)_3(81c)_3(82w)_3(83w)", 
                                                               "_6(19p)_3(51w)_3(52w)", "_6(19p)_3(12c)_3(52w)", "_6(19p)_3(51c)_3(52w)", "_6(19p)_5(13p)_3(51w)_3(52w)", 
                                                               "_6(19p)_5(00p)_3(51c)_3(52w)", "_6(19p)_5(13p)_3(51c)_3(52w)", "_6(19p)_5(00p)_3(51w)_3(52w)", 
                                                               "_6(19p)_3(81c)_3(82c)_3(83w)", "_6(19p)_3(81w)_3(82w)_3(83w)", "_6(19p)_3(81c)_3(82w)_3(83w)", 
                                                               "_6(20a)_3(51w)_3(52w)", "_6(20a)_3(51c)_3(52w)", "_6(20a)_5(00p)_3(51c)_3(52w)", "_6(20a)_5(13p)_3(51c)_3(52w)", 
                                                               "_6(20p)_3(51c)_3(52w)", "_6(20p)_5(13p)_3(51w)_3(52w)", "_6(17p)_3(12c)_3(22w)", 
                                                               "_6(19a)_3(12c)_3(22w)"), "_6(19x)_3(51x)_3(52w)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(17a)_6(01c)_3(40c)_3(71c)_3(72c)", "_6(17a)_6(01p)_3(40c)_3(71w)_3(72c)",
                                                               "_6(17a)_6(01p)_3(40c)_3(71c)_3(72c)", "_6(19a)_6(01p)_3(40c)_3(61c)", 
                                                               "_6(19a)_6(01c)_3(40c)_3(51w)_3(52c)", "_6(19a)_6(01c)_3(40c)_6(01c)_3(40c)_6(01p)_3(40c)_3(51c)_3(52c)", 
                                                               "_6(19a)_6(01p)_3(40w)_3(51w)_3(52c)", "_6(19a)_6(01p)_6(01c)_6(01p)_3(40c)_3(40c)_3(40w)_3(51c)_3(52c)", 
                                                               "_6(19a)_6(01p)_3(40w)_3(51c)_3(52c)", "_6(19a)_6(01p)_3(40c)_3(51w)_3(52c)", "_6(19a)_6(01p)_3(40c)_3(51c)_3(52c)",
                                                               "_6(19a)_5(13p)_6(01p)_3(40c)_3(51c)_3(52c)", "_6(19a)_3(51c)_6(01p)_3(40c)_3(52c)", 
                                                               "_6(19a)_5(00p)_6(01p)_3(40c)_3(51w)_3(52c)", "_6(19a)_3(51w)_6(01p)_3(40w)_3(52c)", 
                                                               "_6(19a)_5(00p)_6(01p)_3(40c)_3(51c)_3(52c)", "_6(19a)_5(13p)_6(01c)_3(40c)_3(51c)_3(52c)", 
                                                               "_6(19a)_6(01c)_3(40c)_3(51c)_3(52c)", "_6(19a)_6(01p)_3(40c)_3(61c)_3(52c)", "_6(01p)_6(19a)_3(40c)_3(51c)_3(52c)",
                                                               "_6(19a)_6(01p)_3(40w)_3(61c)", "_6(19p)_6(01p)_3(40c)_3(51c)_3(52c)", "_6(19p)_6(01p)_3(40c)_3(51w)_3(52c)",
                                                               "_6(19p)_6(01p)_6(01p)_3(40c)_3(40c)_3(51c)_3(52c)", "_6(19p)_6(01p)_6(01p)_3(40w)_3(40c)_3(61c)",
                                                               "_6(19p)_6(01p)_3(40w)_3(51c)_3(52c)", "_6(19p)_5(00p)_6(01p)_3(40c)_3(51c)_3(52c)", 
                                                               "_6(19p)_6(01c)_3(40w)_3(51w)_3(52c)", "_6(19p)_6(01p)_3(40c)_3(51c)_6(01p)_3(40c)_3(52c)", 
                                                               "_6(19p)_6(01p)_3(40c)_6(01c)_3(40c)_3(51c)_3(52c)", "_6(01p)_3(40c)_6(19p)_3(61c)", "_6(19p)_3(51c)_6(01p)_3(40c)_3(52c)", 
                                                               "_6(20a)_6(01p)_3(40c)_3(51c)_3(52c)", "_6(20a)_5(13p)_6(01p)_3(40c)_3(51w)_3(52c)", "_6(20a)_6(01c)_3(40c)_3(51c)_3(52c)", 
                                                               "_6(20a)_6(01c)_3(40c)_3(61c)", "_6(20p)_6(01p)_3(40c)_3(51c)_3(52c)", "_6(20p)_6(01p)_3(40c)_3(51w)_3(52c)", 
                                                               "_6(20p)_5(13p)_6(01p)_3(40c)_3(51w)_3(52c)", "_6(20p)_6(01p)_3(51c)_3(52c)_3(40c)", "_6(01p)_3(40w)_6(20a)_3(51c)_3(52c)",
                                                               "_6(20a)_6(01p)_3(40c)_3(51w)_3(52c)") , "_6(19x)_3(40x)_3(51x)_3(52c)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(17a)_6(01c)_3(40c)_3(71c)_3(72w)", "_6(19a)_6(01p)_3(40c)_3(51w)_3(52w)",
                                                               "_6(19a)_6(01p)_3(40c)_3(51c)_3(52w)", "_6(19a)_6(01c)_3(40c)_3(51c)_3(52w)",
                                                               "_6(19p)_6(01p)_3(40c)_3(51c)_3(52w)", "_6(19p)_6(01p)_3(40c)_3(51w)_3(52w)",
                                                               "_6(19p)_6(01p)_3(40w)_3(51c)_3(52w)", "_6(20a)_6(01p)_3(40c)_3(51c)_3(52w)",
                                                               "_6(20a)_6(01p)_3(40c)_3(51w)_3(52w)", 
                                                               "_6(20p)_6(01p)_3(40c)_3(51w)_3(52w)"), "_6(19x)_3(40x)_3(51x)_3(52w)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(10a)_3(12c)_3(22c)", "_6(10a)_3(12w)_3(22c)", "_6(10a)_3(11c)_3(11c)", 
                                                               "_6(10a)_5(00p)_3(12c)_3(22c)", "_6(10p)_3(00w)_3(00c)", "_6(10p)_3(12w)_3(22c)", "_6(10p)_3(12c)_3(22c)", 
                                                               "_6(10p)_3(11c)_3(22c)", "_3(12c)_3(22c)_6(10p)", "_6(10p)_3(12c)_4(1t)_3(22c)", "_6(10p)_3(13c)_3(22c)", 
                                                               "_6(10p)_5(00p)_3(12c)_3(22c)", "_6(10p)_3(12w)_3(22c)_3(22c)", "_6(10p)_5(00p)_3(12w)_3(22c)", 
                                                               "_3(12w)_3(22c)_6(10p)", "_3(12c)_6(10p)_3(22c)", "_6(11a)_3(12c)_3(22c)", "_6(11a)_3(12w)_3(22c)", 
                                                               "_3(12w)_3(22c)_6(11a)", "_6(11a)_3(11w)_3(22c)", "_3(12c)_6(11a)_3(22c)", "_6(11a)_3(12c)_4(1t)_3(22c)", 
                                                               "_6(11a)_3(11c)_3(11c)", "_6(11a)_3(12c)_3(12c)", "_6(11a)_3(11c)_3(12c)_3(22c)", "_6(11a)_3(12c)_4(0t)_3(22c)",
                                                               "_3(12w)_6(11a)_3(22c)", "_6(11p)_3(12c)_3(22c)", "_6(11p)_3(12w)_3(22c)", "_3(12c)_3(22c)_6(11p)",
                                                               "_3(12w)_3(22c)_6(11p)", "_3(12c)_6(11p)_3(22c)", "_6(11p)_3(11w)_3(22c)", "_6(11p)_3(11c)_3(22c)",
                                                               "_6(11p)_3(12w)_3(22c)_3(12c)", "_3(12w)_6(11p)_3(22c)", "_6(11p)_3(22c)_3(12c)", "_6(12p)_3(12c)_3(22c)",
                                                               "_6(12p)_3(12w)_3(22c)", "_3(12c)_3(22c)_6(12p)", "_6(12p)_5(33p)_3(12w)_3(22c)", "_6(12p)_3(11c)_3(22c)",
                                                               "_6(12p)_5(33p)_3(12c)_3(22c)", "_6(12p)_5(34p)_3(12c)_3(22c)", "_6(12p)_5(00p)_3(12w)_3(22c)",
                                                               "_6(13a)_5(13p)_3(12c)_3(22c)", "_6(14a)_3(12c)_3(22c)", "_6(14a)_3(12w)_3(22c)","_6(15a)_3(12c)_3(22c)", 
                                                               "_6(15a)_3(12w)_3(22c)", "_6(21p)_3(12c)_3(22c)", "_6(21p)_3(12w)_3(22c)", "_3(12c)_3(22c)_6(21p)", 
                                                               "_3(22c)_6(21p)_3(12c)", "_6(21p)_5(00p)_3(12c)_3(22c)", "_6(11a)_3(13c)_3(23c)", "_3(12c)_3(22c)_6(11a)",
                                                               "_6(11a)_3(11c)_3(22c)", "_6(12a)_3(12c)_3(22c)", "_6(12a)_3(12w)_3(22c)", "_6(12p)_5(00p)_3(12c)_3(22c)",
                                                               "_6(12p)_5(13p)_3(12c)_3(22c)", "_6(12p)_5(13p)_3(12w)_3(22c)", "_6(14p)_3(12c)_3(22c)", "_6(14p)_3(12w)_3(22c)", 
                                                               "_6(15p)_3(12c)_3(22c)", "_6(15p)_3(12w)_3(22c)", "_6(21a)_3(12c)_3(22c)", "_6(21a)_3(12w)_3(22c)", 
                                                               "_6(11a)_6(01p)_6(01p)_3(12c)_3(22c)", "_6(10p)_6(01p)_6(01p)_3(12w)_3(22c)",
                                                               "_6(11p)_6(01p)_6(01p)_3(12w)_3(22c)"), "_6(1x)_3(12x)_3(22c)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_3(12c)_6(11a)_3(22w)", "_3(12c)_6(11p)_3(22w)", "_6(10a)_3(12c)_3(22w)", 
                                                               "_6(10a)_3(12w)_3(22w)", "_6(10p)_3(11c)_3(22w)", "_6(10p)_3(12c)_3(22w)", "_6(10p)_3(12w)_3(22w)", 
                                                               "_6(10p)_5(13p)_3(12c)_3(22w)", "_6(10p)_5(13p)_3(12w)_3(22w)", "_6(11a)_3(12c)_3(22w)", 
                                                               "_6(11a)_3(12c)_4(1t)_3(22w)", "_6(11a)_3(12w)_3(22w)", "_6(11a)_3(13c)_3(22w)", "_6(11a)_3(22c)_3(22w)", 
                                                               "_6(11a)_5(13p)_3(12c)_3(22w)", "_6(11p)_3(11w)_3(11w)", "_6(11p)_3(12c)_3(22w)", "_6(11p)_3(12w)_3(22w)",
                                                               "_6(11p)_5(13p)_3(12w)_3(22w)", "_6(11p)_6(20p)_3(51c)_3(52c)_3(12w)_3(22w)", "_6(12a)_3(12c)_3(22w)", 
                                                               "_6(12a)_3(12w)_3(22w)", "_6(12p)_3(11c)_3(22w)", "_6(12p)_3(12c)_3(22w)", "_6(12p)_3(12w)_3(22w)", 
                                                               "_6(12p)_5(00p)_3(12c)_3(22w)", "_6(12p)_5(13p)_3(12w)_3(22w)", "_6(12p)_5(33p)_3(12c)_3(22w)", 
                                                               "_6(14a)_3(12c)_3(22w)", "_6(14a)_3(12w)_3(22w)", "_6(14p)_3(12w)_3(22w)", "_6(14p)_3(12c)_3(22w)", 
                                                               "_6(15a)_3(12c)_3(22w)", "_6(15a)_3(12w)_3(22w)", "_6(15p)_3(12w)_3(22w)", "_6(15p)_3(12c)_3(22w)",
                                                               "_6(21a)_3(12c)_3(22w)", "_6(21a)_3(12w)_3(22w)", "_6(21p)_3(12c)_3(22w)", "_6(21p)_3(12w)_3(22w)", 
                                                               "_6(12p)_5(13p)_3(12c)_3(22w)", "_6(11a)_3(11c)_3(22w)", 
                                                               "_6(10p)_6(01p)_6(01p)_3(12c)_3(22w)"), "_6(1x)_3(12x)_3(22w)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(01c)_3(40c)_6(11a)_3(12c)_3(22c)", "_6(01c)_3(40c)_6(14a)_3(12w)_3(22c)",
                                                               "_6(01p)_3(40c)_6(10p)_3(12c)_3(22c)", "_6(01p)_3(40c)_6(11a)_3(12c)_3(22c)", 
                                                               "_6(01p)_3(40c)_6(11p)_3(12c)_3(22c)", "_6(01p)_3(40c)_6(12p)_3(12c)_3(22c)", 
                                                               "_6(01p)_3(40c)_6(14a)_3(12c)_3(22c)", "_6(01p)_3(40w)_6(11p)_3(12c)_3(22c)", 
                                                               "_6(01p)_6(10p)_3(40c)_3(12c)_3(22c)", "_6(05p)_3(40c)_6(21p)_3(12w)_3(22c)", 
                                                               "_6(05p)_3(40w)_6(14a)_3(12c)_3(22c)", "_6(10a)_6(01c)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(10a)_6(01c)_3(40c)_3(12w)_3(22c)", "_6(10a)_6(01c)_3(40w)_3(12w)_3(22c)", 
                                                               "_6(10a)_6(01p)_3(40c)_3(12c)_3(22c)", "_6(10a)_6(01p)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(10a)_6(01p)_3(40w)_3(12c)_3(22c)", "_6(10a)_6(01p)_3(40w)_3(12w)_3(22c)", 
                                                               "_6(10p)_3(12c)_6(01c)_3(40c)_3(22c)", "_6(10p)_3(12c)_6(01c)_3(40w)_3(22c)", 
                                                               "_6(10p)_3(12c)_6(01p)_3(40c)_3(22c)", "_6(10p)_3(12c)_6(01p)_3(40w)_3(22c)", 
                                                               "_6(10p)_3(12c)_6(06t)_3(40c)_3(22c)", "_6(10p)_3(12w)_6(01c)_3(40c)_3(22c)", 
                                                               "_6(10p)_3(12w)_6(01p)_3(40c)_3(22c)", "_6(10p)_3(12w)_6(01p)_3(40w)_3(22c)", 
                                                               "_6(10p)_3(12w)_6(06t)_3(40c)_3(22c)", "_6(10p)_3(12w)_6(06u)_3(40c)_3(22c)", 
                                                               "_6(10p)_6(01c)_3(40c)_3(12c)_3(22c)", "_6(10p)_6(01c)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(10p)_6(01c)_3(40w)_3(12c)_3(22c)", "_6(10p)_6(01c)_3(40w)_3(12w)_3(22c)", 
                                                               "_6(10p)_6(01c)_6(01c)_3(40c)_3(40c)_3(12c)_3(22c)", "_6(10p)_6(01c)_6(01c)_3(40c)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(10p)_6(01c)_6(01c)_3(40w)_3(40w)_3(12w)_3(22c)", "_6(10p)_6(01c)_6(01p)_3(40c)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(10p)_6(01c)_6(01p)_3(40w)_3(40w)_3(12w)_3(22c)", "_6(10p)_6(01p)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(10p)_6(01p)_3(40c)_3(12c)_3(22c)_6(01p)_3(40c)", "_6(10p)_6(01p)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(10p)_6(01p)_3(40c)_6(01c)_3(40c)_6(01c)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(10p)_6(01p)_3(40c)_6(01p)_3(40w)_3(12c)_3(22c)", "_6(10p)_6(01p)_3(40w)_3(12c)_3(22c)", 
                                                               "_6(10p)_6(01p)_3(40w)_3(12w)_3(22c)", "_6(10p)_6(01p)_3(40w)_6(01p)_3(40c)_3(11c)_3(12c)", 
                                                               "_6(10p)_6(01p)_6(01c)_3(40c)_3(40c)_3(12c)_3(22c)", "_6(10p)_6(01p)_6(01p)_3(12w)_3(22c)", 
                                                               "_6(10p)_6(01p)_6(01p)_3(40c)_3(40c)_3(12c)_3(22c)", "_6(10p)_6(01p)_6(01p)_3(40c)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(10p)_6(01p)_6(01p)_3(40c)_3(40w)_3(12c)_3(22c)", 
                                                               "_6(10p)_6(01p)_6(01p)_6(01c)_3(40c)_3(40c)_3(40w)_3(12c)_3(22c)", 
                                                               "_6(10p)_6(01p)_6(01p)_6(01p)_3(40c)_3(12c)_3(22c)", "_6(10p)_6(01t)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(10p)_6(01t)_3(40c)_3(12w)_3(22c)", "_6(10p)_6(06t)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(10p)_6(06t)_3(40c)_3(12w)_3(22c)", "_6(10p)_6(06t)_3(40c)_6(01p)_3(40c)_6(01p)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(11a)_3(12c)_3(40c)_3(22c)_6(01p)", "_6(11a)_3(12c)_6(01c)_3(40c)_3(22c)", 
                                                               "_6(11a)_3(12c)_6(01c)_3(40w)_3(22c)", "_6(11a)_3(12c)_6(01c)_6(01c)_3(40c)_3(40w)_3(22c)", 
                                                               "_6(11a)_3(12c)_6(01p)_3(40c)_3(22c)", "_6(11a)_3(12c)_6(01p)_3(40w)_3(22c)", 
                                                               "_6(11a)_3(12c)_6(01p)_6(01p)_3(40c)_3(40w)_3(22c)", "_6(11a)_3(12c)_6(06t)_3(40c)_3(22c)", 
                                                               "_6(11a)_3(12w)_6(01c)_3(40c)_3(22c)", "_6(11a)_3(12w)_6(01c)_3(40w)_3(22c)", 
                                                               "_6(11a)_3(12w)_6(01p)_3(40c)_3(22c)", "_6(11a)_3(12w)_6(01p)_3(40w)_3(22c)", 
                                                               "_6(11a)_3(12w)_6(06t)_3(40c)_3(22c)", "_6(11a)_6(01c)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(11a)_6(01c)_3(40c)_3(12w)_3(22c)", "_6(11a)_6(01c)_3(40c)_6(01c)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(11a)_6(01c)_3(40c)_6(01c)_3(40c)_3(12w)_3(22c)", "_6(11a)_6(01c)_3(40c)_6(01p)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(11a)_6(01c)_3(40w)_3(12c)_3(22c)", "_6(11a)_6(01c)_3(40w)_3(12w)_3(22c)", 
                                                               "_6(11a)_6(01c)_6(01c)_3(40c)_3(40c)_3(12c)_3(22c)", "_6(11a)_6(01c)_6(01c)_3(40c)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(11a)_6(01p)_3(12c)_3(22c)_3(40c)", "_6(11a)_6(01p)_3(40c)_3(11c)_3(22c)", 
                                                               "_6(11a)_6(01p)_3(40c)_3(12c)_3(22c)", "_6(11a)_6(01p)_3(40c)_3(12c)_6(01p)_3(40c)_3(22c)",
                                                               "_6(11a)_6(01p)_3(40c)_3(12w)_3(22c)", "_6(11a)_6(01p)_3(40c)_6(01c)_3(40c)_3(12c)_3(22c)",
                                                               "_6(11a)_6(01p)_3(40c)_6(01p)_3(40c)_3(12c)_3(22c)", "_6(11a)_6(01p)_3(40w)_3(12c)_3(22c)", 
                                                               "_6(11a)_6(01p)_3(40w)_3(12w)_3(22c)", "_6(11a)_6(01p)_3(40w)_6(01p)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(11a)_6(01p)_6(01c)_3(40c)_3(40c)_3(12c)_3(22c)", "_6(11a)_6(01p)_6(01c)_3(40c)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(11a)_6(01p)_6(01p)_3(12c)_3(22c)", "_6(11a)_6(01p)_6(01p)_3(40c)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(11a)_6(01p)_6(01p)_3(40c)_3(40c)_3(12w)_3(22c)", "_6(11a)_6(01p)_6(01p)_3(40c)_3(40w)_3(12c)_3(22c)", 
                                                               "_6(11a)_6(01p)_6(01p)_3(40w)_3(40c)_3(12c)_3(22c)", "_6(11a)_6(01p)_6(01p)_6(01p)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(11a)_6(01p)_6(01p)_6(01p)_3(40c)_3(40c)_3(40w)_3(12c)_3(22c)", "_6(11a)_6(01t)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(11a)_6(01t)_3(40c)_3(12w)_3(22c)", "_6(11a)_6(01t)_3(40c)_6(01p)_3(40w)_3(12c)_3(22c)", 
                                                               "_6(11a)_6(06t)_3(12c)_3(22c)_3(40c)", "_6(11a)_6(06t)_3(40w)_3(12c)_3(22c)", 
                                                               "_6(11a)_6(06u)_3(40c)_3(12c)_3(22c)", "_6(11p)_3(11c)_6(01p)_3(40w)_3(11c)", 
                                                               "_6(11p)_3(12c)_6(01c)_3(40c)_3(22c)", "_6(11p)_3(12c)_6(01c)_3(40w)_3(22c)", 
                                                               "_6(11p)_3(12c)_6(01p)_3(40c)_3(22c)", "_6(11p)_3(12c)_6(01p)_3(40w)_3(22c)", "_6(11p)_3(12c)_6(06t)_3(40c)_3(22c)", 
                                                               "_6(11p)_3(12w)_6(01p)_3(40c)_3(22c)", "_6(11p)_3(12w)_6(01p)_3(40w)_3(22c)", "_6(11p)_3(12w)_6(06t)_3(40c)_3(22c)", 
                                                               "_6(11p)_6(01c)_3(40c)_3(12c)_3(22c)", "_6(11p)_6(01c)_3(40c)_3(12w)_3(22c)", "_6(11p)_6(01c)_3(40c)_6(01p)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(11p)_6(01c)_3(40w)_3(12c)_3(22c)", "_6(11p)_6(01c)_3(40w)_3(12w)_3(22c)", "_6(11p)_6(01c)_6(01c)_3(40c)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(11p)_6(01c)_6(01c)_3(40w)_3(40c)_3(12c)_3(22c)", "_6(11p)_6(01p)_3(40c)_3(12c)_3(22c)", "_6(11p)_6(01p)_3(40c)_3(12w)_3(22c)",
                                                               "_6(11p)_6(01p)_3(40c)_6(01c)_3(40c)_3(12c)_3(22c)", "_6(11p)_6(01p)_3(40c)_6(01p)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(11p)_6(01p)_3(40c)_6(01p)_3(40c)_3(12w)_3(22c)", "_6(11p)_6(01p)_3(40w)_3(12c)_3(22c)", "_6(11p)_6(01p)_3(40w)_3(12w)_3(22c)", 
                                                               "_6(11p)_6(01p)_3(40w)_6(01p)_3(40c)_3(12c)_3(22c)", "_6(11p)_6(01p)_6(01c)_3(40c)_3(40w)_3(12c)_3(22c)", 
                                                               "_6(11p)_6(01p)_6(01c)_3(40w)_3(40c)_3(12c)_3(22c)", "_6(11p)_6(01p)_6(01p)_3(12w)_3(22c)", 
                                                               "_6(11p)_6(01p)_6(01p)_3(40c)_3(40c)_3(12c)_3(22c)", "_6(11p)_6(01p)_6(01p)_3(40c)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(11p)_6(01p)_6(01p)_3(40c)_3(40w)_3(12c)_3(22c)", "_6(11p)_6(01p)_6(01p)_3(40c)_3(40w)_3(12w)_3(22c)", 
                                                               "_6(11p)_6(01p)_6(01p)_3(40w)_3(40c)_3(12c)_3(22c)", "_6(11p)_6(01t)_3(40c)_3(12c)_3(22c)", "_6(11p)_6(06t)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(11p)_6(06t)_3(40c)_3(12w)_3(22c)", "_6(12p)_3(12c)_6(01c)_3(40c)_3(22c)", "_6(12p)_3(12c)_6(01c)_3(40w)_3(22c)", 
                                                               "_6(12p)_3(12c)_6(01p)_3(40c)_3(22c)", "_6(12p)_3(12w)_6(01c)_3(40c)_3(22c)", "_6(12p)_3(12w)_6(01p)_3(40c)_3(22c)", 
                                                               "_6(12p)_5(13p)_6(01c)_3(40c)_3(12c)_3(22c)", "_6(12p)_6(01c)_3(40c)_3(12c)_3(22c)", "_6(12p)_6(01c)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(12p)_6(01c)_3(40w)_3(12c)_3(22c)", "_6(12p)_6(01c)_6(01c)_3(40c)_3(40c)_3(12c)_3(22c)", "_6(12p)_6(01p)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(12p)_6(01p)_3(40c)_3(12w)_3(22c)", "_6(12p)_6(01p)_3(40w)_3(12c)_3(22c)", "_6(12p)_6(01p)_3(40w)_3(12w)_3(22c)", 
                                                               "_6(12p)_6(01p)_6(01p)_3(40c)_3(40c)_3(12c)_3(22c)", "_6(12p)_6(01p)_6(01p)_3(40c)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(12p)_6(01t)_3(40c)_3(12w)_3(22c)", "_6(12p)_6(06t)_3(40c)_3(12c)_3(22c)", "_6(13a)_5(13p)_6(01p)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(14a)_3(12c)_6(01p)_3(40c)_3(22c)", "_6(14a)_6(01p)_3(40c)_3(12c)_3(22c)", "_6(15p)_6(01p)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(21a)_3(12c)_6(01p)_3(40c)_3(22c)", "_6(21p)_3(12c)_3(40c)_3(22c)_6(01p)", "_6(21p)_3(12c)_6(01c)_3(40c)_3(22c)", 
                                                               "_6(21p)_3(12c)_6(01p)_3(40c)_3(22c)", "_6(21p)_3(12c)_6(01p)_3(40w)_3(22c)", "_6(21p)_3(12c)_6(06t)_3(40c)_3(22c)", 
                                                               "_6(21p)_3(12w)_6(01p)_3(40c)_3(22c)", "_6(21p)_6(01c)_3(40c)_3(12c)_3(22c)", "_6(21p)_6(01c)_3(40c)_3(12c)_3(22c)_6(01c)_3(40c)", 
                                                               "_6(21p)_6(01c)_3(40c)_3(12w)_3(22c)", "_6(21p)_6(01c)_6(01c)_3(40c)_3(40c)_3(12c)_3(22c)", "_6(21p)_6(01p)_3(40c)_3(12c)_3(22c)", 
                                                               "_6(21p)_6(01p)_3(40c)_3(12w)_3(22c)", "_6(21p)_6(01p)_3(40w)_3(12c)_3(22c)", "_6(21p)_6(01p)_3(40w)_3(12w)_3(22c)", 
                                                               "_6(21p)_6(01p)_6(01p)_3(40c)_3(40c)_3(12c)_3(22c)", "_6(21p)_6(06t)_3(40c)_3(12w)_3(22c)", 
                                                               "_6(21p)_6(06t)_3(40w)_3(12c)_3(22c)", "_6(11a)_6(06t)_3(40c)_3(12c)_3(22c)"), "_6(1x)_3(40x)_3(12x)_3(22c)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(01p)_3(40c)_6(10p)_3(12c)_3(22w)", "_6(01p)_3(40c)_6(10p)_3(12w)_3(22w)", "_6(01p)_3(40c)_6(11a)_3(12c)_3(22w)", 
                                                               "_6(01p)_3(40c)_6(11p)_3(12c)_3(22w)", "_6(01p)_3(40c)_6(14a)_3(12w)_3(22w)", "_6(05p)_3(40c)_6(14a)_3(12w)_3(22w)", 
                                                               "_6(05p)_3(40w)_6(14a)_3(12c)_3(22w)", "_6(10a)_6(01c)_3(40c)_3(12c)_3(22w)", "_6(10a)_6(01p)_3(40c)_3(12c)_3(22w)", 
                                                               "_6(10a)_6(01p)_3(40c)_3(12w)_3(22w)", "_6(10a)_6(01p)_3(40w)_3(12c)_3(22w)", "_6(10a)_6(01p)_3(40w)_3(12w)_3(22w)", 
                                                               "_6(10p)_3(12c)_6(01c)_3(40c)_3(22w)", "_6(10p)_3(12c)_6(01p)_3(40c)_3(22w)", "_6(10p)_3(12c)_6(01p)_3(40w)_3(22w)", 
                                                               "_6(10p)_3(12w)_6(01c)_3(40c)_3(22w)", "_6(10p)_3(12w)_6(01p)_3(40c)_3(22w)", "_6(10p)_3(12w)_6(06t)_3(40c)_3(22w)", 
                                                               "_6(10p)_6(01c)_3(40c)_3(12c)_3(22w)", "_6(10p)_6(01c)_3(40c)_3(12w)_3(22w)", "_6(10p)_6(01c)_3(40w)_3(12c)_3(22w)", 
                                                               "_6(10p)_6(01c)_3(40w)_3(12w)_3(22w)", "_6(10p)_6(01p)_3(40c)_3(12c)_3(22w)", "_6(10p)_6(01p)_3(40c)_3(12w)_3(22w)",
                                                               "_6(10p)_6(01p)_3(40c)_6(01p)_3(40c)_3(12c)_3(22w)", "_6(10p)_6(01p)_3(40w)_3(12c)_3(22w)", "_6(10p)_6(01p)_3(40w)_3(12w)_3(22w)", 
                                                               "_6(10p)_6(01p)_6(01c)_3(40w)_3(40c)_3(12c)_3(22w)", "_6(10p)_6(01p)_6(01p)_3(12c)_3(22w)", 
                                                               "_6(10p)_6(01p)_6(01p)_3(40c)_3(40c)_3(12c)_3(22w)", "_6(10p)_6(01p)_6(01p)_3(40c)_3(40c)_3(12w)_3(22w)", 
                                                               "_6(10p)_6(01t)_3(40c)_3(12w)_3(22w)", "_6(10p)_6(01t)_3(40w)_3(12w)_3(22w)", "_6(10p)_6(06t)_3(40c)_3(12w)_3(22w)", 
                                                               "_6(11a)_3(12c)_6(01c)_3(40c)_3(22w)", "_6(11a)_3(12c)_6(01p)_3(40c)_3(22w)", "_6(11a)_3(12c)_6(01p)_3(40w)_3(22w)", 
                                                               "_6(11a)_3(12w)_6(01c)_3(40c)_3(22w)", "_6(11a)_3(12w)_6(01p)_3(40c)_3(22w)", "_6(11a)_3(12w)_6(01p)_3(40w)_3(22w)", 
                                                               "_6(11a)_3(12w)_6(06t)_3(40c)_3(22w)", "_6(11a)_6(01c)_3(40c)_3(12c)_3(22w)", "_6(11a)_6(01c)_3(40c)_3(12w)_3(22w)", 
                                                               "_6(11a)_6(01c)_3(40c)_6(01c)_3(40c)_3(12c)_3(22w)", "_6(11a)_6(01c)_3(40w)_3(12c)_3(22w)", "_6(11a)_6(01c)_3(40w)_3(12w)_3(22w)", 
                                                               "_6(11a)_6(01c)_6(01c)_3(40c)_3(40c)_3(12c)_3(22w)", "_6(11a)_6(01p)_3(40c)_3(12c)_3(22w)", "_6(11a)_6(01p)_3(40c)_3(12w)_3(22w)", 
                                                               "_6(11a)_6(01p)_3(40c)_6(01p)_3(40c)_3(12w)_3(22w)", "_6(11a)_6(01p)_3(40c)_6(01t)_3(40c)_3(12w)_3(22w)", 
                                                               "_6(11a)_6(01p)_3(40c)_6(06t)_3(40c)_3(12w)_3(22w)", "_6(11a)_6(01p)_3(40w)_3(12c)_3(22w)", "_6(11a)_6(01p)_3(40w)_3(12w)_3(22w)", 
                                                               "_6(11a)_6(01p)_3(40w)_3(13c)_3(23c)_3(33w)", "_6(11a)_6(01p)_6(01p)_3(40c)_3(40c)_3(12c)_3(22w)", "_6(11a)_6(01t)_3(40c)_3(12c)_3(22w)", 
                                                               "_6(11a)_6(06t)_3(40c)_3(12c)_3(22w)", "_6(11a)_6(06t)_3(40c)_3(12w)_3(22w)", "_6(11a)_6(19p)_3(51c)_3(52c)_3(12w)_3(22w)", 
                                                               "_6(11p)_3(12c)_6(01p)_3(40c)_3(22w)", "_6(11p)_3(12c)_6(06t)_3(40c)_3(22w)", "_6(11p)_3(12w)_6(01c)_3(40c)_3(22w)", 
                                                               "_6(11p)_3(12w)_6(01p)_3(40c)_3(22w)", "_6(11p)_3(12w)_6(01p)_3(40w)_3(22w)", "_6(11p)_3(12w)_6(01t)_3(40c)_3(22w)", 
                                                               "_6(11p)_6(01c)_3(40c)_3(12c)_3(22w)", "_6(11p)_6(01c)_3(40c)_3(12w)_3(22w)", "_6(11p)_6(01c)_6(01c)_3(40c)_3(40c)_3(12c)_3(22w)",
                                                               "_6(11p)_6(01p)_3(40c)_3(12c)_3(22w)", "_6(11p)_6(01p)_3(40c)_3(12w)_3(22w)", "_6(11p)_6(01p)_3(40w)_3(12c)_3(22w)", 
                                                               "_6(11p)_6(01p)_3(40w)_3(12w)_3(22w)", "_6(11p)_6(01p)_6(01p)_3(40c)_3(40w)_3(12c)_3(22w)", "_6(11p)_6(06t)_3(40c)_3(12c)_3(22w)", 
                                                               "_6(12p)_6(01c)_3(40c)_3(12c)_3(22w)", "_6(12p)_6(01c)_3(40w)_3(12c)_3(22w)", "_6(12p)_6(01c)_3(40w)_3(12w)_3(22w)", 
                                                               "_6(12p)_6(01p)_3(40c)_3(12c)_3(22w)", "_6(12p)_6(01p)_3(40c)_3(12w)_3(22w)", "_6(12p)_6(01p)_3(40w)_3(12c)_3(22w)", 
                                                               "_6(12p)_6(01p)_3(40w)_3(12w)_3(22w)", "_6(12p)_6(06t)_3(40c)_3(12c)_3(22w)", "_6(21p)_3(12w)_6(01p)_3(40c)_3(22w)", 
                                                               "_6(21p)_6(01p)_3(40c)_3(12c)_3(22w)", "_6(21p)_6(01p)_3(40w)_3(12c)_3(22w)", 
                                                               "_6(21p)_6(01p)_6(01c)_3(40c)_3(40c)_3(12c)_3(22w)"), "_6(1x)_3(40x)_3(12x)_3(22w)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_3(13c)_3(23c)_3(33c)_6(11a)", "_3(13c)_3(23c)_3(33c)_6(11p)", "_3(13w)_3(23c)_3(33c)_6(11a)", "_3(13w)_3(23c)_6(11p)_3(33c)", 
                                                               "_6(10a)_3(13c)_3(23c)_3(33c)", "_6(10a)_3(13c)_3(23w)_3(33c)", "_6(10a)_3(13w)_3(23c)_3(33c)", "_6(10a)_3(13w)_3(23w)_3(33c)", 
                                                               "_6(10p)_3(13c)_3(23c)_3(33c)", "_6(10p)_3(13c)_3(23w)_3(33c)", "_6(10p)_3(13w)_3(23c)_3(33c)", "_6(10p)_3(13w)_3(23w)_3(33c)", 
                                                               "_6(11a)_3(11c)_3(23c)_3(33c)", "_6(11a)_3(12c)_3(12c)_3(33c)", "_6(11a)_3(12c)_3(22c)_3(12c)", "_6(11a)_3(12c)_3(22w)_3(33c)", 
                                                               "_6(11a)_3(12c)_3(23c)_3(33c)", "_6(11a)_3(12w)_3(22c)_3(12c)", "_6(11a)_3(12w)_3(23c)_3(33c)", "_6(11a)_3(13c)_3(23c)_3(33c)",
                                                               "_6(11a)_3(13c)_3(23w)_3(33c)", "_6(11a)_3(13w)_3(23c)_3(33c)", "_6(11a)_3(13w)_3(23w)_3(33c)", "_6(11p)_3(12c)_3(22c)_3(12c)", 
                                                               "_6(11p)_3(12c)_3(22c)_3(33c)", "_6(11p)_3(13c)_3(23c)_3(33c)", "_6(11p)_3(13c)_3(23w)_3(33c)", "_6(11p)_3(13c)_3(33c)", 
                                                               "_6(11p)_3(13w)_3(23c)_3(33c)", "_6(11p)_3(13w)_3(23w)_3(33c)", "_6(12p)_3(13c)_3(23c)_3(33c)", 
                                                               "_6(21a)_3(13c)_3(23c)_3(33c)"), "_6(1x)_3(13x)_3(23x)_3(33c)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(10a)_3(13c)_3(23c)_3(33w)", "_6(10a)_3(13w)_3(23c)_3(33w)", "_6(10a)_3(13w)_3(23w)_3(33w)", "_6(10p)_3(13c)_3(23c)_3(33w)", 
                                                               "_6(10p)_3(13w)_3(23c)_3(33w)", "_6(10p)_3(13w)_3(23w)_3(33w)", "_6(11a)_3(13c)_3(23c)_3(33w)", "_6(11a)_3(13c)_3(23w)_3(33w)", 
                                                               "_6(11a)_3(13w)_3(23c)_3(33w)", "_6(11a)_3(13w)_3(23w)_3(33w)", "_6(11p)_3(13c)_3(22c)_3(33w)", "_6(11p)_3(13c)_3(23c)_3(33w)", 
                                                               "_6(11p)_3(13c)_3(23w)_3(33w)", "_6(11p)_3(13w)_3(23c)_3(33w)", "_6(11a)_3(12w)_3(22w)_3(11c)",
                                                               "_6(11p)_3(13w)_3(23w)_3(33w)"), "_6(1x)_3(13x)_3(23x)_3(33w)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(10p)_3(13c)_3(23c)_6(01c)_3(40c)_3(33c)", "_6(11a)_3(13c)_3(23c)_6(01c)_3(40c)_3(33c)", 
                                                               "_6(11a)_3(13c)_3(23c)_6(01p)_3(40c)_3(33c)", "_6(11a)_3(13c)_6(01c)_3(40c)_3(23c)_3(33c)", 
                                                               "_6(11a)_3(13c)_6(01p)_3(40c)_3(23c)_3(33c)", "_6(11a)_6(01c)_3(40c)_3(13c)_3(23c)_3(33c)", 
                                                               "_6(11a)_6(01c)_3(40c)_3(13c)_3(23w)_3(33c)", "_6(11a)_6(01p)_3(40c)_3(13c)_3(23c)_3(33c)", 
                                                               "_6(11a)_6(01p)_3(40c)_3(13c)_3(23w)_3(33c)", "_6(11a)_6(01p)_3(40c)_3(13w)_3(23c)_3(33c)", 
                                                               "_6(11a)_6(01p)_3(40w)_3(13c)_3(23c)_3(33c)", "_6(11a)_6(01p)_3(40w)_3(13c)_3(23w)_3(33c)", 
                                                               "_6(11a)_6(06t)_3(40c)_3(13c)_3(23c)_3(33c)", "_6(11p)_3(13c)_3(23c)_6(01p)_3(40c)_3(33c)", 
                                                               "_6(11p)_3(13c)_3(23w)_6(01c)_3(40c)_3(33c)", "_6(11p)_3(13c)_3(23w)_6(01p)_3(40c)_3(33c)", 
                                                               "_6(11p)_3(13c)_6(01p)_3(40c)_3(23c)_3(33c)", "_6(11p)_6(01c)_3(40c)_3(13c)_3(23c)_3(33c)", 
                                                               "_6(11p)_6(01c)_3(40c)_3(13w)_3(23w)_3(33c)", "_6(11p)_6(01p)_3(40c)_3(13c)_3(23c)_3(33c)", 
                                                               "_6(11p)_6(01p)_3(40c)_3(13c)_3(23w)_3(33c)", "_6(11p)_6(01p)_3(40c)_3(13w)_3(23w)_3(33c)", 
                                                               "_6(11p)_6(01p)_3(40w)_3(13c)_3(23c)_3(33c)", "_6(11p)_6(01p)_3(40w)_3(13c)_3(23w)_3(33c)", 
                                                               "_6(11p)_6(06t)_3(40c)_3(13c)_3(23c)_3(33c)", "_6(11p)_6(06u)_3(40c)_3(13c)_3(23w)_3(33c)",
                                                               "_6(11p)_6(06t)_3(40c)_3(13c)_3(23w)_3(33c)"), "_6(1x)_3(40x)_3(13x)_3(23x)_3(33c)", codf),
                  
                  codf = ifelse(is.na(codf) & clean_cod %in% c("_6(11a)_3(13c)_3(23w)_6(01p)_3(40c)_3(33w)", "_6(11a)_6(01c)_3(40c)_3(13c)_3(23c)_3(33w)", 
                                                               "_6(11a)_6(01p)_3(40c)_3(13c)_3(23c)_3(33w)", "_6(11p)_6(01p)_3(40c)_3(13c)_3(23c)_3(33w)",
                                                               "_6(11p)_6(01p)_3(40c)_3(13c)_3(23w)_3(33w)", "_6(11p)_6(01p)_3(40c)_3(13w)_3(23w)_3(33w)"), "_6(1x)_3(40x)_3(13x)_3(23x)_3(33w)", codf)) %>%
    
    dplyr::mutate(codf = ifelse(stringr::str_detect(cods, "_6\\(4") & codf == "", "_6(0x)", codf),
                  codf = ifelse(codf %in% c("_10", "", "_5(33p)", "_5(35p)") & stringr::str_detect(cods, '_7\\(4'), "_7(4p)", codf),
                  codf = ifelse(codf %in% c("_12(0)", "_12(0)_10(0)") & stringr::str_detect(cods, '_7\\(4'), "_12(0)_7(4p)", codf),
                  codf = ifelse(codf %in% c("_12(1)", "_12(1)_10(0)") & stringr::str_detect(cods, '_7\\(4'), "_12(1)_7(4p)", codf))
  
}

tokenizer_plays <- function(df){
  
  obj <- pbp %>% 
    dplyr::group_by(game_id, period, clock) %>%
    dplyr::mutate(n = dplyr::n(),
                  joincod = paste0(cod, collapse = ""),
                  row = dplyr::row_number()) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(importance = 1)
  
  obj2 <- obj %>% 
    dplyr::group_by(joincod, row) %>% 
    dplyr::summarise(cod = unique(cod), 
                     n = unique(n), 
                     importance = unique(importance), 
                     cnt = dplyr::n(), 
                     .groups = 'drop') %>%
    nulling_cods_always_null() %>% 
    put_features_cod() %>% 
    dplyr::mutate(valcod = stringr::str_replace_all(valcod, '(_)(6\\(0)', '\\11\\2'),
                  valcod = stringr::str_replace_all(valcod, '(_)(3\\(4)', '\\13\\2')) %>% 
    dplyr::mutate(modcod = stringr::str_remove_all(valcod, "\\(.{1,3}\\)")) %>% 
    dplyr::mutate(total_modcod = (row == 1) * cnt) %>% 
    dplyr::group_by(modcod) %>% 
    dplyr::mutate(total_modcod = sum(total_modcod)) %>% 
    dplyr::ungroup() %>% 
    nulling_cods_should_be_null() %>% 
    fix_plays_after_and_before_period() %>% 
    dplyr::mutate(aux = ifelse(importance == 0, -1, 0)) %>% 
    dplyr::mutate(importance = ifelse(stringr::str_detect(cod, '_18') & 
                                        stringr::str_detect(dplyr::lag(cod), "_18", negate = TRUE) & 
                                        stringr::str_detect(next_cod, '_13'), 1, importance)) %>% 
    dplyr::mutate(aux = ifelse(stringr::str_detect(cod, "_4\\(.t\\)|_4\\(0u") & 
                                 last_cod == '_3(40w)' & 
                                 stringr::str_detect(next_cod, "_6\\("), 1, aux),
                  aux = ifelse(stringr::str_detect(cod, "_6\\(0") & 
                                 stringr::str_detect(last_cod, '_3\\(') & 
                                 next_cod == "", 2, aux),
                  aux = ifelse(stringr::str_detect(next_cod, "_3") & 
                                 stringr::str_detect(last_cod, "_6") & 
                                 stringr::str_detect(cod, "_5"), 3, aux)) %>% 
    dplyr::mutate(importance = ifelse(aux > 0, 0, importance)) %>% 
    dplyr::mutate(ind_foul = ifelse(aux >= 0 & 
                                      stringr::str_detect(cod, "6\\(") & 
                                      stringr::str_detect(valcod, "_6\\((1|2)") & 
                                      stringr::str_detect(valcod, "_3\\((1|2|3|5|6|7|8|9)"), 1, 0),
                  ind_foul = ifelse(aux >= 0 & 
                                      stringr::str_detect(cod, "6\\(") & 
                                      stringr::str_detect(valcod, "_16\\(0") & 
                                      stringr::str_detect(valcod, "_33\\(4", negate = T), ind_foul + 10, ind_foul),
                  ind_foul = ifelse(aux >= 0 & 
                                      stringr::str_detect(cod, "6\\(") & 
                                      stringr::str_detect(valcod, "_16\\(0") & 
                                      stringr::str_detect(valcod, "_33\\(4"), ind_foul + 100, ind_foul))  
  
  
  # REF É O GRUPO QUE DESEJAMOS MOVER A AÇÃO
  # SUM_JOIN É O GRUPO ATUAL DA AÇÃO
  # A AÇÃO SÓ PODE MOVER PARA O GRUPO SEGUINTE
  
  # MOVA A AÇÃO SE 
  ## HÁ UMA AÇÃO DE INTERESSE NO GRUPO SEGUINTE (EXEMPLO FALTA), >> (ou seja, se ref == 1000, não há uma ação de interesse no grupo seguinte)
  ## E ESSA AÇÃO PODE SER MOVIDA (join = 0), 
  ## E SE O GRUPO FUTURO É SEGUINTE AO GRUPO ATUAL (sum_join + 1) == ref
  ## E SE O GRUPO ATUAL (sum_join) NÃO É O IDENTIFICADO COMO GRUPO 0 (sum_join != 0, isso já está tratado em outra parte do código)
  ## E SE EXISTIR UM GRUPO EM si SEGUINTE AO ATUAL DA AÇÃO A SER MOVIDA (ou seja, si < max(si))
  
  
  obj3 <- obj2 %>% 
    dplyr::rename(realcod = valcod) %>% 
    put_features_cod() %>% 
    dplyr::select(-valcod) %>%
    assign_specific_turnovers_situations() %>% 
    assign_specific_tech_fouls_situations() %>% 
    dplyr::mutate(importance = ifelse(aux > 0, 0, importance)) %>% 
    put_features_cod() %>% 
    dplyr::mutate(aux = ifelse(stringr::str_detect(cod, "_6") & 
                                 stringr::str_detect(last_cod, "_6\\(0"), 40, aux)) %>% 
    assign_specific_modcod_situations() %>% 
    dplyr::mutate(importance = ifelse(aux > 0, 0, importance),
                  importance = ifelse(aux %in% c(50, 51, 56, 57), 1, importance)) %>% 
    dplyr::mutate(aux = ifelse(total_modcod <= 1, -2, aux)) %>% 
    dplyr::select(-valcod)
  
  obj4 <- obj3 %>%
    dplyr::group_by(joincod) %>% 
    dplyr::mutate(si = cumsum(importance)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(si = ifelse(si == 0, 1, si)) %>% 
    group_plays_around_jumpball() %>% 
    group_plays_around_fouls() %>% 
    group_plays_around_turnovers()
  
  obj5 <- obj4 %>% 
    dplyr::mutate(aux = ifelse(modcod %in% c('_6_1_3', '_3_3_4_6', '_3', 
                                             '_2_4_6_1_3', '_6_3_10_3_4'), -2, aux)) %>% 
    dplyr::filter(aux != -2) %>%
    dplyr::group_by(joincod, cnt, si) %>% 
    dplyr::mutate(cods = paste0(cod, collapse = "")) %>% 
    dplyr::mutate(clean_cod = stringr::str_remove_all(cods, "_(7|8|9|11|18)\\(.{1,3}\\)"),
                  clean_cod = stringr::str_replace_all(clean_cod, "(_3\\(.{1,2}w\\))(_4\\(.{1,3}\\))", "\\1"),
                  clean_cod = stringr::str_remove_all(clean_cod, "_6\\((4.{1,2}|18a)\\)")) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(joincod) %>% 
    dplyr::mutate(maxsi = max(si)) %>% 
    dplyr::ungroup() %>% 
    simplify_cod()
  
  obj6 <- obj5 %>% 
    dplyr::select(joincod, row, si, codf)
  
  obj7 <- obj %>% 
    dplyr::left_join(obj6, by = c('joincod', 'row')) %>%
    dplyr::group_by(season, game_id, period, clock, si, codf) %>% 
    dplyr::summarise(joinpcod = paste0(pcod, collapse = ""), .groups = 'drop')
  
}


obj7 %>% 
  dplyr::group_by(joinpcod, codf) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop')

obj7 %>% 
  dplyr::mutate(pcodf = NA_character_,
                pcodf = ifelse(stringr::str_detect(codf, "_1\\("), stringr::str_extract(joinpcod, "p[0-9]{3}_1\\(.{1,3}\\)"), pcodf),
                
                pcodf = ifelse(stringr::str_detect(codf, "_2\\("), stringr::str_extract(joinpcod, "p[0-9]{3}_2\\(.{1,3}\\)"), pcodf),
                
                pcodf = ifelse(codf %in% c('_10', "_12(0)_10(0)", "_12(1)_10(0)"), stringr::str_extract(joinpcod, "p[0-9]{3}_10\\(.\\)"), pcodf),
                
                pcodf = ifelse(codf %in% c('_12(0)', '_12(1)', '_12(2)', '_12(3)', '_12(4)'), stringr::str_extract(joinpcod, "p[0-9]{3}_12\\(.\\)"), pcodf),
                pcodf = ifelse(codf %in% c("_12(0)_7(4p)", "_12(0)_7(4p)"), stringr::str_extract(joinpcod, "p[0-9]{3}_7\\(4p\\)"), pcodf),
                
                pcodf = ifelse(codf %in% c('_13(0)', '_13(1)', '_13(2)', '_13(3)', '_13(4)'), stringr::str_extract(joinpcod, "p[0-9]{3}_13\\(.\\)"), pcodf),
                
                pcodf = ifelse(codf %in% c('_4(0p)', '_4(0t)', '_4(1t)'), stringr::str_extract(joinpcod, "p[0-9]{3}_4\\(.{1,3}\\)"), pcodf),
                
                pcodf = ifelse(codf %in% c('_5(01p)'), stringr::str_extract(joinpcod, "p[0-9]{3}_5\\(.{1,3}\\)"), pcodf),
  )



sort_modcod <- function(string){
  
  string %>% 
    stringr::str_replace_all(., "_", ",_") %>% 
    stringr::str_remove(., "\\,") %>%  
    stringr::str_split(., "\\,") %>% 
    unlist() %>% 
    sort() %>% 
    paste0(collapse = '')
  
}

x0 <- obj3 %>% 
  dplyr::distinct(modcod, joincod, cnt) %>% 
  dplyr::group_by(modcod) %>% 
  dplyr::summarise(cnt = sum(cnt)) %>% 
  dplyr::arrange(cnt) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(sortmodcod = sort_modcod(modcod)) %>% 
  dplyr::ungroup()

x0 %>% 
  dplyr::arrange(sortmodcod, desc(cnt)) %>% 
  dplyr::group_by(sortmodcod) %>% 
  dplyr::mutate(num_linhas = dplyr::n()) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(modcod_provavel = modcod,
                cnt_modcod_provavel = cnt) %>% 
  dplyr::left_join(x0, ., by = 'sortmodcod') %>% 
  dplyr::select(-sortmodcod) %>%
  dplyr::arrange(desc(cnt_modcod_provavel), modcod_provavel, desc(cnt)) %>% 
  dplyr::filter(num_linhas > 1) %>% 
  print(., n = 100)

