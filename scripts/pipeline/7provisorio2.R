
# git token
#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

require(tidyverse)
load("D:/Mestrado/NBA/data/pbp_jogadas.RData")

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


obj <- pbp_jogadas %>% 
  dplyr::group_by(game_id, period, clock) %>%
  dplyr::mutate(n = dplyr::n(),
                joincod = paste0(cod, collapse = ""),
                row = dplyr::row_number()) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(importance = 1)

# aux here solves only specific problems, maybe move this
obj2 <- obj %>% 
  dplyr::group_by(joincod, row) %>% 
  dplyr::summarise(cod = unique(cod), 
                   n = unique(n), 
                   importance = unique(importance), 
                   cnt = dplyr::n(), .groups = 'drop') %>%
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
  dplyr::mutate(importance = ifelse(stringr::str_detect(cod, '_18') & stringr::str_detect(dplyr::lag(cod), "_18", negate = TRUE) & stringr::str_detect(next_cod, '_13'), 1, importance)) %>% 
  dplyr::mutate(aux = ifelse(stringr::str_detect(cod, "_4\\(.t\\)|_4\\(0u") & last_cod == '_3(40w)' & stringr::str_detect(next_cod, "_6\\("), 1, aux),
                aux = ifelse(stringr::str_detect(cod, "_6\\(0") & stringr::str_detect(last_cod, '_3\\(') & next_cod == "", 2, aux),
                aux = ifelse(stringr::str_detect(next_cod, "_3") & stringr::str_detect(last_cod, "_6") & stringr::str_detect(cod, "_5"), 3, aux)) %>% 
  dplyr::mutate(importance = ifelse(aux > 0, 0, importance)) %>% 
  dplyr::mutate(ind_foul = ifelse(aux >= 0 & stringr::str_detect(cod, "6\\(") & stringr::str_detect(valcod, "_6\\((1|2)") & stringr::str_detect(valcod, "_3\\((1|2|3|5|6|7|8|9)"), 1, 0),
                ind_foul = ifelse(aux >= 0 & stringr::str_detect(cod, "6\\(") & stringr::str_detect(valcod, "_16\\(0") & stringr::str_detect(valcod, "_33\\(4", negate = T), ind_foul + 10, ind_foul),
                ind_foul = ifelse(aux >= 0 & stringr::str_detect(cod, "6\\(") & stringr::str_detect(valcod, "_16\\(0") & stringr::str_detect(valcod, "_33\\(4"), ind_foul + 100, ind_foul))

obj3 <- obj2 %>% 
  dplyr::rename(realcod = valcod) %>% 
  put_features_cod() %>% 
  dplyr::select(-valcod) %>%
  assign_specific_turnovers_situations() %>% 
  assign_specific_tech_fouls_situations() %>% 
  dplyr::mutate(importance = ifelse(aux > 0, 0, importance)) %>% 
  put_features_cod() %>% 
  dplyr::mutate(aux = ifelse(stringr::str_detect(cod, "_6") & stringr::str_detect(last_cod, "_6\\(0"), 40, aux)) %>% 
  assign_specific_modcod_situations() %>% 
  dplyr::mutate(importance = ifelse(aux > 0, 0, importance),
                importance = ifelse(aux %in% c(50, 51, 56, 57), 1, importance)) %>% 
  dplyr::mutate(aux = ifelse(total_modcod <= 1, -2, aux)) %>% 
  dplyr::select(-valcod)

# modcod _6_1_3 é errado
# modcod _3_3_4_6 é errado
# modcod _3 é errado, talvez corrigir os dados no relogio
# modcod _2_4_6_1_3 é errado           
# modcod _6_3_10_3_4 é errado

obj4 <- obj3 %>%
  dplyr::group_by(joincod) %>% 
  dplyr::mutate(si = cumsum(importance)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(si = ifelse(si == 0, 1, si)) %>% 
  group_plays_around_jumpball() %>% 
  group_plays_around_fouls() %>% 
  group_plays_around_turnovers()


obj5 <- obj4 %>% 
  dplyr::mutate(aux = ifelse(modcod %in% c('_6_1_3', '_3_3_4_6', '_3', '_2_4_6_1_3', '_6_3_10_3_4'), -2, aux)) %>% 
  dplyr::filter(aux != -2) %>%
  dplyr::group_by(joincod, cnt, si) %>% 
  dplyr::mutate(cods = paste0(cod, collapse = "")) %>% 
  dplyr::mutate(clean_cod = stringr::str_remove_all(cods, "_(7|8|9|11|18)\\(.{1,3}\\)"),
                clean_cod = stringr::str_replace_all(clean_cod, "(_3\\(.{1,2}w\\))(_4\\(.{1,3}\\))", "\\1"),
                clean_cod = stringr::str_remove_all(clean_cod, "_6\\((4.{1,2}|18a)\\)")) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(modcod = stringr::str_remove_all(clean_cod, "\\(.{1,3}\\)"))

obj5 %>% 
  dplyr::group_by(clean_cod) %>% 
  dplyr::summarise(cnt = sum(cnt)) %>% 
  dplyr::arrange(modcod, clean_cod, desc(cnt)) %>% 
  dplyr::group_split(clean_cod) %>% 
  '['(301:350) %>% 
  plyr::l_ply(.data = ., .fun = function(df){
    
    df %>% 
      print(., n = 900)
    
  })


# REF É O GRUPO QUE DESEJAMOS MOVER A AÇÃO
# SUM_JOIN É O GRUPO ATUAL DA AÇÃO
# A AÇÃO SÓ PODE MOVER PARA O GRUPO SEGUINTE

# MOVA A AÇÃO SE 
## HÁ UMA AÇÃO DE INTERESSE NO GRUPO SEGUINTE (EXEMPLO FALTA), >> (ou seja, se ref == 1000, não há uma ação de interesse no grupo seguinte)
## E ESSA AÇÃO PODE SER MOVIDA (join = 0), 
## E SE O GRUPO FUTURO É SEGUINTE AO GRUPO ATUAL (sum_join + 1) == ref
## E SE O GRUPO ATUAL (sum_join) NÃO É O IDENTIFICADO COMO GRUPO 0 (sum_join != 0, isso já está tratado em outra parte do código)
## E SE EXISTIR UM GRUPO EM si SEGUINTE AO ATUAL DA AÇÃO A SER MOVIDA (ou seja, si < max(si))

# OFICIAL ATE AQUI


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


sort_modcod <- function(string){
  
  string %>% 
    stringr::str_replace_all(., "_", ",_") %>% 
    stringr::str_remove(., "\\,") %>%  
    stringr::str_split(., "\\,") %>% 
    unlist() %>% 
    sort() %>% 
    paste0(collapse = '')
  
}



obj4 %>% 
  dplyr::filter(stringr::str_detect(cod, "_5"), importance == 1) %>% 
  dplyr::filter(stringr::str_detect(last_cod, "_(6|10)", neg = TRUE)) %>% 
  dplyr::distinct(joincod)



obj4 %>% 
  dplyr::filter(stringr::str_detect(cod, "_5")) %>% 
  dplyr::filter(stringr::str_detect(before_cod, "_6", neg = TRUE)) %>% 
  dplyr::distinct(joincod) %>% 
  print(., n )

# corrigir os importancia 0 antes de 5
# corrigir quando 7(2p) fica sem um arremesso correto
obj5 <- obj4 %>% 
  dplyr::filter(joincod == '_18(2)_6(13a)_5(13p)_6(01p)_3(40c)') %>% 
  dplyr::select(-c(before_cod, after_cod, last_cod, next_cod)) %>% 
  dplyr::rename(aux = importance) %>% 
  dplyr::mutate(importance = 1,
                importance = ifelse(stringr::str_detect(cod, "_(8|9|11|18)"), 0, importance),
                importance = ifelse(stringr::str_detect(cod, "_6\\(18"), 0, importance),
                importance = ifelse(stringr::str_detect(cod, "_6\\(4"), 0, importance),
                importance = ifelse(stringr::str_detect(cod, "_10\\(0"), 0, importance),
                importance = ifelse(stringr::str_detect(cod, "_5\\(26"), 0, importance),
                importance = ifelse(stringr::str_detect(cod, "_7\\((0|1|4|5)"), 0, importance)) %>% 
  put_features_cod() %>% 
  dplyr::mutate(refsi = ifelse(stringr::str_detect(cod, "_10") & importance == 1, si, 0)) %>% 
  dplyr::group_by(joincod) %>%
  dplyr::mutate(refsi = max(refsi)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(si = ifelse(importance == 0 & stringr::str_detect(next_cod, "_10") &  si < refsi, si + 1, si)) %>% 
  dplyr::mutate(importance = 1,
                importance = ifelse(stringr::str_detect(cod, "_(8|9|11|18)"), 0, importance),
                importance = ifelse(stringr::str_detect(cod, "_6\\(18"), 0, importance),
                importance = ifelse(stringr::str_detect(cod, "_6\\(4"), 0, importance),
                importance = ifelse(stringr::str_detect(cod, "_7\\((0|1|4|5)"), 0, importance)) %>% 
  put_features_cod() %>% 
  dplyr::mutate(refsi = ifelse(stringr::str_detect(cod, "_6") & importance == 1, si, 0)) %>% 
  dplyr::group_by(joincod) %>%
  dplyr::mutate(refsi = max(refsi)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(si = ifelse(importance == 0 & stringr::str_detect(next_cod, "_6") &  si < refsi, si + 1, si))



i = 1
i = i + 10
obj4 %>% 
  dplyr::filter(modcod == '_6_3_4_5') %>% 
  dplyr::filter(total > 1) %>% 
  dplyr::group_split(id) %>% 
  '['(i:(i+9)) %>% 
  dplyr::bind_rows() %>% 
  dplyr::distinct(joincod, codf, si, .keep_all = TRUE) %>% 
  dplyr::select(-c(fullcod, importance, n)) %>% 
  dplyr::arrange(si) %>% 
  dplyr::group_split(id) %>%
  plyr::l_ply(.data = ., .fun = function(x){
    
    x %>% 
      dplyr::group_split(si) %>% 
      print()
    
    print("=================================")

  })

###########################
# _6_1_3, _3_3_4_6, _3, _6_3_10_3_4, _2_4_6_1_3, 

# ,
###########################

# zerar 4 quando _3(11w)_4(1t) e modcod == _6_3_4_5
# zerar 4 quando _3(12w)_4 e modcod == _6_3_4     
obj3 %>% 
  dplyr::filter(stringr::str_detect(modcod, '_6_3_4$'), 
                stringr::str_detect(cod, '_4'), 
                stringr::str_detect(joincod, "_3\\(12w\\)_4"), 
                next_cod == "", 
                stringr::str_count(modcod, "_6") == 1, 
                stringr::str_count(modcod, "_3") == 1) %>% 
  dplyr::arrange(cod) %>% 
  dplyr::select(-c(fullcod, before_cod, after_cod))

# quando modcod == _4_6_3_4_5 e _4(0u) entao 0
obj3 %>% 
  dplyr::filter(modcod == '_4_6_3_4_5') %>% 
  dplyr::filter(stringr::str_detect(cod, '_4')) %>% 
  dplyr::arrange(cod)

# modcod == _6_3_4_10     
obj3 %>% 
  dplyr::filter(stringr::str_detect(modcod, "_6_3_4_10"), 
                stringr::str_detect(joincod, '_3\\(12'), 
                stringr::str_detect(cod, '_4')) %>% 
  dplyr::select(-c(fullcod, before_cod, after_cod)) %>% 
  dplyr::group_split(modcod)

# modcod _6_1_3     
# modcod _1_3_6     
# modcod _4_3_6_3

# modcod _3_3_4_6 esta errado
# modcod _12_33_4     

# modcod _4_16_33_6_3_3
# modcod _16_33_6_3
# modcod _16_33_6_3_4_3     
# modcod _1_16_33_6_3
# modcod _12_16_33_10     
# modcod _6_5_16_16     ????
# modcod _2_4_33_16
# modcod _2_4_6_1_3  >> errado
# modcod _2_4_6_16_33_6_3_4_3     ????
# modcod _3_4_6_3
# modcod _4_3_3_6     
# modcod _4_16_6_33_3_3     
# modcod _6_3_4     quando e lane
# modcod _3         
# modcod _12_16_33_10     
# modcod  _2_4_33_16     
# modcod _2_4_6_1_3     
# modcod _3_4_6_3     
# modcod _4_3_3_6
# modcod _4_16_6_33_3_3     
# modcod _6_3_10_3_4     
# modcod _6_3_10_3_4     
# modcod _6_6_3_3_3_4_3     
# modcod _6_6_3_3_3_4_3_4     


obj4 %>% dplyr::filter(modcod %in% x) %>% 
  dplyr::distinct(joincod, codf, si, .keep_all = TRUE) %>% 
  dplyr::select(-c(fullcod, importance, n)) %>% 
  dplyr::arrange(si) %>% dplyr::filter(total > 1) %>% 
  dplyr::group_split(id) %>% length()

x <- c("_1_16_33_4_6_3_4","_1_16_33_6_3","_1_16_33_6_3_3","_1_3_4_6","_1_3_6","_1_3_6_16_3",
       "_1_6_16_16_16_16_16_33_3","_1_6_16_3_33","_1_6_16_33_5","_1_6_3_16_16_33_33","_1_6_3_6_5_3","_12_16_33_10",
       "_12_3","_12_3_3","_12_3_4_3_4_2","_12_33","_12_33_4","_12_33_4_16","_16_16_2_4_33_33","_16_3_6_3_4_3",
       "_16_33_3_4_6_3_3","_16_33_6_3","_16_33_6_3_3","_16_33_6_3_4_3",
       "_16_33_6_5_3_4_3_4","_16_5_33_6_3_3","_16_6_16","_16_6_33_3_16_33_3","_16_6_33_3_3","_16_6_33_3_4_3",
       "_2_10_10_4","_2_4_1_6_16_33_16_16_33_33_3","_2_4_3_3_6","_2_4_3_4_3_6","_2_4_3_6_3","_2_4_33_16",
       "_2_4_6_3_16_16_33_33_3","_2_4_6_5_16_33_4_3_3","_3","_3_4_6_3","_3_4_6_3_3","_3_4_6_3_4","_33",
       "_4_1_16_33_6_3","_4_1_3_3","_4_16_13_33","_4_16_33_16_33_6_3_3_4","_4_16_33_3_3_6","_4_16_5_33_4",
       "_4_16_6_33_3_3","_4_2_4_6_3_4_4_3","_4_3_3_4_6","_4_3_3_6","_4_3_4_6_3","_4_3_6_3","_4_3_6_3_4",
       "_4_6_16_16_3_4_3_4","_4_6_16_33_4_16_33_4_16_33_3_3","_4_6_3_16_16_33_4_33_4_3","_4_6_6_5_3_4_3",
       "_5_2_4_1_6_16_16_3","_5_3_4_6_3","_5_6_16_16_3_3","_6_16_16","_6_16_16_16_16",
       "_6_16_16_16_16_3_3_3","_6_16_16_16_16_33_33_33_33","_6_16_16_16_33_33_33","_6_16_16_3_3","_6_16_16_3_3_4",
       "_6_16_16_3_4_3","_6_16_16_33_16_16_33_33_33","_6_16_16_33_3_3","_6_16_16_33_3_4_33_3","_6_16_16_33_33_16_33_3_3_4",
       "_6_16_16_33_33_4_16_33_3_4_3","_6_16_3_3","_6_16_3_3_3","_6_16_3_33","_6_16_33_16_16_33_33_3_4_3","_6_16_33_4_16_33",
       "_6_16_33_4_16_33_3_3","_6_16_33_4_16_33_3_4_3","_6_16_6_5_33_3_3","_6_2_4_3_3",
       "_6_2_4_3_4_3","_6_3_16_3_33","_6_3_16_33_16_33_3","_6_3_16_33_16_33_3_3","_6_3_16_33_4_16_33_3","_6_3_3_4_16_33_4_3",
       "_6_3_3_5_10","_6_3_33_3_16","_6_3_4_10","_6_3_4_10_5","_6_3_4_16_16_33_4_33_3","_6_3_4_16_33_16_33_3_4",
       "_6_3_4_16_33_16_33_4_3","_6_3_4_3_16_33_4_3","_6_3_4_5","_6_3_4_5_10","_6_3_5_3","_6_33_3_3_16","_6_5_16",
       "_6_5_16_16","_6_5_16_3_4_3_4_33_4","_6_5_16_33_4_16_33","_6_5_16_33_4_3_3","_6_5_16_33_4_3_4_3","_6_5_3_3",
       "_6_5_3_4_16_33_3","_6_5_3_4_16_33_3_4","_6_5_3_4_3","_6_6_3_3_3_3","_6_6_3_3_3_4_3","_6_6_3_3_3_4_3_4",
       "_6_6_3_4_3_3_3","_6_6_3_4_3_4_3_3_4","_6_6_5_3_3_3_3")








# _16_13      >> nao sei
# _16_16_2_4_33_33     >> errado
# _1_6_16_33v >> nao sei
# _1_6_3_16_33      >> nao sei
# _1_6_3_16_33_4      >> nao sei
# _2_4_16_33_6_5_16_33      >> nao sei
# _2_4_1_6_3_16_33_4      >> nao sei
# _2_4_2_4_6_16_33      >> nao sei
# _2_4_2_4_6_3_4_3_16_33_4
# _2_4_6_16_16_33_4_33      >> nao sei
# _2_4_6_1_3     >> errado
# _33_4_16      >> errado
# _33_6_16      >> nao sei
# _3_13        >> errado
# _3_3        >> errado
# _3_3_2_4_6      >> errado
# _3_3_3_4_4      >> errado
# _3_3_3_4_6      >> errado
# _3_3_4     >> errado
# _3_3_4_6      >> errado
# _3_4_3
# _3_4_4_6_3_4      >> errado
# _4_16_33_6_3      >> nao sei
# _4_16_33_6_3_3
# _4_16_33_6_3_3_4      >> nao sei
# _4_16_33_6_3_3_4_13      >> nao sei
# _4_2_4_16_33_6_3_3      >> nao sei
# _4_6_3_3_16_33    >> nao sei
# _4_6_3_4_10      >> verificr isso aqui
# _4_6_3_4_5      >> corrigir o de 4(0u)
# _4_6_5_3_3      >> corrigir o 5(33p)
# _5_2_4_13      >> olhar isso aqui
# _5_6_16_16      >> olhar isso
# _6_1_16_33_3      >> errado
# _6_1_3
# _6_1_3_4
# _6_3_10_3_4      >> esperar
# _6_3_3_16_16      >> esperar
# _6_3_4_16_33_4_10_3_4      >> errado
# _6_3_6_3_4      >> errado
# _6_4_3_3      >> errado
# _6_6_16_16_3_33_3      >> esperar
# _6_6_16_33_33_4_33_3_3      >> esperar


# 16 sem free throw precisa ficar junto com as outras acoes


obj4 <- obj3 %>% 
  dplyr::group_by(joincod) %>% 
  dplyr::mutate(si = cumsum(importance)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(si = ifelse(si == 0, 1, si)) %>% 
  dplyr::group_by(joincod, si) %>% 
  dplyr::mutate(codf = paste0(cod, collapse = '')) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(joincod, cnt, realcod, modcod, cod) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(clean_cod = stringr::str_remove_all(cod, "_(7|8|9|11|18)\\(.{1,3}\\)"),
                clean_cod = stringr::str_remove_all(clean_cod, "_6\\(18a\\)"),
                clean_cod = stringr::str_remove_all(clean_cod, "_6\\(4.{2}\\)")) %>% 
  dplyr::mutate(modcleancod = clean_cod,
                modcleancod = stringr::str_replace_all(clean_cod, "_6\\(0", "_16(0"),
                modcleancod = stringr::str_replace_all(clean_cod, "_3\\(4", "_33(4"),
                modcleancod = stringr::str_remove_all(modcleancod, "\\(.{1,3}\\)")) %>% 
  dplyr::group_by(clean_cod) %>% 
  dplyr::mutate(total_clean = sum(cnt)) %>% 
  dplyr::group_by(modcleancod) %>% 
  dplyr::mutate(total_modclean = sum(cnt)) %>% 
  dplyr::group_by(modcod) %>% 
  dplyr::mutate(total_modcod = sum(cnt)) %>% 
  dplyr::group_by(realcod) %>% 
  dplyr::mutate(total_real = sum(cnt)) %>% 
  dplyr::group_by(cod) %>% 
  dplyr::mutate(total_cod = sum(cnt)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(total_modclean), desc(total_clean), desc(total_modcod), desc(total_real), realcod, cod, desc(cnt)) %>% 
  dplyr::mutate(id = dplyr::row_number()) %>% 
  dplyr::group_by(total_real, realcod) %>% 
  dplyr::mutate(id = min(id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(id, modcleancod, clean_cod, modcod, realcod, total_real, cod, joincod, cnt) %>% 
  dplyr::group_by(joincod) %>% 
  dplyr::mutate(n = dplyr::n()) %>% 
  dplyr::ungroup()

# errado modcod
## _12_16_33_10 
## _12_33_4 
## _16_16 
## _16_16_16_16 
## _16_16_16_33_4_33_33 
## _16_16_33_33 
## _16_16_33_4_33 
## _16_33_16_33 
## _16_6_33_3_3 
## _16_6_33_3_4_3 
## _1_16_16  
##  _1_16_33_16_33 
# _1_16_33_6_3 
# _1_1_6_3 
# _1_3_6 
# _1_5_5 
# _6_3_4_3_6_6_5_16_16_16_33_33_4_33     



obj4 <- obj3 %>% 
  dplyr::group_by(joincod) %>% 
  dplyr::mutate(si = cumsum(importance)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(joincod, realcod, si) %>% 
  dplyr::summarise(cod = paste0(cod, collapse = ''), 
                   cnt = sum(cnt), .groups = 'drop') %>% 
  dplyr::mutate(clean_cod = stringr::str_remove_all(cod, "_(7|8|9|11|18)\\(.{1,3}\\)"),
                clean_cod = stringr::str_remove_all(clean_cod, "_6\\(18a\\)"),
                clean_cod = stringr::str_remove_all(clean_cod, "_6\\(4.{2}\\)")) %>% 
  dplyr::group_by(clean_cod, cod, realcod) %>% 
  dplyr::summarise(cnt_join = mean(cnt), .groups = 'drop_last') %>% 
  dplyr::mutate(cnt = sum(cnt_join)) %>% 
  dplyr::group_by(clean_cod) %>% 
  dplyr::mutate(total = mean(cnt)) %>% 
  dplyr::arrange(desc(total), desc(cnt)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(modcod1 = stringr::str_remove_all(clean_cod, "\\(.{1,3}\\)")) %>% 
  dplyr::group_by(modcod1) %>% 
  dplyr::mutate(total_modcod = sum(cnt)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(id = dplyr::row_number()) %>% 
  dplyr::group_by(modcod1) %>%
  dplyr::mutate(id = min(id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_split(id)


obj4 %>% 
  '['(81:120)


#26 _16_33_6_3_3                         110
#49 _16_33_4_6_3_3                        42
#53 _6_3_3_3_16_33                        40
#54 _4_6_3_3_16_33                        39
#60 _6_3_3_4_16_33                        37
#62 _6_3_4_3_16_33_4                      34
#66 _2_4_6_3_3_16_33                      30
#73 _1_6_3_16_33_4                        26
#76 _6_3_3_16_16_33_33_4                  24
#81 _4_16_33_6_3_3                        21
#85 _6_3_4_3_4_16_33                      21
#86 _16_33_6_3_4_3                        20
#87 _2_4_6_16_33_6_3_4_3                  20
#91 _6_3_4_3_6_6_5_16_16_16_33_33_4_33    20
#93 _1_6_3_16_16_33_33                    18
#99 _6_3_3_16_33_16_33                    18
#105 _2_4_6_3_4_3_16_33                    16
#109 _6_3_4_3_3_4_16_16_33_33              16


113 _4_16_6_33_3_3                        15
116 _6_6_16_33_33_4_33_3_3                15
117 _2_4_1_6_16_33_16_16_33_33_3          14
118 _2_4_2_4_6_3_16_33_4_3                14
120 _6_16_33_6_3                          14
121 _6_3_33_3_16                          14
122 _6_3_3_4_5_6_16_33_3_3                14
123 _6_3_3_6_5_16_16_33_33                14
126 _2_4_6_3_3_6_16_33_16_33_4            13
127 _4_16_33_16_33_6_3_3_4                13
129 _4_6_16_33_4_16_33_4_16_33_3_3        13
130 _4_6_3_16_16_33_4_33_4_3              13
132 _6_16_16_33_33_4_16_33_3_3            13
133 _6_16_16_33_33_4_16_33_3_4_3          13
134 _6_3_4_16_33_16_33_3_4                13
135 _6_3_4_3_16_16_33_33                  13
136 _1_6_16_16_16_16_16_33_3              12
138 _2_4_6_16_33_3_4_3_4                  12
143 _5_6_16_33_3                          12
148 _6_3_3_4_16_33_4_3                    12
149 _6_5_16_3_4_3_4_33_4                  12
150 _1_6_3_6_16_33_3                      11
152 _2_4_1_6_3_16_33_4                    11
153 _2_4_2_4_6_3_4_3_16_33_4              11
155 _2_4_6_3_16_16_33_33_3                11
156 _2_4_6_3_16_33_4_3_4_6                11
158 _4_16_33_4_6_5_3_3                    11
159 _4_1_6_16_33_3_4_3                    11
162 _6_16_16_33_33_16_33_3_3_4            11
163 _6_16_16_33_3_3                       11
164 _6_16_33_16_16_33_33_3_4_3            11
165 _6_16_33_3_3_16_33_4                  11
166 _6_16_33_3_3_4_6_3_4_3                11
167 _6_16_33_3_3_6                        11
169 _6_3_3_16_16_33_33                    11
170 _6_3_3_6_16_33_3_3                    11
171 _6_3_4_16_16_33_4_33_3                11
172 _6_3_4_16_33_16_33_4_3                11
174 _6_3_4_3_16_33_4_3                    11
177 _6_5_16_33_4_3_4_3                    11
178 _6_5_16_33_4_6_3_3                    11
179 _16_33_6_5_3_4_3_4                    10
180 _1_16_33_6_3                          10
181 _1_6_16_33_3_4_6                      10
182 _1_6_16_33_3_5                        10
184 _2_4_6_5_16_33_4_3_3                  10
185 _4_16_33_6_3_3_4                      10
186 _4_16_33_6_3_3_4_13                   10
187 _4_2_4_2_4_6_16_33_3_3                10
191 _4_6_3_16_33                          10
192 _6_16_16_16_33_33_33_3_3              10
195 _6_16_16_33_3_4_33_3                  10
199 _6_16_33_6_3_4_3_4                    10
200 _6_16_6_5_33_3_3                      10
201 _6_3_16_33_16_33_3_3                  10
202 _6_3_3_4_6_16_33                      10
203 _6_3_4_3_4_16_33_16_33                10
204 _6_3_4_3_4_6_16_33_3_3                10
205 _6_5_3_3_16_33                        10
206 _6_5_3_4_16_33_3                      10
207 _16_16_33_4_3_4_3_4_16                 9
208 _16_33_3_4_6_3_3                       9
209 _16_6_33_3_16_33_3                     9
210 _2_10_4_16_33_6_5_3_3                  9
211 _2_4_1_6_16_33_3_4                     9
212 _2_4_6_3_16_33_4_3                     9
213 _2_4_6_3_33_3                          9
215 _4_2_4_16_33_6_3_3                     9
217 _4_6_3_16_33_4_3                       9
218 _4_6_3_3_4_16_33_4                     9
219 _5_16_33_4_6_3_3                       9
225 _6_16_33_6_3_4                         9
226 _6_3_16_33_16_33_3                     9
227 _6_3_16_33_4_16_33_3                   9
228 _6_3_3_3_4_6_16_33                     9
230 _6_3_4_16_33_4_10_3_4                  9
232 _6_5_3_4_16_33_3_4                     9
233 _16_6_33_3_3                           8
235 _33_16_6_3_4_3_3                       8
236 _4_1_16_33_6_3                         8
237 _4_6_16_33_3_4_3_4                     8
238 _4_6_3_3_4_16_33_10                    8
239 _4_6_3_4_3_16_33                       8
242 _6_16_33_3_16_3_4_33                   8
243 _6_16_33_3_4_3_4_6                     8
244 _6_16_33_4_6_3                         8
245 _6_3_16_33_4_3_3_4                     8
246 _6_3_16_33_4_3_4                       8
247 _6_3_3_4_6_16_33_4                     8
248 _6_5_16_33_4_3_3                       8
249 _6_6_16_16_3_33_3                      8
250 _6_6_16_33_3_3                         8
251 _16_33_4_6_3_4_3                       7
252 _16_5_33_6_3_3                         7
253 _1_16_33_4_6_3_4                       7
254 _1_16_33_6_3_3                         7
255 _1_6_16_33_3_4_3                       7
256 _4_2_4_6_16_33_3                       7
257 _4_5_6_16_33_3_3                       7
259 _4_6_3_5_16_33                         7
260 _4_6_3_6_16_33_3                       7
261 _4_6_6_3_16_33                         7
263 _5_6_3_4_3_16_33                       7
264 _6_16_33_3_3_13                        7
265 _6_16_33_3_3_3_4                       7
266 _6_16_33_3_3_4_1                       7
267 _6_16_33_3_5                           7
268 _6_16_33_6_3_3                         7
269 _6_1_16_33_3                           7
271 _6_3_16_33_4_3_3                       7
272 _6_3_3_4_16_33_10                      7
273 _6_3_3_4_16_33_4                       7
274 _6_3_3_6_16_33_3                       7
275 _16_33_4_6_3                           6
276 _16_6_33_3_4_3                         6
278 _1_6_3_4_16_33                         6
279 _4_16_33_3_3_6                         6
280 _4_16_33_6_3                           6
281 _4_1_6_3_16_33                         6
282 _5_1_6_3_16_33                         6
283 _6_3_4_5_16_33                         6
284 _6_6_3_4_16_33                         6
285 _1_6_16_3_33                           5
286 _4_6_16_33_3                           5
288 _6_16_3_33                             5
289 _6_33_3_3_16                           5
290 _6_3_16_3_33                           5
291 _6_6_16_33_3                           5
292 _6_3_16_33                             4


obj4 <- obj3 %>% 
  dplyr::group_by(joincod) %>%
  dplyr::mutate(si = cumsum(importance)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(joincod, si) %>% 
  dplyr::mutate(pattern = paste0(cod, collapse = ''),
                seqimp = paste0(importance, collapse = '')) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(importance == 1)

obj4 %>% 
  dplyr::group_by(pattern) %>% 
  dplyr::summarise(cnt = sum(cnt), .groups = 'drop') %>% 
  dplyr::mutate(valcod = stringr::str_remove_all(pattern, "_(7|8|9|11|18)\\(.{1,3}\\)"),
                valcod = stringr::str_remove_all(valcod, "_6\\(18a\\)"),
                valcod = stringr::str_remove_all(valcod, "_6\\(4.{1,2}\\)")) %>% 
  dplyr::select(valcod, pattern, cnt) %>% 
  dplyr::group_split(valcod) %>% 
  '['(1:20)

obj3 %>% 
  dplyr::group_by(importance, cod, next_cod) %>% 
  dplyr::summarise(cnt = sum(cnt), .groups = 'drop')

c('_1(1)_3(11c)', '_1(1a)_3(11c)', '_1(2)_3(11w)', '_1(3)_3(11c)', '_1(3)_3(11w)', 
  '_1(3)_3(12c)_3(22c)', '_1(3a)_3(11c)', '_1(4)_3(11c)', '_1(4)_3(11w)', '_1(4a)_3(11c)',
  '_1(5)_3(11c)', '_1(5a)_3(11c)', '_10(1)_3(22w)', '_10(1)_4(1t)', '_10(1)_5(13p)',
  '_12(2)_3(11c)', '_12(2)_3(40c)', '_12(2)_3(40w)', '_12(3)_3(40c)', '_12(3)_3(40w)', 
  '_12(4)_3(12c)_3(22c)', '_12(4)_3(12w)_4(1t)_3(22w)', '_12(4)_3(40c)', '_12(4)_3(40w)',
  '_13(1)_3(40c)', '_4(0p)_3(12c)', '_4(0p)_3(12c)_3(22c)', '_4(0p)_3(12c)_3(22w)', 
  '_4(0p)_3(12w)', '_4(0p)_3(12w)_4(0t)_3(22c)', '_4(0p)_3(40c)', '_4(0p)_3(40c)_3(40c)',
  '_4(0t)_3(12c)', '_4(0t)_3(12c)_3(22c)', '_4(0t)_3(12w)_4(1t)_3(22w)', '_4(0t)_3(22c)',
  '_4(1t)_5(19p)', '_4(1t)_5(20p)', '_5(12p)_5(40t)', '_5(25t)_3(40w)', '_5(28p)_3(51c)_3(52c)', 
  '_5(28p)_3(51w)_4(1t)_3(52c)', '_5(28p)_3(61c)', '_5(30p)_3(12w)', '_5(33p)_3(12c)_3(22c)', 
  '_5(33p)_3(12c)_3(22w)', '_5(33p)_3(12w)_4(1t)_3(22c)', '_5(34p)_3(12c)_3(22c)', '_5(35p)_3(22c)', 
  '_6(01c)_3(22w)_4(0t)_3(40c)', '_6(01c)_3(40c)_3(11c)', '_6(01c)_3(40c)_3(12c)_3(22c)', 
  '_6(01c)_3(40c)_3(12w)_4(1t)_3(22c)', '_6(01c)_3(40c)_3(13c)_3(23c)_3(33c)', 
  '_6(01c)_3(40c)_3(13c)_3(23c)_3(33w)', '_6(01c)_3(40c)_3(13c)_3(23w)_4(1t)_3(33c)', 
  '_6(01c)_3(40c)_3(13w)_4(1t)_3(23w)_4(1t)_3(33c)', '_6(01c)_3(40c)_3(22w)', 
  '_6(01c)_3(40c)_3(23c)_3(33c)', '_6(01c)_3(40c)_3(23w)_4(1t)_3(33c)',
  '', '', '', '', '', '', '', '', '',
  '', '', '', '', '', '', '', '', '',)


pbp_momentos %>% 
  dplyr::mutate(np = stringr::str_count(cod, "_")) %>% 
  dplyr::group_by(np, cod) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::mutate(perc = 100 * n / sum(n))


change_cod <- function(df){
  
  df %>% 
    dplyr::mutate(cod = stringr::str_replace_all(cod, "_6\\(0", "_16(0"),
                  cod = stringr::str_replace_all(cod, "_6\\(4", "_16(4"),
                  cod = stringr::str_replace_all(cod, "_3\\(40", "_33(40")) %>% 
    dplyr::mutate(cod = stringr::str_replace_all(cod, "(_7\\(.+?\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_8\\(.\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_9\\(.\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_10\\(0\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_11\\(.\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_18\\(.\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_16\\(4.{2}\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_6\\(18.{1}\\))(\\[1\\])", "\\1[0]"))
}

simplify_cod <- function(df){
  
  df %>% 
    dplyr::mutate(modcod = stringr::str_remove_all(cod, "_[0-9]+\\(.{1,3}\\)\\[0\\]"),
                  modcod = stringr::str_remove_all(modcod, "\\(.{1,3}\\)\\[1\\]"))
  
}

put_ind_column <- function(df){
  
  df %>% 
    dplyr::mutate(ind = ifelse(stringr::str_detect(modcod, "_6(.+)?_16(.+)?_33(.+)?_3"), 1, 0),
                  ind = ifelse(stringr::str_detect(modcod, "_6(.+)?_16(.+)?_3(.+)?_33"), 1, ind),
                  ind = ifelse(stringr::str_detect(modcod, "_3(.+)?_4(.+)?_3"), ind + 2, ind),
                  ind = ifelse(stringr::str_count(modcod, "_16") > 1, ind + 3, ind),
                  ind = ifelse(stringr::str_detect(modcod, "_1_3"), ind + 7, ind),
                  ind = ifelse(stringr::str_detect(modcod, "_10_4"), ind + 10, ind))
  
}

change_cod_free_throws <- function(df){
  
  df %>% 
    dplyr::mutate(cod = stringr::str_replace_all(cod, "(_3\\(.+?\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_5\\(00p\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_5\\(13p\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_33\\(40.\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_3\\(12w\\)\\[0\\]_4\\(1t\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_3\\(13w\\)\\[0\\]_4\\(1t\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_3\\(23w\\)\\[0\\]_4\\(1t\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_3\\(51w\\)\\[0\\]_4\\(1t\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_3\\(71w\\)\\[0\\]_4\\(1t\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_3\\(81w\\)\\[0\\]_4\\(1t\\))(\\[1\\])", "\\1[0]"),
                  cod = stringr::str_replace_all(cod, "(_3\\(82w\\)\\[0\\]_4\\(1t\\))(\\[1\\])", "\\1[0]")) 
}

change_cod_rebounds <- function(df){
  
  df %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_4(_33|_16_33)"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])(_(16|33)\\(.{1,3}\\)\\[.\\])", "\\1\\2[0]\\4"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_33_4(_33|_16_33)"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4\\(.{1,3}\\))(\\[1\\])(_(16|33)\\(.{1,3}\\)\\[.\\])", "\\1\\2\\3[0]\\5"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_33_4(_33|_16_33)"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_(16|33)\\(.{1,3}\\)\\[.\\])", "\\1\\2[0]\\4\\5"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_33_4(_33|_16_33)"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4\\(.{1,3}\\))(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_(16|33)\\(.{1,3}\\)\\[.\\])", "\\1\\2\\3[0]\\5\\6"), cod)) %>% 
  
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_4_(3$|3_)"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])(_3\\(.{1,3}\\)\\[0\\])", "\\1\\2[0]\\4"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_33_4_(3$|3_)"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4\\(.{1,3}\\))(\\[1\\])(_3\\(.{1,3}\\)\\[0\\])", "\\1\\2\\3[0]\\5"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_33_4_(3$|3_)"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\1\\2[0]\\4\\5"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_33_4_(3$|3_)"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4\\(.{1,3}\\))(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\1\\2\\3[0]\\5\\6"), cod)) %>% 
    
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_4_33"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])(_33\\(.{1,3}\\)\\[0\\])", "\\1\\2[0]\\4"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_3_4_33"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4\\(.{1,3}\\))(\\[1\\])(_33\\(.{1,3}\\)\\[0\\])", "\\1\\2\\3[0]\\5"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_3_4_33"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_33\\(.{1,3}\\)\\[0\\])", "\\1\\2[0]\\4\\5"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_3_4_33"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4\\(.{1,3}\\))(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_33\\(.{1,3}\\)\\[0\\])", "\\1\\2\\3[0]\\5\\6"), cod)) %>% 
    
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_4_(3$|3_)"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])(_3\\(.{1,3}\\)\\[0\\])", "\\1\\2[0]\\4"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_3_4_(3$|3_)"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4\\(.{1,3}\\))(\\[1\\])(_3\\(.{1,3}\\)\\[0\\])", "\\1\\2\\3[0]\\5"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_3_4_(3$|3_)"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\1\\2[0]\\4\\5"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_3_4_(3$|3_)"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4\\(.{1,3}\\))(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\1\\2\\3[0]\\5\\6"), cod))
}

change_cod_turnovers <- function(df){
  
  df %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5"), stringr::str_replace(cod, "(_5\\(00p\\))(\\[1\\])", "\\1[0]"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_5"), stringr::str_replace(cod, "(_5\\(13p\\))(\\[1\\])", "\\1[0]"), cod))
}

put_useful_columns <- function(df){
  
  df %>% 
    dplyr::mutate(val = stringr::str_replace_all(cod, "_[0-9]+\\(.+?\\)", ""),
                  val = stringr::str_replace_all(val, "\\[|\\]", "")) %>% 
    dplyr::mutate(np_val = stringr::str_count(val, "1")) %>% 
    dplyr::mutate(val_play = "",
                  val_play = ifelse(np_val == 1, stringr::str_extract(cod, '_[0-9]+\\(.{1,3}\\)\\[1\\]'), val_play))
  
}


change_cod1 <- function(df){
  
  df %>%
    dplyr::mutate(aux = ifelse(stringr::str_detect(modcod, "_1_|_1$") & stringr::str_detect(modcod, "^_1_3"), 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_1\\(.{1,3}\\)\\[1\\])", "\\1;"), cod),
                  aux = ifelse(stringr::str_detect(modcod, "_1_|_1$") & stringr::str_detect(cod, "(\\[1\\])(_1\\(.{1,3}\\))"), 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(\\[1\\])(_1\\(.{1,3}\\))", "\\1;\\2"), cod),
                  aux = ifelse(stringr::str_detect(modcod, "_1_|_1$") & stringr::str_detect(cod, "(_1\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])"), 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_1\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                  aux = ifelse(stringr::str_detect(modcod, "_1_|_1$") & stringr::str_detect(cod, "(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_1\\(.{1,3}\\)\\[1\\])") & np_val > 1, 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_1\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                  aux = ifelse(stringr::str_detect(modcod, "_1_|_1$") & stringr::str_detect(cod, "(_1\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_[0-9]+\\(.{1,3}\\)\\[1\\])"), 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_1\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod),
                  
                  aux = ifelse(stringr::str_detect(modcod, "_1_|_1$") & stringr::str_detect(cod, "(_5\\(.{1,3}\\)\\[0\\])(_1\\(.{1,3}\\)\\[1\\])"), 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_5\\(.{1,3}\\)\\[0\\])(_1\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                  aux = ifelse(stringr::str_detect(modcod, "_1_|_1$") & stringr::str_detect(cod, "(_5\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_1\\(.{1,3}\\)\\[1\\])"), 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_5\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_1\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                  
                  aux = ifelse(stringr::str_detect(modcod, "_1_|_1$"), 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_16)", "\\2\\1\\3"), cod),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_7\\(2p\\)\\[0\\])(\\;)(_1\\()", "\\2\\1\\3"), cod),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_1\\(.{1,3}\\)\\[1\\])(\\;)(_7\\(2p\\)\\[0\\])", "\\1\\3\\2"), cod),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_18\\(.{1,3}\\)\\[0\\])(\\;)", "\\2\\1"), cod)) %>% 
    dplyr::select(-aux)
  
  


}

change_cod2 <- function(df){
  
  df %>% 
    dplyr::mutate(aux = ifelse(modcod == "_5_2", 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace(cod, "_5\\(13p\\)\\[0\\]", "_5(13p)[1]"), cod)) %>% 
    dplyr::mutate(aux = ifelse(stringr::str_detect(modcod, "_2") & stringr::str_detect(cod, "(\\[1\\])(_2\\(.{1,3}\\))"), 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(\\[1\\])(_2\\(.{1,3}\\))", "\\1;\\2"), cod),
                  aux = ifelse(stringr::str_detect(modcod, "_2") & stringr::str_detect(cod, "(_2\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])"), 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_2\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                  aux = ifelse(stringr::str_detect(modcod, "_2") & stringr::str_detect(cod, "(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_2\\(.{1,3}\\)\\[1\\])") & np_val > 1, 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_[0-9]+\\(.{1,3}\\)\\[0\\])(_2\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                  aux = ifelse(stringr::str_detect(modcod, "_2") & stringr::str_detect(cod, "(_2\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_[0-9]+\\(.{1,3}\\)\\[1\\])"), 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_2\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                  aux = ifelse(stringr::str_detect(modcod, "_2"), 1, 0),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_16)", "\\2\\1\\3"), cod),
                  cod = ifelse(aux == 1, stringr::str_replace_all(cod, "((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_10)", "\\2\\1\\3"), cod)) %>% 
    dplyr::select(-aux)
  
  
}

change_cod4 <- function(df){
  
df %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '^_4_3'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])(_3)', "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '^_4_3'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3)', "\\1;\\2\\3"), cod)) %>%
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '^_4_(3|33)'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])(_(3|33))', "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '^_4_(3|33)'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_(3|33))', "\\1;\\2\\3"), cod)) %>%
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_2_4_(3|33)'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])(_(3|33))', "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '_2_4_(3|33)'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_(3|33))', "\\1;\\2\\3"), cod)) %>%
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_4_4'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])(_4)', "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '_4_4'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4)', "\\1;\\2\\3"), cod)) %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_4_5'), stringr::str_replace(cod, '(_5\\(37t\\))(\\[1\\])', "\\1[0]"), cod)) %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_4_5'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])(_5\\(.{1,3}\\)\\[1\\])', "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '_4_5'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])(_5\\(.{1,3}\\)\\[0\\])(_5\\(.{1,3}\\)\\[1\\])', "\\1\\2;\\3"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '_4_5'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_5\\(.{1,3}\\)\\[1\\])', "\\1\\2;\\3"), cod)) %>%
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_4_5'), stringr::str_replace(cod, '\\;$', ""), cod)) %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_4_6'), stringr::str_replace_all(cod, '(_4\\(.{1,3}\\)\\[1\\])(_6)', "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '_4_6'), stringr::str_replace_all(cod, '(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6)', "\\1;\\2\\3"), cod)) %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_4_10'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[.\\])(_10)', "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '_4_10'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_10)', "\\1;\\2\\3"), cod)) %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_4_13'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])(_13)', "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '_4_13'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_13)', "\\1;\\2\\3"), cod)) %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_4_13'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])(_18)', "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '_4_13'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_18)', "\\1;\\2\\3"), cod)) %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_4_16'), stringr::str_replace(cod, '(_[33]\\(.{1,3}\\)\\[.\\])(_4\\(.{1,3}\\)\\[1\\])(_16)', "\\1\\2;\\3"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '_4_16'), stringr::str_replace(cod, '(_[33]\\(.{1,3}\\)\\[.\\])(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16)', "\\1\\2;\\3\\4"), cod)) %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_4_16') & stringr::str_detect(modcod, "_33", negate = T), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])(_16)', "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '_4_16') & stringr::str_detect(modcod, "_33", negate = T), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16)', "\\1;\\2\\3"), cod)) %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '^_4_16'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])(_16)', "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, '^_4_16'), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16)', "\\1;\\2\\3"), cod))
    

    # dplyr::mutate(cod = ifelse(stringr::str_detect(cod, "._4\\(.{1,3}\\)\\[1\\]_3"), stringr::str_replace_all(cod, "(._4\\(.{1,3}\\))(\\[1\\])(_3)", "\\1[0]\\3"), cod),
    #               cod = ifelse(stringr::str_detect(cod, "._4\\(.{1,3}\\)\\[1\\][(_[0-9]+\\(.{1,3}\\)\\[0\\])+]_3"), stringr::str_replace_all(cod, "(._4\\(.{1,3}\\))(\\[1\\])([_[0-9]+\\(.{1,3}\\)\\[0\\]]+)(_3)", "\\1[0]\\3\\4"), cod))
  
}

change_cod4a <- function(df){
  
  df %>% 
    change_cod_rebounds() %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_4"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_3_4"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_33_4"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_33_4"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod))


}

change_cod12 <- function(df){
  
  df %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "._12"), stringr::str_remove_all(cod, ';'), cod)) %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "._12"), stringr::str_replace_all(cod, "\\[1\\]", "[0]"), cod), 
                  cod = ifelse(stringr::str_detect(modcod, "._12"), stringr::str_replace(cod, "(_12\\(.{1,3}\\))(\\[0\\])", "\\1[1]"), cod))
    
}

change_cod13 <- function(df){
  
  df %>%
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_13."), stringr::str_replace(cod, "(_13\\(.\\)\\[.\\])", "aaa\\1"), cod),
                  cod = ifelse(stringr::str_detect(modcod, "_13."), paste0('a', cod), cod),
                  cod1 = ifelse(stringr::str_detect(modcod, "_13."), stringr::str_replace(cod, "(^.+?aaa)(.+$)", "\\1"), ""),
                  cod2 = ifelse(stringr::str_detect(modcod, "_13."), stringr::str_replace(cod, "(^.+?aaa)(.+$)", "\\2"), "")) %>% 
    dplyr::mutate(cod1 = ifelse(stringr::str_detect(modcod, "_13."), stringr::str_replace(cod1, "a_", "_"), cod1),
                  cod1 = ifelse(stringr::str_detect(modcod, "_13."), stringr::str_replace(cod1, "aaaa", ""), cod1),
                  cod1 = ifelse(stringr::str_detect(modcod, "_13."), stringr::str_replace(cod1, "aaa", ""), cod1)) %>% 
    dplyr::mutate(cod2 = ifelse(stringr::str_detect(modcod, "_13."), stringr::str_remove_all(cod2, "\\;"), cod2)) %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_13."), paste0(cod1, cod2), cod)) %>% 
    dplyr::select(-c(cod1, cod2))
  
    # 
    # dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_13."), stringr::str_remove_all(cod2, ';'), cod)) %>% 
    # dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_13."), stringr::str_replace_all(cod, "\\[1\\]", "[0]"), cod), 
    #               cod = ifelse(stringr::str_detect(modcod, "_13."), stringr::str_replace(cod, "(_13\\(.{1,3}\\))(\\[0\\])", "\\1[1]"), cod))
    
}

sub_all_patterns <- function(string, pattern, replacement){
  
  while(stringr::str_detect(string, pattern)){
    
    string <- stringr::str_replace(string, pattern, replacement)
    
  }
  
  return(string)
  
}

xdf <- pbp_momentos %>% 
  dplyr::mutate(np = stringr::str_count(cod, "_")) %>% 
  dplyr::group_by(np, cod) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::mutate(cod = stringr::str_replace_all(cod, '\\)', ')[1]')) %>% 
  change_cod() %>% 
  simplify_cod() %>% 
  put_ind_column() %>%
  change_cod_free_throws() %>% 
  change_cod_rebounds() %>% 
  change_cod_turnovers() %>% 
  put_useful_columns() %>% 
  dplyr::mutate(ind11 = stringr::str_detect(val, "11"),
                ind11 = as.integer(ind11)) %>%
  dplyr::mutate(cod = stringr::str_replace(cod, "(_18\\(.\\))(\\[0\\])(_13\\(.\\))(\\[1\\])", "\\1\\4\\3\\2")) %>% 
  dplyr::mutate(original_cod = cod,
                original_cod = stringr::str_remove_all(original_cod, "\\[.\\]"))

xdf1 <- xdf %>% 
  change_cod1() %>% 
  change_cod2() %>% 
  change_cod4() %>% 
  change_cod4a() %>% 
  dplyr::mutate(cod = ifelse(stringr::str_count(modcod, "_16") > 1, stringr::str_replace_all(cod, "(_16\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod),
                cod = ifelse(stringr::str_count(modcod, "_16") > 1, stringr::str_replace(cod, "(_16\\(.{1,3}\\))(\\[0\\])", "\\1[1]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_16") & stringr::str_detect(modcod, "_3$|_3_", negate = TRUE), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_16") & stringr::str_detect(modcod, "_3$|_3_", negate = TRUE), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "(_3_6)(_16_3)$"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod)) %>%
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "(_33_16)(_6_3_4_3_3)$"), stringr::str_replace(cod, "(_16\\(.{1,3}\\)\\[1\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "(_33_16)(_6_3_4_3_3)$"), stringr::str_replace(cod, "(_16\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>%
  
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "(_16_3)(_6_3)"), stringr::str_replace(cod, "(_16\\(.{1,3}\\)\\[1\\]_3\\(.{1,3}\\)\\[0\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod)) %>%
  
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6(.+)?_16(.+)?_33(.+)?_3|_6(.+)?_16(.+)?_3(.+)?_33|_16(.+)?_6(.+)?_3(.+)?_33|_16(.+)?_6(.+)?_33(.+)?_3"), 
                             stringr::str_replace(cod, "(_16\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_10_13$"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])", "\\1;"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_10_5"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])(_5\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_10_5"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_5\\(.{1,3}\\)\\[.\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_10"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_10"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_10_6"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_10_6"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_10_16"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_10_16"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_12_10"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_12_10"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_12_5"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])(_5\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_12_5"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_5\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_12_16"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_12_16"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_12_6"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_12_6"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_12_3"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])(_3\\(.{1,3}\\)\\[0\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_12_3"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_12_33"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])(_33\\(.{1,3}\\)\\[0\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_12_33"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_33\\(.{1,3}\\)\\[0\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_5"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_5\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_5"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_5\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_5_3"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(\\;)(_5\\(.{1,3}\\)\\[1\\])", "\\1\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_5_3"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_5\\(.{1,3}\\)\\[1\\])", "\\1\\2\\4"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_5"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(\\;)(_5\\(19p\\)\\[1\\])", "\\1\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_5"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_5\\(19p\\)\\[1\\])", "\\1\\2\\4"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_5"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(\\;)(_5\\(20p\\)\\[1\\])", "\\1\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_5"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_5\\(20p\\)\\[1\\])", "\\1\\2\\4"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_10"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_10"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_10"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_10"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_10"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_13"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[.\\])(_13\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_13"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_13\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_13"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[.\\])(_18\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_13"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_18\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_13"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_13\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_13"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_13\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_13"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_18\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_13"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_18\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_13"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_13\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_13"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_13\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_13"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_18\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_13"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_18\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_13"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_13\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_13"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_13\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_13"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_18\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_13"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_18\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_5_3"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[.\\])(_3\\(.{1,3}\\)\\[0\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_5_3"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_5\\(.{1,3}\\)\\[0\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_4"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[.\\])(_4\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_4"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_4\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_5"), sub_all_patterns(cod, "(_5\\(.{1,3}\\)\\[1\\])(_5\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_5"), sub_all_patterns(cod, "(_5\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_5\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_5"), sub_all_patterns(cod, "(_5\\(.{1,3}\\)\\[0\\])(_5\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_5"), sub_all_patterns(cod, "(_5\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_5\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>%
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_6"), sub_all_patterns(cod, "(_6\\(.{1,3}\\)\\[1\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_6"), sub_all_patterns(cod, "(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_6"), stringr::str_replace_all(cod, "(_5\\(.{1,3}\\)\\[1\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_6"), stringr::str_replace_all(cod, "(_5\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_6"), stringr::str_replace_all(cod, "(_5\\(37t\\)\\[0\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_6"), stringr::str_replace_all(cod, "(_5\\(37t\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_10"), stringr::str_replace_all(cod, "(_5\\(.{1,3}\\)\\[.\\])(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_10"), stringr::str_replace_all(cod, "(_5\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_16"), stringr::str_replace_all(cod, "(_5\\(.{1,2}p\\)\\[1\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_16"), stringr::str_replace_all(cod, "(_5\\(.{1,2}p\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_16"), stringr::str_replace_all(cod, "(_5\\(37t\\)\\[0\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_16"), stringr::str_replace_all(cod, "(_5\\(37t\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_16_13$"), stringr::str_replace(cod, "(_16\\(.{1,3}\\)\\[.\\])(_13\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_16_13$"), stringr::str_replace(cod, "(_16\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_13\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_16_13$"), stringr::str_replace(cod, "(_16\\(.{1,3}\\)\\[.\\])(_18\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_16_13$"), stringr::str_replace(cod, "(_16\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_18\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_1_3"), stringr::str_replace_all(cod, "(_1\\(.{1,3}\\)\\[1\\])(_3\\(.{1,3}\\)\\[0\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_1_3"), stringr::str_replace_all(cod, "(_1\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_5_16") & stringr::str_detect(modcod, "_(3$|3_)", negate = T), stringr::str_replace_all(cod, "(_6\\(.{1,3}\\)\\[1\\]_5\\(.{1,3}\\)\\[0\\])(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_5_16") & stringr::str_detect(modcod, "_(3$|3_)", negate = T), stringr::str_replace_all(cod, "(_6\\(.{1,3}\\)\\[1\\]_5\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_5_5"), stringr::str_replace_all(cod, "(_6\\(.{1,3}\\)\\[1\\]_5\\(.{1,3}\\)\\[0\\])(_5\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_5_5"), stringr::str_replace_all(cod, "(_6\\(.{1,3}\\)\\[1\\]_5\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_5\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_5_6"), stringr::str_replace_all(cod, "(_6\\(.{1,3}\\)\\[1\\]_5\\(.{1,3}\\)\\[0\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_5_6"), stringr::str_replace_all(cod, "(_6\\(.{1,3}\\)\\[1\\]_5\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_16_5_33"), stringr::str_replace_all(cod, "(_16\\(.{1,3}\\)\\[.\\]_5\\(.{1,3}\\))(\\[1\\])(_33\\(.{1,3}\\)\\[0\\])", "\\1[0]\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_16_5_33"), stringr::str_replace_all(cod, "(_16\\(.{1,3}\\)\\[.\\]_5\\(.{1,3}\\))(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_33\\(.{1,3}\\)\\[0\\])", "\\1[0]\\3\\4"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_16_5_33"), stringr::str_replace_all(cod, "(_16\\(.{1,3}\\)\\[.\\].+_5\\(.{1,3}\\))(\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_33\\(.{1,3}\\)\\[0\\])", "\\1[0]\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6.+?_3_6$"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6.+?_3_6$"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_5"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_5\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_5"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_5\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_5"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;)(_5\\(19p\\)\\[1\\])", "\\1\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_5"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_5\\(19p\\)\\[1\\])", "\\1\\2\\4"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_5"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;)(_5\\(20p\\)\\[1\\])", "\\1\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_5"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_5\\(20p\\)\\[1\\])", "\\1\\2\\4"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_5"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_5\\(.{1,2}p\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_5"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_5\\(.{1,2}p\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_6$"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_6\\(.{1,3}\\)\\[1\\]$)", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_6$"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\]$)", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_6$"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)$", "\\1;\\2\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_6$"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)$", "\\1;\\2\\3\\4"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_4_16(.+)?_33"), stringr::str_replace(cod, "(_4\\(.{1,3}\\)\\[1\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_4_16(.+)?_33"), stringr::str_replace(cod, "(_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3(_6|_6_1|_6_5|_6_5_3|_6_5_3_4|_6_2_4|_6_16_33_3|_6_16_33_3_3|_6_3_5|_6_3_3_4_2|_6_16_33_16_33_4|_6_3_3_3)$") & stringr::str_count(modcod, "_6") > 1, stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3(_6|_6_1|_6_5|_6_5_3|_6_5_3_4|_6_2_4|_6_16_33_3|_6_16_33_3_3|_6_3_5|_6_3_3_4_2|_6_16_33_16_33_4|_6_3_3_3)$") & stringr::str_count(modcod, "_6") > 1, stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3(_6|_6_1|_6_5|_6_5_3|_6_5_3_4|_6_2_4|_6_16_33_3|_6_16_33_3_3|_6_3_5|_6_3_3_4_2|_6_16_33_16_33_4|_6_3_3_3)$") & stringr::str_count(modcod, "_6") > 1, stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1\\2\\3\\4;\\5"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_6(_3|_3_4|_3_3|_3_4_3|_3_3_4|_3_4_3_4)$") & stringr::str_count(modcod, "_6") > 1, stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_6(_3|_3_4|_3_3|_3_4_3|_3_3_4|_3_4_3_4)$") & stringr::str_count(modcod, "_6") > 1, stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_6(_3|_3_4|_3_3|_3_4_3|_3_3_4|_3_4_3_4)$") & stringr::str_count(modcod, "_6") > 1, stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1\\2\\3\\4;\\5"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_4_6(_3|_3_4|_3_3|_3_4_3|_3_3_4|_3_4_3_4)$"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;_4\\(.{1,3}\\)\\[1\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_4_6(_3|_3_4|_3_3|_3_4_3|_3_3_4|_3_4_3_4)$"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1\\2\\3\\4;\\5"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_6(_5|_3|_3_4|_3_3|_3_4_3|_3_3_4|_3_4_3_4|_5_3_3|_5_3_4_3_4|_3_3_4_13|_5_16_33)$"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_6(_5|_3|_3_4|_3_3|_3_4_3|_3_3_4|_3_4_3_4|_5_3_3|_5_3_4_3_4|_3_3_4_13|_5_16_33)$"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_4_6(_3|_3_4|_3_3|_3_4_3|_3_3_4|_3_4_3_4)$"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(\\;_4\\(.{1,3}\\)\\[1\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_4_6(_3|_3_4|_3_3|_3_4_3|_3_3_4|_3_4_3_4)$"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(\\;_4\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3\\4"), cod))  %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_1_3"), stringr::str_replace_all(cod, "(_6\\(.{1,3}\\)\\[.\\])(\\;)(_1\\(.{1,3}\\)\\[.\\])", "\\1\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_1_3"), stringr::str_replace_all(cod, "(_6\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_1\\(.{1,3}\\)\\[.\\])", "\\1\\2\\4"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_1_3"), stringr::str_replace_all(cod, "(_6\\(.{1,3}\\)\\[.\\])(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_1\\(.{1,3}\\)\\[.\\])", "\\1\\3\\4"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_1_3"), stringr::str_replace_all(cod, "(_6\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_1\\(.{1,3}\\)\\[.\\])", "\\1\\2\\4\\5"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_1_3"), stringr::str_replace_all(cod, "(_1\\(.{1,3}\\)\\[.\\])(\\;)(_3\\(.{1,3}\\)\\[.\\])", "\\1\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_1_3"), stringr::str_replace_all(cod, "(_1\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_3\\(.{1,3}\\)\\[.\\])", "\\1\\2\\4"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_1_3"), stringr::str_replace_all(cod, "(_1\\(.{1,3}\\)\\[.\\])(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[.\\])", "\\1\\3\\4"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_1_3"), stringr::str_replace_all(cod, "(_1\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[.\\])", "\\1\\2\\4\\5"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "(_3_3_4_6|_3_4_6_3)$") & stringr::str_count(modcod, "_6") == 1, stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[.\\])(\\;)(_4\\(.{1,3}\\)\\[.\\])(\\;)(_6\\(.{1,3}\\)\\[.\\])", "\\1\\3\\5"), cod),
                cod = ifelse(stringr::str_detect(modcod, "(_3_3_4_6|_3_4_6_3)$") & stringr::str_count(modcod, "_6") == 1, stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[.\\])(\\;)(_4\\(.{1,3}\\)\\[.\\])(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[.\\])", "\\1\\3\\5\\6"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_3_16_16|_6_3_3_16_16|_6_3_3_16_33_16_33"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_3_16_16|_6_3_3_16_16|_6_3_3_16_33_16_33"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_5_3_3_16"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_5_3_3_16"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_10_16"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[.\\])(\\;)(_10\\(.{1,3}\\)\\[.\\])", "\\1\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_10_16"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[.\\])(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_10\\(.{1,3}\\)\\[.\\])", "\\1\\3\\4"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_4_4_3"), stringr::str_replace(cod, "(_4\\(.{1,3}\\)\\[.\\])(\\;)(_4\\(.{1,3}\\)\\[.\\])", "\\1\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_3_4_4_3"), stringr::str_replace(cod, "(_4\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_4\\(.{1,3}\\)\\[.\\])", "\\1\\2\\4"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_3_5") & stringr::str_detect(cod, "_5\\(39t\\)"), stringr::str_replace(cod, "(\\;)(_5\\(39t\\)\\[1\\])", "\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_16_6"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_16)", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_3_4_3_16_16_33_33"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = stringr::str_replace_all(cod, "\\;\\;", ";")) %>% 
  change_cod12() %>% 
  change_cod13() %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, '_1_3_4_6'), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;)(_4\\(.{1,3}\\)\\[.\\])(\\;)", "\\1\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod, '_5\\(37t\\)\\[.\\]_16'), stringr::str_replace(cod, "(_5\\(37t\\)\\[.\\])(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(cod, '_5\\(37t\\)\\[.\\].+?_16'), stringr::str_replace(cod, "(_5\\(37t\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod %in% c('_6_3_5_3', '_16_33_4_16_33', '_3_3_2_4_6', '_3_4_4_6_3_4', '_33_4_16', '_6_2_4_3_3', 
                                           '_6_1_16_33_3', '_6_2_4_3_4_3', '_33_6_16', '_6_6_3_3_3_3', '_6_6_3_3_3_4_3',
                                           '_6_6_3_4_3_3_3', '_6_6_5_3_3_3_3', '_6_3_4_16_33_4_10_3_4', '_6_6_3_3_3_4_3_4', 
                                           '_16_16_33_4_3_4_3_4_16', '_16_16_2_4_33_33', '_6_6_5_3_3_3_4_3', '_6_6_16_33_33_4_33_3_3'), stringr::str_remove_all(cod, '\\;'), cod)) %>% 
  dplyr::mutate(cod = ifelse(cod %in% c('_6(11a)[1];_6(19a)[1]_18(0)[0]_3(51c)[0]_3(52c)[0]_3(11c)[0]', 
                                        '_6(11p)[1];_6(19p)[1]_9(0)[0]_18(0)[0]_3(12c)[0]_3(22c)[0]_3(61c)[0]_8(0)[0]'), str_remove_all(cod, "\\;"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod %in% c('_12_33_4_16'), stringr::str_replace(cod, "(\\;_33\\(.{1,3}\\)\\[0\\])(\\;)(_4\\(.{1,3}\\)\\[1\\])(_16\\(.{1,3}\\)\\[1\\]$)", "\\1\\3\\4"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod %in% c('_16_33_3_4_6_3_3'), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])", ";\\1"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod %in% c('_1_16_33_4_6_3_4'), stringr::str_replace(cod, "(\\;)(_4\\(.{1,3}\\)\\[.\\])(\\;)", "\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod %in% c('_6_3_4_3_3_6_3'), stringr::str_replace(cod, "(\\;)(_4\\(.{1,3}\\)\\[.\\])", "\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod %in% c('_6_3_3_6_3_3_2'), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[.\\])(_6\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod == '_6_3_4_3_6_6_5_16_16_16_33_33_4_33', stringr::str_replace(cod, "(\\;)(_3\\(12w\\))", "\\2"), cod),
                cod = ifelse(modcod == '_6_3_4_3_6_6_5_16_16_16_33_33_4_33', stringr::str_replace(cod, "(_3\\(22c\\)\\[0\\])", "\\1;"), cod)) %>% 
  dplyr::mutate(cod = ifelse(cod == '_6(13a)[1]_5(13p)[0]_18(2)[0]_16(01p)[0]_33(40w)[0];_4(1t)[1];_6(19a)[1]_3(51c)[0]_3(52c)[0]_8(0)[0]_8(0)[0]', stringr::str_replace(cod, "(_5\\(13p\\)\\[0\\])", "\\1;"), cod)) %>% 
  dplyr::mutate(modcod2 = cod) %>% 
  dplyr::mutate(modcod2 = stringr::str_remove_all(modcod2, "_6\\(18.{1}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_7\\(.{1,3}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_8\\(.{1,3}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_9\\(.{1,3}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_10\\(0\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_11\\(.{1,3}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_16\\(4.{2}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_18\\(.\\)\\[.\\]")) %>% 
  dplyr::mutate(modcod2 = stringr::str_remove_all(modcod2, "\\(.{1,3}\\)\\[.\\]")) %>%
  dplyr::mutate(cod = ifelse(modcod2 == '_6_3;_3;_10', stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\1\\3\\4"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod2 == '_6_3;_4;_10', stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;)(_4\\(.{1,3}\\)\\[.\\])", "\\1\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod2 %in% c('_6_3;_4;_5', '_1;_6_3;_4;_5', '_4;_6_3;_4;_5') & stringr::str_detect(cod, "_5\\((19|20)p\\)"), stringr::str_replace(cod, "(\\;)(_4\\(.{1,3}\\)\\[.\\])(\\;)(_5\\(.{1,3}\\)\\[.\\])", "\\2\\4"), cod),
                cod = ifelse(modcod2 %in% c('_6_3;_4;_5', '_1;_6_3;_4;_5', '_4;_6_3;_4;_5') & stringr::str_detect(cod, "_5\\((19|20)p\\)"), stringr::str_replace(cod, "(\\;)(_4\\(.{1,3}\\)\\[.\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(\\;)(_5\\(.{1,3}\\)\\[.\\])", "\\2\\5"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod2 == '_1;_6;_16_33;_5', stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(\\;)(_16\\(.{1,3}\\)\\[.\\]_33\\(.{1,3}\\)\\[.\\])(\\;)", "\\1\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod2 == '_4;_16_33_3_3_6', stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])", "\\1;"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod2 == '_6_3;_10_3;_4', stringr::str_replace(cod, "(_3\\(12c\\)\\[0\\])(\\;)", "\\1"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod2 == '_6;_6;_3_6_5_3_3', stringr::str_replace(cod, "(\\;)(_3\\(11c\\)\\[0\\])", "\\2\\1"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod2 == '_6_3;_16_16_33_33_4_3', stringr::str_remove_all(cod, "\\;"), cod)) %>% 
  
  dplyr::mutate(cod = ifelse(modcod == '_6_3_3_5_10', stringr::str_remove_all(cod, "\\;"), cod),
                cod = ifelse(modcod == '_6_3_3_5_10', stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_6\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod2 %in% c('_6_3;_3_6_3', '_4;_6_3;_3_6_3', '_6_3;_3_6_3_3'), stringr::str_replace(cod, "(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\2\\3\\1"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod2 %in% c('_6_3;_4_3_6_3', '_6_3;_3_6_3_4_3', '_6_3;_4_3_6_3_3', '_4;_6_3;_4_3_6_3_3'), stringr::str_replace(cod, "(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\2\\3\\1"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod2 == '_6_3;_4;_5;_10', stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;)(_4)", "\\1\\3"), cod),
                cod = ifelse(modcod2 == '_6_3;_4;_5;_10', stringr::str_replace(cod, "(\\;)(_5)", "\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod2  == '_2;_4;_6_3;_4;_16_33_3', stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[.\\])(\\;)(_4\\(.{1,3}\\)\\[.\\])(\\;)", "\\1\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod == '_6_3_3_6_5_16_16_33_33', stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6)", "\\1\\2;\\3"), cod),
                cod = ifelse(modcod == '_6_3_3_6_5_16_16_33_33', stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[0\\])(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>%  
  dplyr::mutate(cod = ifelse(modcod == '_6_6_3_4_3_4_3_3_4', stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(\\;)(_6\\(.{1,3}\\)\\[1\\])", "\\1\\3"), cod)) %>%  
  dplyr::mutate(cod = ifelse(modcod2 == '_4;_16_33;_16_33_6_3_3;_4', stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(\\;)(_16)", "\\1\\3"), cod),
                cod = ifelse(modcod2 == '_4;_16_33;_16_33_6_3_3;_4', stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>%  
  
  dplyr::mutate(cod = ifelse(modcod2 == '_6_3;_3_16_16_33_33;_4', stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\1\\3\\4\\2"), cod)) %>%  
  dplyr::mutate(cod = ifelse(modcod2 == '_6_3;_4_3_6_3_4_3', stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\1\\3\\4\\2"), cod)) %>%  
  dplyr::mutate(cod = ifelse(modcod2 == '_2;_4;_6_3;_16_16_33_33_3', stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;)", "\\1"), cod)) %>%  
  dplyr::mutate(cod = ifelse(modcod2 == '_4;_6_3;_4_3_6_3_4_3', stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_3\\(.{1,3}\\)\\[0\\])", "\\1\\3\\4\\2"), cod)) %>%  
  dplyr::mutate(cod = ifelse(modcod2 == '_4;_6_3;_16_16_33_4_33_4_3', stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(\\;)", "\\1"), cod)) %>%  
  dplyr::mutate(cod = ifelse(modcod2 == '_6;_6;_3;_6_5', stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(\\;)(_3\\(.{1,3}\\)\\[0\\])", "\\1\\3"), cod)) %>%  
  
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod2, "_6_16(_33\\;|_33$|_16_33_33\\;|_16_33_33$|_33_16_33\\;|_33_16_33$)"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_16(_33\\;|_33$|_16_33_33\\;|_16_33_33$|_33_16_33\\;|_33_16_33$)"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod2, "_6_3_16_33\\;"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_16_33\\;"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_16_33$"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_16_33$"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_16_33_33\\;"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_16_33_33$"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_16_33\\;"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_16_33\\;"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_16_33$"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_16_33$"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_16_33_33\\;"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_16_33_33$"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[.\\])", "\\1\\2;\\3"), cod),

                cod = ifelse(stringr::str_detect(modcod2, "_6_3_4_3_16_33\\;"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_4_3_16_33\\;"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_4_3_16_33$"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_4_3_16_33$"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_4_3_16_33_33\\;"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_4_3_16_33_33$"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_3_16_33\\;"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_3_16_33\\;"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_3_16_33$"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_3_16_33$"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_3_16_33_33\\;"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_3_3_3_16_33_33$"), stringr::str_replace(cod, "((?:_3\\(.{1,3}\\)\\[0\\])+)((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod2, "_6_16(_33$|_33\\;)"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod2, "_6_16(_33$|_33\\;)"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])((?:_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_16\\(.{1,3}\\)\\[.\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod, "_7\\(6p\\)") & stringr::str_detect(modcod, "_10_5"), stringr::str_replace(cod, "(\\;)(_5\\(20p\\))", "\\2"), cod)) %>% 
  dplyr::mutate(modcod2 = cod) %>% 
  dplyr::mutate(modcod2 = stringr::str_remove_all(modcod2, "_6\\(18.{1}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_7\\(.{1,3}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_8\\(.{1,3}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_9\\(.{1,3}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_10\\(0\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_11\\(.{1,3}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_16\\(4.{2}\\)\\[.\\]"),
                modcod2 = stringr::str_remove_all(modcod2, "_18\\(.\\)\\[.\\]")) %>% 
  dplyr::mutate(modcod2 = stringr::str_remove_all(modcod2, "\\(.{1,3}\\)\\[.\\]"))

actions <- xdf1 %>% 
  dplyr::select(cod) %>% 
  dplyr::mutate(x = stringr::str_split(cod, "_")) %>% 
  tidyr::unnest(x) %>% 
  dplyr::filter(x != "") %>% 
  dplyr::mutate(x = stringr::str_remove_all(x, '\\[.\\]|\\;')) %>% 
  dplyr::distinct(x) %>%
  dplyr::arrange(x) %>% 
  dplyr::mutate(x = paste0("_", x),
                x = stringr::str_replace_all(x, "\\(", "\\\\("),
                x = stringr::str_replace_all(x, "\\)", "\\\\)")) %>% 
  dplyr::pull()


obj <- plyr::llply(1:length(actions), .fun = function(i){
  
  xdf1 %>% 
    dplyr::filter(stringr::str_detect(cod, actions[i])) %>% 
    dplyr::group_by(np_val) %>% 
    dplyr::summarise(pattern = actions[i],
                     num = dplyr::n(),
                     cnt = sum(n),
                     pattern_only = sum(stringr::str_detect(cod, paste0("^", actions[i], "\\[.\\]$"))),
                     pattern_first = sum(stringr::str_detect(cod, paste0("^", actions[i]))),
                     pattern_last = sum(stringr::str_detect(cod, paste0(actions[i], "\\[.\\]$"))),
                     comma_pattern = sum(stringr::str_detect(cod, paste0("\\;", actions[i]))),
                     pattern_comma = sum(stringr::str_detect(cod, paste0(actions[i], "\\[.\\]\\;"))),
                     comma_pattern_comma = sum(stringr::str_detect(cod, paste0("\\;", actions[i], "\\[.\\]\\;")))) %>% 
    dplyr::mutate(pattern_mid = num - pattern_first - pattern_last) %>% 
    dplyr::select(pattern, np_val, num, cnt, pattern_only, pattern_first, pattern_mid, 
                  pattern_last, comma_pattern, pattern_comma, comma_pattern_comma)
  
}, .progress = 'time') %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(pattern = stringr::str_remove_all(pattern, "\\\\"))




xdf1 %>% 
  dplyr::mutate(x = stringr::str_split(cod, '\\;')) %>% 
  tidyr::unnest(x) %>% 
  dplyr::select(x, n, original_cod) %>% 
  dplyr::rename(cod = x) %>% 
  dplyr::mutate(cod = stringr::str_remove_all(cod, '\\[.\\]')) %>% 
  dplyr::mutate(cod = stringr::str_replace_all(cod, '\\)', ')[1]')) %>% 
  change_cod() %>% 
  simplify_cod() %>% 
  dplyr::mutate(ind_tlane = stringr::str_count(original_cod, "_5\\((19|20)p\\)"),
                ind_vlane = stringr::str_count(original_cod, "_7\\(6p\\)")) %>% 
  dplyr::mutate(codfinal = NA_character_) %>% 
  dplyr::group_by(codfinal, modcod, ind_tlane, ind_vlane) %>% 
  dplyr::summarise(n = sum(n), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::mutate(perc = 100 * cumsum(n) / sum(n),
                perc = as.character(round(perc, 4))) %>%
  print(., n = 300)

dplyr::mutate(codfinal = ifelse(modcod %in% c('_1', '_6_1_16_33_3', '_6_1_3'), '_1', NA_character_),
              codfinal = ifelse(modcod %in% c('_2'), '_2', codfinal),
              codfinal = ifelse(modcod %in% c('_4'), '_4', codfinal),
              codfinal = ifelse(modcod %in% c('_4_5'), '_4_5', codfinal),
              codfinal = ifelse(modcod %in% c('_5'), '_5', codfinal),
              codfinal = ifelse(modcod %in% c('_6'), '_6', codfinal),
              codfinal = ifelse(modcod %in% c('_6_5', '_5_6'), '_6_5', codfinal),
              codfinal = ifelse(modcod %in% c(''), '', codfinal),
              codfinal = ifelse(modcod %in% c('_16_33_10_16_33', '_10'), '_10', codfinal),
              codfinal = ifelse(modcod %in% c('_10_10_4', '_10_4'), '_10_4', codfinal),
) %>% 
  


xdf1 %>% 
  dplyr::mutate(x = stringr::str_split(cod, '\\;')) %>% 
  tidyr::unnest(x) %>% 
  dplyr::group_by(x) %>% 
  dplyr::summarise(n = sum(n)) %>% 
  dplyr::mutate(val = stringr::str_extract(x, "_(12|13|1|2)\\(.{1,3}\\)\\[.\\]"),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(52c\\)"), "_3(52c)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(52w\\)"), "_3(52w)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(22c\\)"), "_3(22c)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(22w\\)"), "_3(22w)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(11c\\)"), "_3(11c)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(11w\\)"), "_3(11w)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(33c\\)"), "_3(33c)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(33w\\)"), "_3(33w)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(61c\\)"), "_3(61c)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(61w\\)"), "_3(61w)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(72c\\)"), "_3(72c)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(72w\\)"), "_3(72w)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(83c\\)"), "_3(83c)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_3\\(83w\\)"), "_3(83w)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_4\\(.{1,3}\\)") & stringr::str_detect(x, "_5\\(37t\\)"), "_4_5(37t)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_4\\(.{1,3}\\)"), stringr::str_extract(x, "_4\\(.{1,3}\\)"), val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_6\\(.{1,3}\\)") & stringr::str_detect(x, "_5\\(.{1,3}\\)"), "_6(13a)_5(13a)", val),
                val = ifelse(is.na(val) & stringr::str_detect(x, "_5\\(.{1,2}p\\)"), stringr::str_extract(x, "_5\\(.{1,2}p\\)"), val)) %>% 
  dplyr::mutate(val = stringr::str_remove_all(val, '\\[.\\]')) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::group_split(val)

dplyr::filter(is.na(val))

xdf1 %>% 
  dplyr::mutate(cod = stringr::str_replace_all(cod, "\\[.\\]", "[0]")) %>% 
  dplyr::mutate(cod = stringr::str_replace_all(cod, "(\\;_[0-9]+\\(.{1,3}\\))(\\[0\\])", "\\1[1]")) %>% 
  dplyr::mutate(val = stringr::str_remove_all(cod, "_[0-9]+\\(.{1,3}\\)"),
                val = stringr::str_remove_all(val, "\\[|\\]|\\;"))

xdf1 %>% 
  dplyr::mutate(aux = stringr::str_count(modcod, "_")) %>% 
  dplyr::group_by(aux, val) %>% 
  dplyr::summarise(n = sum(n), num_linhas = dplyr::n()) %>% 
  dplyr::arrange(desc(num_linhas))

obj <- xdf1 %>% 
  dplyr::select(-original_cod) %>% 
  dplyr::mutate(aux = stringr::str_count(modcod, "_")) %>% 
  dplyr::group_by(aux, val) %>% 
  dplyr::mutate(cnt = sum(n), 
                num_linhas = dplyr::n()) %>% 
  dplyr::arrange(desc(num_linhas), desc(aux), desc(cnt)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(grupo = dplyr::row_number()) %>% 
  dplyr::group_by(aux, val) %>% 
  dplyr::mutate(grupo = min(grupo)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-c(ind, np_val, val_play, ind11, aux, cnt, num_linhas)) %>% 
  dplyr::group_split(grupo)





xdf1 %>% 
  dplyr::filter(stringr::str_detect(cod, "_7\\(1t\\)\\[0\\]_16\\(01p\\)\\[1\\]_16\\(01c\\)\\[0\\]_9\\(1\\)\\[0\\]_18\\(5\\)\\[0\\]_2\\(1b\\)\\[1\\]_4\\(0p\\)\\[1\\]_33\\(40c\\)\\[0\\]_33\\(40c\\)\\[0\\]_8\\(0\\)\\[0\\]"))


obj %>% 
  dplyr::bind_rows() %>% 
  dplyr::filter(grupo >= 33300) %>% 
  print(., n = 100)


xdf1 %>% 
  dplyr::mutate(aux = stringr::str_count(modcod, "_")) %>% 
  dplyr::arrange(aux, modcod, modcod2, np, val, desc(n)) %>% 
  dplyr::select(-original_cod) %>% 
  dplyr::group_split(aux) %>% 
  '[['(1) %>% 
  dplyr::slice(0001:10000) %>% 
  print(., n = 1000)

xdf1 %>% 
  dplyr::group_by(modcod, modcod2) %>% 
  dplyr::summarise(n = sum(n), .groups = 'drop_last') %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::mutate(ind = dplyr::n_distinct(modcod2),
                ind = ifelse(ind > 1, ind, 0)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(cnt)) %>% 
  print(., n = 900)

xdf1 %>% 
  dplyr::filter(stringr::str_detect(modcod2, '_6_5')) %>% 
  dplyr::group_split(modcod2)

xdf1 %>% 
  dplyr::filter(stringr::str_detect(modcod, "_6_3_4_5"))


xdf1 %>% 
  dplyr::mutate(x = stringr::str_split(modcod2, ';')) %>% 
  tidyr::unnest(x) %>% 
  dplyr::group_by(x) %>% 
  dplyr::summarise(n = sum(n)) %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 300)

xdf1 %>% 
  dplyr::filter(modcod == '_6_3_3_6_3') %>% 
  dplyr::group_split(modcod2)

xdf1 %>% 
  dplyr::group_by(np_val, modcod, modcod2) %>% 
  dplyr::summarise(n = sum(n), .groups = 'drop_last') %>%
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(np_val, desc(cnt), desc(n)) %>% 
  dplyr::group_split(np_val) %>% 
  '[['(6) %>% 
  print(., n = 1000)

#################################################################


xdf1 %>% 
  dplyr::filter(modcod %in% x) %>% 
  dplyr::arrange(np, modcod) %>% 
  dplyr::select(-c(original_cod, val)) %>% 
  dplyr::filter(np <= 20) %>% 
  print(., n = 1000)



_4(0t)[1];_5(37t)[0];_5(34p)[1]
_2(3)[1];_4(0t)[1]_5(37t)[0]_13(2)[1]
_2(5)[1];_4(0t)[1];_5(37t)[0];_5(11p)[1]
_4(0t)[1];_5(37t)[0];_9(0)[0]_5(34p)[1]
_4(0t)[1];_5(37t)[0];_8(0)[0]_5(34p)[1]
_5(39t)[1];_16(08t)[1]_8(0)[0]_33(40c)[0]
_6(11a)[1]_3(11w)[0];_4(1t)[1];_5(20p)[1]
_6(10p)[1]_3(12w)[0];_4(0t)[1];_5(20p)[1]
_6(11a)[1]_8(0)[0]_3(11w)[0];_4(1t)[1];_5(20p)[1]
_6(11a)[1]_3(11w)[0];_4(1t)[1];_5(19p)[1]_8(0)[0]
_6(11a)[1]_3(11w)[0];_4(1t)[1];_7(3p)[0]_5(20p)[1]
_1(3a)[1];_6(11p)[1]_3(11c)[0]_9(0)[0]_6(15p)[1]_3(11w)[0];_4(1t)[1]



x <- c('_3_3_4_6', '_3_3_3_4_6',  
'_6_3_3_3_5', 
'_6_3_3_5', '_6_3_3_6_5', '_6_3_3_6_5_3', '_6_3_4_3_5', '_6_3_4_3_3_5', 
'_6_3_4_3_6_5_3', '_6_3_4_5', 
'_6_3_4_3_6_5', '_6_3_5', '_6_3_5_3', '_6_3_6', '_6_3_6_5', '_6_5', '_6_5_10', 
'_6_5_13', '_6_5_16_33_3_3', '_6_5_16_33_4_3_3', '_6_5_16_33_4_3_4_3', '_6_13', '_16_3_6_3_4_3', '_16_33_3_4_6_3_3', 
'_16_33_6', '_16_33_6_5', '_33_4_16', '_33_16_6_3_4_3_3',
'_6_5_3_3_16_33',
'_6_5_6_3_3_3', '_6_6_3_3_3_3', '_6_6_3_4_3_3_3', '_6_6_5_3_3_3_3', '_6_6_5_3_3_3_4_3',
'_4_5_6', '_1_6_3_5', '_4_5_6_3_4_3', '_4_5_16_33', '_6_16_16_33_33', '_1_6_3_6',
'_4_6_3_5', '_1_6_3_6_3_3', '_4_6_3_4_5', '_6_16_33_16_33', '_1_6_3_6_3', '_16_33_4_16_33',
'_16_33_4_5', '_1_6_3_6_5', '_6_16_33_6_5', '_4_5_13', '_4_16_13', '_6_16_16_3_3',
'_2_4_33_16', '_6_16_16', '_6_16_33_6_3', '_6_3_10_3_4', '_6_3_3_16_33_16_33',
'_6_6_3_3_3_4_3_4', '_6_6_6_3_4_3', '_12_33_4_16', '_16_33_6_5_3_4_3_4', '_1_13_13',
'_2_4_6_3_4','_3_3_2_4_6', '_3_4_4_6_3_4', '_4_16_13_33', '_4_16_33_3_3_6', '_4_16_33_6_5',
'_4_3_3_4_6', '_4_6_3_3_6_3_3_3', '_4_6_3_4_10', '_5_16_33_6', '_5_16_33_6_5', '_5_3_4_6_3',
'_6_16_33_6', '_6_16_6_5_33_3_3', '_6_1_16_33_3', '_6_2_4_3_3', '_6_2_4_3_4_3',
'_6_3_3_16_16', '_6_3_3_16_16_33_33', '_6_3_3_6_1', '_6_3_3_6_16_33_3', '_6_3_3_6_16_33_3_3',
'_6_3_3_6_3_3_2', '_6_3_4_10_5', '_6_3_4_3_16_16_33_33', '_6_3_4_3_5_13', '_6_5_16_33_3_3_4',
'_6_5_16_33_3_4_3_4', '_6_5_16_3_4_3_4_33_4', '_6_6_3_4_3_4_3_3_4', '_6_6_6_5',
'_6_5_16_33_4_16_33', '_2_4_16_13', '_6_16_33_16_33_4', '_6_16_33_4_16_33_3_3', '_2_4_5_13',
'_16_33_4_16_33_4', '_1_6_3_6_3_4', '_1_6_3_16_16_33_33', '_2_4_6_1_3', '_4_1_6_3_5', '_4_6_16_16_33_33',
'_4_6_16_16_33_4_33', '_5_6_16_16_33_33', '_6_3_3_16_16_33_33_4', '_16_16_2_4_33_33', '_16_16_33_4_3_4_3_4_16',
'_1_16_33_4_16_33', '_1_6_3_6_16_33_3', '_1_6_3_6_3_5',  '_2_4_16_33_6', '_2_4_16_33_6_5',
'_2_4_6_3_3_6_5_3', '_2_4_6_5_16_33_4_3_3', '_4_16_33_10_16_33', '_4_6_16_33_16_33',
'_4_6_3_6_16_33_3', '_4_6_5_16_33_3_3_4', '_4_6_5_16_33_5', '_4_6_6_6_5', '_5_16_33_4_16_33',
'_5_5_5_2', '_5_6_16_16', '_5_6_16_16_33_4_33', '_6_16_16_16_33_33_33', '_6_16_16_33_33_3_4_6_3_3_3',
'_6_16_16_33_33_4', '_6_16_33_4_16_33', '_6_16_33_4_16_33_3_4_3', '_6_16_33_4_3_3_4_16_33',
'_6_16_33_4_6_3', '_6_16_33_6_3_4', '_6_16_33_6_3_4_3_4', '_6_3_16_33_4_16_33_3',
'_6_3_3_6_5_16_16_33_33', '_6_5_6_5_16_33_3_4_3_4',
'_2_4_6_16_16_33_33', '_2_4_6_16_33_6_3_4_3', '_1_6_3_6_2_4', '_1_6_5_16_33_3_4_6_3_3',
'_2_10_4_16_33_6_5_3_3', '_2_4_16_33_6_5_16_33', '_2_4_5_10_5', '_2_4_6_16_16_33_4_33',
'_2_4_6_3_4_16_33_3', '_2_4_6_3_4_5_6_5', '_4_16_33_16_33_6_3_3_4', '_4_16_33_6_3_3_4_13',
'_4_2_4_16_16', '_4_2_4_6_3_4_4_3', '_5_1_6_3_6_3_4', '_6_16_16_16_16',
'_6_16_16_16_16_33_33_33_33', '_6_16_16_33_16_16_33_33_33', '_6_16_16_33_33_4_16_33_3_3',
'_6_16_16_33_33_4_16_33_3_4_3', '_6_3_4_16_33_4_10_3_4',
'_5_6_16_16_16_16', '_6_3_4_3_6_6_5_16_16_16_33_33_4_33', '_6_5_1_6_3_6_3_3_4_2',
'_2_4_6_3_3_6_16_33_16_33_4', '_4_6_16_33_4_16_33_4_16_33_3_3')

## np_val == 2
# _6_5, _6_3_3_5, _6_3_5, _6_3_4_5, _6_3_4_3_5, _6_3_3_6_5, _16_33_6,
# _6_3_3_3_5, _6_5_16_33_3_3, _16_33_6_5, _6_3_3_3_16_33, _6_3_4_10,
# _6_3_4_3_6_5, _3_3_4_6, _6_3_4_3_3_5, _6_3_3_6_5_3, _6_3_6_5, _6_5_13,
# _6_13, _6_3_3_10, _6_3_6, _6_5_10, _16_33_3_4_6_3_3, _16_3_6_3_4_3,
# _33_16_6_3_4_3_3, _33_4_16, _3_3_3_4_6, _6_3_3_3_6_5, _6_3_4_3_6_5_3, 
# _6_3_5_3,  _6_5_16_33_4_3_3, _6_5_16_33_4_3_4_3, _6_5_3_3_16_33, 
# _6_5_6_3_3_3, _6_6_3_3_3_3, _6_6_3_4_3_3_3, _6_6_5_3_3_3_3, _6_6_5_3_3_3_4_3

## np_val == 3
# _4_5_6, _1_6_3_5, _4_5_6_3_4_3, _4_5_16_33, _6_16_16_33_33, _1_6_3_6,
# _4_6_3_5, _1_6_3_6_3_3, _4_6_3_4_5, _6_16_33_16_33, _1_6_3_6_3, _16_33_4_16_33,
# _16_33_4_5, _1_6_3_6_5, _6_16_33_6_5, _4_5_13, _4_16_13, _6_16_16_3_3, 
# _2_4_33_16, _6_16_16, _6_16_33_6_3, _6_3_10_3_4, _6_3_3_16_33_16_33, 
# _6_6_3_3_3_4_3_4, _6_6_6_3_4_3, _12_33_4_16, _16_33_6_5_3_4_3_4, _1_13_13,
# _2_4_6_3_4, _3_3_2_4_6, _3_4_4_6_3_4, _4_16_13_33, _4_16_33_3_3_6, _4_16_33_6_5, 
# _4_3_3_4_6, _4_6_3_3_6_3_3_3, _4_6_3_4_10, _5_16_33_6, _5_16_33_6_5, _5_3_4_6_3,
# _5_4_2, _6_16_33_6, _6_16_6_5_33_3_3, _6_1_16_33_3, _6_2_4_3_3, _6_2_4_3_4_3,
# _6_3_3_16_16, _6_3_3_16_16_33_33, _6_3_3_6_1, _6_3_3_6_16_33_3, _6_3_3_6_16_33_3_3,
# _6_3_3_6_3_3_2, _6_3_4_10_5, _6_3_4_3_16_16_33_33, _6_3_4_3_5_13, _6_5_16_33_3_3_4,
# _6_5_16_33_3_4_3_4, _6_5_16_3_4_3_4_33_4, _6_6_3_4_3_4_3_3_4, _6_6_6_5 

## np_val == 4
# _6_5_16_33_4_16_33, _2_4_16_13, _6_16_33_16_33_4, _6_16_33_4_16_33_3_3, _2_4_5_13,
# _16_33_4_16_33_4, _1_6_3_6_3_4, _1_6_3_16_16_33_33, _2_4_6_1_3, _4_1_6_3_5, _4_6_16_16_33_33,
# _4_6_16_16_33_4_33, _5_6_16_16_33_33, _6_3_3_16_16_33_33_4, _16_16_2_4_33_33, _16_16_33_4_3_4_3_4_16,
# _1_16_33_4_16_33, _1_6_3_6_16_33_3, _1_6_3_6_3_5,  _2_4_16_33_6, _2_4_16_33_6_5,
# _2_4_6_3_3_6_5_3, _2_4_6_5_16_33_4_3_3, _4_16_33_10_16_33, _4_6_16_33_16_33, 
# _4_6_3_6_16_33_3, _4_6_5_16_33_3_3_4, _4_6_5_16_33_5, _4_6_6_6_5, _5_16_33_4_16_33,
# _5_5_5_2, _5_6_16_16, _5_6_16_16_33_4_33, _6_16_16_16_33_33_33, _6_16_16_33_33_3_4_6_3_3_3,
# _6_16_16_33_33_4, _6_16_33_4_16_33, _6_16_33_4_16_33_3_4_3, _6_16_33_4_3_3_4_16_33,
# _6_16_33_4_6_3, _6_16_33_6_3_4, _6_16_33_6_3_4_3_4, _6_3_16_33_4_16_33_3, 
# _6_3_3_6_5_16_16_33_33, _6_5_6_5_16_33_3_4_3_4, 

## np_val == 5
# _2_4_6_16_16_33_33, _2_4_6_16_33_6_3_4_3, _1_6_3_6_2_4, _1_6_5_16_33_3_4_6_3_3,
# _2_10_4_16_33_6_5_3_3, _2_4_16_33_6_5_16_33, _2_4_5_10_5, _2_4_6_16_16_33_4_33,
# _2_4_6_3_4_16_33_3, _2_4_6_3_4_5_6_5, _4_16_33_16_33_6_3_3_4, _4_16_33_6_3_3_4_13,
# _4_2_4_16_16, _4_2_4_6_3_4_4_3, _5_1_6_3_6_3_4, _6_16_16_16_16,
# _6_16_16_16_16_33_33_33_33, _6_16_16_33_16_16_33_33_33, _6_16_16_33_33_4_16_33_3_3,
# _6_16_16_33_33_4_16_33_3_4_3, _6_3_4_16_33_4_10_3_4, 

## np_val == 6
# _5_6_16_16_16_16, _6_3_4_3_6_6_5_16_16_16_33_33_4_33, _6_5_1_6_3_6_3_3_4_2

## np_val == 7
# _2_4_6_3_3_6_16_33_16_33_4, _4_6_16_33_4_16_33_4_16_33_3_3, 

#################################################################

# _3_6_3       _3;_6_3
# _6_3_4       _6_3_4
# _3_4_3_6_3   _3_4_3;_6_3
# _12_3        _12_3
# _12_3_3      _12_3_3
# _5_6_3_4_3   _5_6_3_4_3


# A tibble: 284 × 2
# 30 "_6_3_4_5"                           103
# 36 "_3"                                  46
# 43 "_6_16_16_33_33"                      30
# 47 "_6_3_3_6_5"                          20
# 49 "_6_3_3_16_33"                        18
# 50 "_6_3_6"                              18
# 51 "_12_33"                              17

32 "_6_3_4_5"                           103 ??????????
    "_1;_6_3;_4;_5"
    "_4;_6_3;_4;_5"
    "_6_3;_4;_5"
    "_6_3;_4;_5;_10"

38 "_3"                                  46 ??????????
    "_1;_3;_4;_6"
    "_2;_4;_6;_1;_3"
    "_3"
    "_3;_4;_4;_6_3_4"
    "_3;_6_3"
    "_3;_6_3;_4"
    "_4;_3;_6_3"
    "_4;_3;_6_3;_4"
    "_6;_1;_3"
    "_6;_1;_3;_4"

43 "_6_3_4_3_16_33"                      38
    "_2;_4;_2;_4;_6_3_4_3_16_33;_4"
    "_2;_4;_6_3_4_3_16_33"
    "_4;_6_3_4_3_16_33"
    "_5;_6_3_4_3_16_33"
    "_6_3_4_3_16_33"
    "_6_3_4_3_16_33;_4"

45 "_6_3_16_33"                          37
    "_1;_6_3_16_33"
    "_1;_6_3_16_33;_4"
    "_2;_4;_1;_6_3_16_33;_4"
    "_4;_1;_6_3_16_33"
    "_4;_6;_6_3_16_33"
    "_5;_1;_6_3_16_33"
    "_6_3_16_33"
    "_6_3_16_33;_4_16_33_3"

51 "_6_3_3_6_5"                          20
    "_2;_4;_6_3_3_6_5_3"
    "_6_3_3_6_5"
    "_6_3_3_6_5;_16_16_33_33"
    "_6_3_3_6_5_3"
    "_6_3_3_6_5_3;_4"

52 "_3_6_3"                              19 ??????????
    "_1;_6_3_6_3;_5"
    "_2;_4;_2;_4;_2;_4;_6_3;_3_6_3"
    "_2;_4_3;_6_3"
    "_3_4_3;_6_3"
    "_3;_6_3"
    "_3;_6_3;_4"
    "_4;_3;_6_3"
    "_4;_3;_6_3;_4"
    "_4;_6_3;_3_6_3"
    "_4;_6_3;_3_6_3"
    "_4;_6_3_3_6_3_3_3"
    "_4;_6_3;_4_3_6_3_3"
    "_4;_6_3;_4_3_6_3_4_3"
    "_6_3;_3_6_3"
    "_6_3;_3_6_3_3"
    "_6_3_3_6_3_3;_2"
    "_6_3;_3_6_3;_4"
    "_6_3;_3_6_3_4_3"
    "_6_3;_4_3_3_6_3"
    "_6_3;_4_3_6_3"
    "_6_3;_4_3_6_3_3"
    "_6_3;_4_3_6_3_4_3"
    "_6_3;_6_3;_4"
    "_6_5;_1;_6_3_6_3_3;_4;_2"


53 "_6_3_6"                              18
54 "_12_33"                              17
56 "_5_6"                                15 ??????????
57 "_4_16_33"                            14
58 "_6_16_33_16_33"                      14
59 "_16_33_6"                            13
60 "_3_6"                                13
64 "_16_33_6_5"                          10
66 "_16_33_3_3"                           9
67 "_3_3"                                 9
68 "_4_16"                                9
71 "_5_5"                                 8
73 "_6_3_4_10"                            8
75 "_6_3_4"                               7
76 "_6_3_6_5"                             7
77 "_16_3"                                6
81 "_6_3_3_3_16_33"                       6
82 "_6_3_4_3_6_5"                         6
86 "_16_33_3_4_3"                         5
87 "_4_16_33_3_3"                         5
88 "_6_16_16_33_4_33"                     5
91 "_6_3_3_6_5_3"                         5
93 "_3_4_6_3"                             4 ??????????
94 "_3_6_3_3"                             4 ??????????
98 "_6_16_33_6_5"                         4
100 "_10_3"                                3
103 "_16_33_3"                             3
104 "_3_4_3"                               3
105 "_3_4_3_3_6"                           3
106 "_4_16_33_4"                           3
107 "_4_3_6_3"                             3
108 "_4_5_13"                              3
109 "_6_16_16"                             3
119 "_6_3_3_16_16_33_33"                   3
121 "_6_3_3_5"                             3
122 "_6_5_13"                              3
123 "_6_6_5"                               3
125 "_16_16_16_33_33_4_33"                 2
126 "_16_16_33_33_16_33"                   2
127 "_16_33_4_3_3"                         2
128 "_16_33_6_3_3"                         2
129 "_33"                                  2
130 "_3_10"                                2
131 "_3_3_3"                               2
132 "_3_3_3_6"                             2
133 "_4_16_33_3_4_3"                       2
134 "_4_16_6_33_3_3"                       2
135 "_4_3"                                 2
136 "_4_33_16"                             2
137 "_4_3_6_3_3"                           2
138 "_4_3_6_3_4_3"                         2
139 "_5_3"                                 2
140 "_6_13"                                2
141 "_6_16_16_16_16"                       2
145 "_6_16_3_33"                           2 ??????????
146 "_6_3_16_16_33_33"                     2
150 "_6_3_3_16_33_16_33"                   2
151 "_6_3_4_3_5"                           2
152 "_6_3_6_16_33_3"                       2
153 "_6_5_10"                              2
154 "_6_5_3_4_16_33_3"                     2
155 "_10_10_4"                             1
156 "_10_16_33"                            1
157 "_12_3"                                1
158 "_12_3_3"                              1
159 "_12_3_4_3"                            1
169 "_16_16_33_4_3_4_3"                    1
172 "_16_33_3_3_6"                         1
173 "_16_33_3_4_6_3_3"                     1
174 "_16_33_4_3_4_3"                       1
175 "_16_33_6_5_3_3"                       1
176 "_16_33_6_5_3_4_3"                     1
177 "_16_3_4_3_4_33"                       1
178 "_16_6_16"                             1
185 "_33_16_6_3_4_3_3"                     1
187 "_33_6_16"                             1
188 "_3_16"                                1
189 "_3_3_2"                               1
190 "_3_3_4_6_3"                           1
191 "_3_3_6_5"                             1
192 "_3_4_6_3_3"                           1
193 "_3_6_16_3"                            1
194 "_3_6_3_4_3"                           1
195 "_4_16_13_33"                          1
196 "_4_16_16"                             1
197 "_4_16_33_3"                           1
198 "_4_16_33_4_16_33_3_3"                 1
199 "_4_33_33"                             1
200 "_4_3_3"                               1
201 "_4_3_3_6"                             1
202 "_4_3_3_6_3"                           1
203 "_4_3_4_3"                             1
204 "_4_3_4_3_6"                           1
205 "_4_5_10"                              1
206 "_5_4"                                 1
207 "_5_6_3_4_3"                           1
210 "_6_16_16_16_16_33_33_33_33"           1
212 "_6_16_16_16_16_33_4_33_4_33_33"       1
214 "_6_16_16_16_33_33_33"                 1
219 "_6_16_16_33_16_16_33_33_33"           1
220 "_6_16_16_33_33_16_33_3_3"             1
223 "_6_16_16_33_33_3_4_6_3_3_3"           1
238 "_6_16_33_3_5"                         1
244 "_6_16_33_6"                           1
245 "_6_16_3_3"                            1
246 "_6_16_3_3_3"                          1
247 "_6_16_6_5_33_3_3"                     1
248 "_6_33_3_3_16"                         1
255 "_6_3_33_3"                            1
256 "_6_3_3_16_16"                         1
258 "_6_3_3_3_6_5"                         1
260 "_6_3_3_6"                             1
261 "_6_3_3_6_16_33_16_33"                 1
262 "_6_3_3_6_16_33_3"                     1
263 "_6_3_3_6_16_33_3_3"                   1
264 "_6_3_3_6_3_3"                         1
265 "_6_3_3_6_3_3_3"                       1
267 "_6_3_4_16_33"                         1
272 "_6_3_4_3_16_16_33_33"                 1
276 "_6_3_4_3_6_5_3"                       1
277 "_6_3_6_3"                             1
278 "_6_3_6_3_3"                           1
279 "_6_3_6_5_3"                           1
280 "_6_4_3_3"                             1
281 "_6_5_3_3_16_33"                       1
282 "_6_5_3_3_3_3"                         1
283 "_6_5_3_3_3_4_3"                       1
284 "_6_6_3_4_3"                           1




 # 70 "_6_16_33"                   "_6_16_33"                       1013    1013     0
# 125 "_6_16_33_4"                 "_6_16_33;_4"                     221     221     0
# 129 "_4_6_16_33"                 "_4;_6_16_33"                     202     202     0
# 138 "_2_4_6_16_33"               "_2;_4;_6_16_33"                  164     164     0
# 228 "_4_6_16_33_4"               "_4_6_16_33;_4"                     1      52     2
# 232 "_2_4_6_16_33_4"             "_2;_4;_6_16_33;_4"                47      47     0
# 273 "_5_6_16_33"                 "_5;_6_16_33"                      31      31     0
# 311 "_6_16_16_33_33"             "_6_16_16_33_33"                   21      21     0
# 314 "_5_6_16_33_4"                 "_5;_6_16_33;_4"                       11      11     0
# 323 "_4_2_4_6_16_33"               "_4;_2;_4;_6_16_33"                    10      10     0
# 428 "_6_16_33_16_33"             "_6_16_33_16_33"                    9       9     0

# 180 "_6_3_4_5"                   "_6_3;_4;_5"                        6      93     3
# 181 "_6_3_4_5"                   "_6_3;_4_5"                         2      93     3
# 182 "_6_3_4_5"                   "_6_3_4_5"                         85      93     3

# 186 "_6_3_4"                     "_6_3_4"                            4      87     2

# 187 "_6_3_3_16_33"               "_6_3_3_16_33"                     86      86     0

# 176 "_6_3_3_6_3"                   "_6_3;_3_6_3"                          16      42     2
# 177 "_6_3_3_6_3"                   "_6_3_3;_6_3"                          26      42     2
# 183 "_6_3_3_6_3_3"                 "_6_3;_3_6_3_3"                         4      39     2

# 206 "_6_3_3_6_3_4_3"               "_6_3;_3_6_3_4_3"                       1      30     2
# 207 "_6_3_3_6_3_4_3"               "_6_3_3;_6_3_4_3"                      29      30     2
# 210 "_6_3_4_3_16_33"               "_6_3_4_3_16_33"                       29      29     0
# 223 "_1_6_3_16_33"                 "_1;_6_3_16_33"                        25      25     0


# 305 "_6_1_3"                     "_6;_1;_3"                         20      23     2
# 306 "_6_1_3"                     "_6;_1_3"                           3      23     2

# 243 "_2_4_6_3_4"                   "_2;_4;_6_3;_4"                        18      19     2
# 244 "_2_4_6_3_4"                   "_2;_4;_6_3_4"                          1      19     2

# 324 "_6_3_3_6_5"                 "_6_3_3_6_5"                       19      19     0

# 255 "_6_3_4_3_6_3"                 "_6_3;_4_3_6_3"                         3      18     2
# 256 "_6_3_4_3_6_3"                 "_6_3_4_3;_6_3"                        15      18     2
# 258 "_6_3_4_3_6_3_3"               "_6_3;_4_3_6_3_3"                       1      17     2
# 259 "_6_3_4_3_6_3_3"               "_6_3_4_3;_6_3_3"                      16      17     2

# 347 "_6_5_6_3_3"                 "_6;_5;_6_3_3"                      1      16     2
# 348 "_6_5_6_3_3"                 "_6_5;_6_3_3"                      15      16     2
# 269 "_1_6_3_6"                     "_1;_6_3_6"                            15      15     0

# 362 "_12_33"                     "_12_33"                           13      13     0

# 371 "_6_3_3_16_33_4"             "_6_3_3_16_33;_4"                  13      13     0

# 397 "_16_33_6"                   "_16_33_6"                         11      11     0

# 315 "_6_3_4_3_6_3_4_3"             "_6_3;_4_3_6_3_4_3"                     1      11     2
# 316 "_6_3_4_3_6_3_4_3"             "_6_3_4_3;_6_3_4_3"                    10      11     2

# 412 "_4_6_3_4_5"                 "_4;_6_3;_4;_5"                     1      10     2
# 413 "_4_6_3_4_5"                 "_4;_6_3_4_5"                       9      10     2

# 461 "_1_6_3_4_5"                 "_1;_6_3;_4;_5"                     7       7     0
# 466 "_3_6_3"                     "_3;_6_3"                           7       7     0

# 474 "_4_6_3_3_4_6"               "_4;_6_3_3;_4_6"                    7       7     0

# 478 "_6_3_4_10"                  "_6_3;_4_10"                        1       7     2
# 479 "_6_3_4_10"                  "_6_3_4_10"                         6       7     2

# 481 "_6_5_16_33_3_3"             "_6_5;_16_33_3_3"                   1       7     2
# 486 "_16_33_6_5"                 "_16_33_6_5"                        6       6     0


# 494 "_2_4_6_3_3_4_6"             "_2;_4;_6_3_3;_4_6"                 6       6     0
# 505 "_6_3_3_3_16_33"             "_6_3_3_3_16_33"                    6       6     0
# 506 "_6_3_4_3_6_5"               "_6_3_4_3_6_5"                      6       6     0
# 514 "_16_33_4_16_33"             "_16_33;_4;_16_33"                  5       5     0
# 516 "_1_16_16"                   "_1_16_16"                          1       5     2
# 517 "_1_6_3_2_4"                 "_1;_6_3_2;_4"                      4       5     2
# 518 "_1_6_3_2_4"                 "_1_6_3_2;_4"                       1       5     2
# 520 "_1_6_5_3_4"                 "_1;_6;_5_3;_4"                     1       5     2
# 523 "_2_4_2_10_4"                "_2;_4;_2_10;_4"                    2       5     2
# 525 "_2_4_6_3_4_16_33_3"         "_2;_4;_6_3;_4;_16_33_3"            1       5     2
# 530 "_3_3_4_6"                   "_3_3;_4;_6"                        5       5     0
# 533 "_4_5_16_33_4"               "_4;_5;_16_33;_4"                   3       5     2
# 534 "_4_5_16_33_4"               "_4_5;_16_33;_4"                    2       5     2
# 535 "_4_6_3_3_16_33"             "_4;_6_3_3_16_33"                   5       5     0
# 537 "_6_16_16_33_33_4_3_3"       "_6_16_16_33_33;_4_3_3"             3       5     2
# 541 "_6_3_10_5"                  "_6_3_10;_5"                        4       5     2
# 542 "_6_3_10_5"                  "_6_3_10_5"                         1       5     2
# 543 "_6_3_3_2_4"                 "_6_3_3_2;_4"                       5       5     0
# 546 "_6_3_3_4_16_33"             "_6_3_3;_4_16_33"                   1       5     2
# 548 "_6_5_2_4"                   "_6_5_2;_4"                         5       5     0
# 558 "_16_33_1"                   "_16_33_1"                          4       4     0
# 564 "_1_6_16_16_33_4_33_3"       "_1_6_16_16_33_4_33_3"              1       4     2
# 566 "_1_6_3_16_33_4"             "_1;_6_3_16_33;_4"                  4       4     0
# 569 "_1_6_3_6_5"                 "_1;_6_3_6_5"                       4       4     0
# 574 "_2_4_5_13"                  "_2;_4;_5;_13"                      3       4     2
# 575 "_2_4_5_13"                  "_2;_4_5;_13"                       1       4     2
# 576 "_2_4_6_3_3_16_33"           "_2;_4;_6_3_3_16_33"                4       4     0
# 583 "_4_1_6_3_4_3_4"             "_4_1;_6_3_4_3;_4"                  1       4     2
# 592 "_4_5_13"                    "_4;_5;_13"                         2       4     2
# 593 "_4_5_13"                    "_4_5;_13"                          2       4     2
# 597 "_4_6_3_3_2"                 "_4;_6_3_3_2"                       4       4     0
# 600 "_5_16_33_16_33"             "_5_16_33_16_33"                    1       4     2
# 607 "_6_16_33_16_33_4"           "_6_16_33_16_33;_4"                 4       4     0
# 610 "_6_16_33_4_16_33_3_3"       "_6_16_33;_4;_16_33_3_3"            4       4     0
# 611 "_6_16_33_6_5"               "_6_16_33_6_5"                      4       4     0
# 615 "_6_3_4_3_2_4"               "_6_3_4_3_2;_4"                     4       4     0
# 618 "_6_3_4_3_4_2_4"             "_6_3_4_3;_4_2;_4"                  1       4     2
# 620 "_10_4_5"                    "_10_4_5"                           2       3     2
# 623 "_12_33_4"                   "_12_33;_4"                         3       3     0
# 626 "_16_33_2_4"                 "_16_33_2;_4"                       3       3     0
# 627 "_16_33_4_16_33_4"           "_16_33;_4;_16_33_4"                3       3     0
# 635 "_1_6_10_5"                  "_1;_6_10;_5"                       2       3     2
# 636 "_1_6_16_33"                 "_1;_6_16_33"                       3       3     0
# 639 "_2_10_4_6"                  "_2_10;_4;_6"                       1       3     2
# 650 "_2_4_6_3_4_3_4_6"           "_2;_4;_6_3_4_3;_4_6"               3       3     0
# 652 "_3_4_3_3_6"                 "_3_4_3_3;_6"                       3       3     0
# 669 "_4_6_3_4_3_4_6"             "_4;_6_3_4_3;_4_6"                  3       3     0
# 675 "_5_6_3_4_5"                 "_5;_6_3_4_5"                       3       3     0
# 676 "_6_16_16_16_33_33_33_4_3_3" "_6_16_16_16_33_33_33;_4_3_3"       1       3     2
# 689 "_6_3_3_6_5_3"               "_6_3_3_6_5_3"                      3       3     0
# 692 "_6_3_4_3_4_6_3_3_4"         "_6_3_4_3;_4;_6_3_3_4"              3       3     0
# 693 "_6_3_6_5"                   "_6_3_6_5"                          3       3     0
# 694 "_6_5_13"                    "_6_5_13"                           3       3     0
# 700 "_10_5_10"                   "_10_5;_10"                         1       2     2
# 708 "_16_33_2"                   "_16_33_2"                          2       2     0
# 725 "_1_6_3_16_16_33_33"         "_1;_6_3_16_16_33_33"               2       2     0
# 729 "_2_10_4_5"                  "_2;_10_4_5"                        2       2     0
# 738 "_2_4_33_16"                 "_2;_4_33_16"                       2       2     0
# 739 "_2_4_5_2"                   "_2;_4;_5;_2"                       1       2     2
# 740 "_2_4_5_2"                   "_2;_4_5;;_2"                       1       2     2
# 746 "_2_4_6_16_16_33_33"         "_2;_4;_6_16_16_33_33"              2       2     0
# 748 "_2_4_6_16_33_6_3_4_3"       "_2;_4;_6_16_33;_6_3_4_3"           2       2     0
# 749 "_2_4_6_1_3"                 "_2;_4;_6;_1;_3"                    2       2     0
# 750 "_2_4_6_3_10"                "_2;_4;_6_3_10"                     2       2     0
# 751 "_2_4_6_3_3_2"               "_2;_4;_6_3_3_2"                    2       2     0
# 756 "_2_4_6_3_4_3_16_33"         "_2;_4;_6_3_4_3_16_33"              2       2     0
# 758 "_2_4_6_3_4_5"               "_2;_4;_6_3_4_5"                    2       2     0
# 761 "_3_3_3_6"                   "_3_3_3;_6"                         2       2     0
# 762 "_3_4_3_6_3"                 "_3_4_3;_6_3"                       2       2     0
# 763 "_3_4_6_3"                   "_3_4_6_3"                          2       2     0
# 783 "_4_3_3_6"                   "_4;_3_3;_6"                        2       2     0
# 788 "_4_6_16_16_33_33"           "_4;_6_16_16_33_33"                 2       2     0
# 789 "_4_6_16_16_33_4_33"         "_4;_6_16_16_33;_4_33"              1       2     2
# 791 "_4_6_3_16_33"               "_4;_6_3_16_33"                     2       2     0
# 803 "_4_6_5_5"                   "_4;_6;_5;_5"                       1       2     2
# 804 "_4_6_5_5"                   "_4;_6_5;_5"                        1       2     2
# 809 "_5_16_16_33_4_33"           "_5;_16_16_33;_4_33"                1       2     2
# 817 "_5_6_16_16_33_33"           "_5;_6_16_16_33_33"                 2       2     0
# 819 "_5_6_16_33_4_3_3"           "_5;_6_16_33;_4_3_3"                1       2     2
# 822 "_5_6_3_3_1"                 "_5;_6_3_3_1"                       2       2     0
# 826 "_6_13"                      "_6_13"                             2       2     0
# 827 "_6_16_16"                   "_6_16_16"                          2       2     0
# 835 "_6_16_33_4_3_3_4_3"         "_6_16_33;_4_3_3_4_3"               1       2     2
# 840 "_6_2_4_5"                   "_6;_2;_4_5"                        2       2     0
# 842 "_6_3_10_3_4"                "_6_3;_10_3;_4"                     2       2     0
# 846 "_6_3_3_16_16_33_33_4"       "_6_3_3_16_16_33_33;_4"             2       2     0
# 847 "_6_3_3_16_33_16_33"         "_6_3_3_16_33_16_33"                2       2     0
# 848 "_6_3_3_3_2"                 "_6_3_3_3_2"                        2       2     0
# 854 "_6_3_3_4_6_3_4_5"           "_6_3_3;_4;_6_3_4_5"                2       2     0
# 861 "_6_3_4_3_4_10"              "_6_3_4_3;_4_10"                    2       2     0
# 864 "_6_3_5_10"                  "_6_3_5;_10"                        2       2     0
# 866 "_6_5_10"                    "_6_5_10"                           2       2     0
# 873 "_6_6_3_3_3_4_3"             "_6;_6_3_3_3_4_3"                   2       2     0
# 874 "_6_6_3_3_3_4_3_4"           "_6;_6_3_3_3_4_3;_4"                2       2     0
# 878 "_6_6_6_3_4_3"               "_6;_6_6_3_4_3"                     1       2     2
# 881 "_10_2_4_5"                  "_10;_2;_4_5;"                      1       1     0
# 898 "_12_3"                      "_12_3"                             1       1     0
# 899 "_12_33_4_16"                "_12_33;_4;_16"                     1       1     0
# 900 "_12_3_3"                    "_12_3_3"                           1       1     0
#  1 _12_3_4_3_4_2                      _12_3_4_3;_4;_2                         1     1     0
#  9 _16_16_2_4_33_33                   _16_16;_2;_4_33_33                      1     1     0
# 12 _16_16_33_4_33_2                   _16_16_33_4_33_2                        1     1     0
# 13 _16_16_33_4_3_4_3_4_16             _16_16_33_4_3_4_3;_4;_16                1     1     0
# 15 _16_33_3_4_6_3_3                   _16_33_3_4_6_3_3                        1     1     0
# 19 _16_33_6_5_3_4_3_4                 _16_33_6_5_3_4_3;_4                     1     1     0
# 26 _1_13_13                           _1_13_13                                1     1     0
# 35 _1_16_33_4_16_33                   _1;_16_33;_4;_16_33                     1     1     0
# 41 _1_3_4_6                           _1;_3;_4;_6                             1     1     0
# 51 _1_6_16_33_5                       _1;_6_16_33;_5                          1     1     0
# 56 _1_6_3_2_4_6                       _1;_6_3_2;_4;_6                         1     1     0
# 58 _1_6_3_4_10                        _1;_6_3;_4_10                           1     1     0
# 60 _1_6_3_4_1_6_3                     _1;_6_3;_4;_1_6_3                       1     1     0
# 62 _1_6_3_4_3_1                       _1;_6_3_4_3_1                           1     1     0
# 64 _1_6_3_4_6_3_3_2                   _1;_6_3;_4;_6_3_3_2                     1     1     0
# 65 _1_6_3_4_6_3_3_4                   _1;_6_3;_4;_6_3_3_4                     1     1     0
# 66 _1_6_3_4_6_3_4_3_4                 _1;_6_3;_4;_6_3_4_3_4                   1     1     0
# 70 _1_6_3_6_16_33_3                   _1;_6_3_6_16_33_3                       1     1     0
# 71 _1_6_3_6_2_4                       _1;_6_3_6;_2;_4                         1     1     0
# 73 _1_6_3_6_3_5                       _1;_6_3_6_3;_5                          1     1     0
# 74 _1_6_3_6_5_3                       _1;_6_3_6;_5_3                          1     1     0
# 78 _2_10_4_16_33_6_5_3_3              _2;_10_4;_16_33_6_5_3_3                 1     1     0
# 88 _2_4_16_33_6                       _2;_4;_16_33_6                          1     1     0
# 89 _2_4_16_33_6_5                     _2;_4;_16_33_6_5                        1     1     0
# 90 _2_4_16_33_6_5_16_33               _2;_4;_16_33_6_5;_16_33                 1     1     0
# 91 _2_4_1_1                           _2;_4;_1_1                              1     1     0
# 98 _2_4_1_6_3_16_33_4                 _2;_4;_1;_6_3_16_33;_4                  1     1     0
# 99 _2_4_1_6_3_2                       _2;_4;_1;_6_3_2                         1     1     0
# 109 _2_4_2_4_5_16_33                   _2;_4;_2;_4_5;_16_33                    1     1     0
# 110 _2_4_2_4_5_5                       _2;_4;_2;_4_5;_5                        1     1     0
# 112 _2_4_2_4_6_16_33                   _2;_4;_2;_4;_6_16_33                    1     1     0
# 113 _2_4_2_4_6_3_16_33_4_3             _2;_4;_2;_4;_6_3_16_33;_4_3             1     1     0
# 114 _2_4_2_4_6_3_4_3_16_33_4           _2;_4;_2;_4;_6_3_4_3_16_33;_4           1     1     0
# 115 _2_4_3_3_6                         _2;_4_3_3;_6                            1     1     0
# 116 _2_4_3_4_3_6                       _2;_4_3_4_3;_6                          1     1     0
# 117 _2_4_3_6_3                         _2;_4_3;_6_3                            1     1     0
# 127 _2_4_6_16_16_33_33_4_3_3           _2;_4;_6_16_16_33_33;_4_3_3             1     1     0
# 128 _2_4_6_16_16_33_4_33               _2;_4;_6_16_16_33_4_33                  1     1     0
# 129 _2_4_6_16_16_33_4_33_3_4_3         _2;_4;_6_16_16_33;_4_33_3_4_3           1     1     0
# 131 _2_4_6_16_33_3_3_4_6_16_33_3       _2;_4;_6_16_33_3_3;_4_6_16_33_3         1     1     0
# 136 _2_4_6_3_10_5                      _2;_4;_6_3_10;_5                        1     1     0
# 140 _2_4_6_3_16_33_4_3_4_6             _2;_4;_6_3_16_33_4_3;_4_6               1     1     0
# 142 _2_4_6_3_3_1                       _2;_4;_6_3_3_1                          1     1     0
# 143 _2_4_6_3_3_2_4                     _2;_4;_6_3_3_2;_4                       1     1     0
# 146 _2_4_6_3_3_6_16_33_16_33_4         _2;_4;_6_3_3_6_16_33_16_33;_4           1     1     0
# 148 _2_4_6_3_3_6_5_3                   _2;_4;_6_3_3_6_5_3                      1     1     0
# 150 _2_4_6_3_4_3_2                     _2;_4;_6_3_4_3_2                        1     1     0
# 155 _2_4_6_3_4_5_6_5                   _2;_4;_6_3_4_5;_6_5                     1     1     0
# 171 _33_16_6_3_4_3_3                   _33_16_6_3_4_3_3                        1     1     0
# 173 _33_4_16                           _33;_4;_16                              1     1     0
# 178 _3_3_2_4_6                         _3_3_2;_4;_6                            1     1     0
# 180 _3_3_3_4_6                         _3_3_3;_4;_6                            1     1     0
# 189 _3_6                               _3;_6                                   1     1     0
# 190 _3_6_3_4                           _3;_6_3;_4                              1     1     0
# 196 _4_10_6_16_33                      _4;_10;_6_16_33                         1     1     0
# 197 _4_16_13_33                        _4_16_13_33                             1     1     0
# 199 _4_16_33_10_16_33                  _4;_16_33;_10_16_33                     1     1     0
# 201 _4_16_33_3_3_6                     _4;_16_33_3_3;_6                        1     1     0
# 207 _4_16_33_6_3_3_4_13                _4;_16_33_6_3_3;_4;_13                  1     1     0
# 208 _4_16_33_6_5                       _4;_16_33_6_5                           1     1     0
# 224 _4_1_6_3_16_33                     _4;_1;_6_3_16_33                        1     1     0
# 241 _4_2_4_2_4_6_16_33                 _4;_2;_4;_2;_4;_6_16_33                 1     1     0
# 243 _4_2_4_2_4_6_16_33_4               _4;_2;_4;_2;_4;_6_16_33;_4              1     1     0
# 270 _4_3_3_4_6                         _4;_3_3;_4;_6                           1     1     0
# 272 _4_3_6_3_4                         _4;_3;_6_3;_4                           1     1     0
# 293 _4_6_16_33_2                       _4;_6_16_33_2                           1     1     0
# 296 _4_6_16_33_4_16_33_4_16_33_3_3     _4;_6_16_33;_4;_16_33_4_16_33_3_3       1     1     0
# 297 _4_6_16_33_4_3_3_16_33             _4;_6_16_33_4_3_3_16_33                 1     1     0
# 299 _4_6_3_16_16_33_4_33_4_3           _4;_6_3_16_16_33_4_33;_4_3              1     1     0
# 306 _4_6_3_3_6_3_3_3                   _4;_6_3_3_6_3_3_3                       1     1     0
# 307 _4_6_3_4_10                        _4;_6_3_4_10                            1     1     0
# 309 _4_6_3_4_3_1                       _4;_6_3_4_3_1                           1     1     0
# 310 _4_6_3_4_3_16_33                   _4;_6_3_4_3_16_33                       1     1     0
# 311 _4_6_3_4_3_2                       _4;_6_3_4_3_2                           1     1     0
# 312 _4_6_3_4_3_2_4_1                   _4;_6_3_4_3_2;_4;_1                     1     1     0
# 318 _4_6_3_5_16_33                     _4;_6_3_5;_16_33                        1     1     0
# 322 _4_6_5_2                           _4;_6_5_2                               1     1     0
# 325 _4_6_6_3_16_33                     _4;_6;_6_3_16_33                        1     1     0
# 326 _4_6_6_5_3_4_3                     _4;_6;_6;_5_3_4_3                       1     1     0
# 327 _4_6_6_6_5                         _4;_6;_6_6_5                            1     1     0
# 336 _5_16_33_4_16_33                   _5;_16_33;_4;_16_33                     1     1     0
# 339 _5_16_33_6                         _5;_16_33_6                             1     1     0
# 340 _5_16_33_6_5                       _5;_16_33_6_5                           1     1     0
# 343 _5_1_6_3_16_33                     _5;_1;_6_3_16_33                        1     1     0
# 351 _5_3_4_6_3                         _5_3;_4;_6_3                            1     1     0
# 352 _5_4_2                             _5_4;_2                                 1     1     0
# 358 _5_5_5_2                           _5;_5_5;_2                              1     1     0
# 363 _5_6_16_16                         _5;_6_16_16                             1     1     0
# 364 _5_6_16_16_16_16                   _5;_6_16_16_16_16                       1     1     0
# 365 _5_6_16_16_33_33_4_3_4_3           _5;_6_16_16_33_33;_4_3_4_3              1     1     0
# 366 _5_6_16_16_33_4_33                 _5;_6_16_16_33_4_33                     1     1     0
# 371 _5_6_3_3_2_4                       _5;_6_3_3_2;_4                          1     1     0
# 374 _5_6_3_4_3_16_33                   _5;_6_3_4_3_16_33                       1     1     0
# 383 _6_16_16_16_16                     _6_16_16_16_16                          1     1     0
# 384 _6_16_16_16_16_33_33_33_33         _6_16_16_16_16_33_33_33_33              1     1     0
# 386 _6_16_16_16_16_33_4_33_4_33_33     _6_16_16_16_16_33_4_33_4_33_33          1     1     0
# 387 _6_16_16_16_16_3_3_3               _6_16_16_16_16_3_3_3                    1     1     0
# 388 _6_16_16_16_33_33_33               _6_16_16_16_33_33_33                    1     1     0
# 391 _6_16_16_33_16_16_33_33_33         _6_16_16_33_16_16_33_33_33              1     1     0
# 397 _6_16_16_33_33_3_4_6_3_3_3         _6_16_16_33_33_3_4_6_3_3_3              1     1     0
# 399 _6_16_16_33_33_4_16_33_3_3         _6_16_16_33_33;_4;_16_33_3_3            1     1     0
# 400 _6_16_16_33_33_4_16_33_3_4_3       _6_16_16_33_33;_4;_16_33_3_4_3          1     1     0
# 401 _6_16_16_33_33_4_3_3_3             _6_16_16_33_33;_4_3_3_3                 1     1     0
# 405 _6_16_16_33_4_33_3_3_3             _6_16_16_33;_4_33_3_3_3                 1     1     0
# 412 _6_16_33_2                         _6_16_33_2                              1     1     0
# 424 _6_16_33_4_16_33_3_4_3             _6_16_33;_4;_16_33_3_4_3                1     1     0
# 427 _6_16_33_4_3_16_33_3               _6_16_33;_4_3_16_33_3                   1     1     0
# 428 _6_16_33_4_3_3_4_16_33             _6_16_33_4_3_3;_4;_16_33                1     1     0
# 433 _6_16_33_6                         _6_16_33_6                              1     1     0
# 440 _6_16_6_5_33_3_3                   _6_16_6_5_33_3_3                        1     1     0
# 442 _6_1_16_33_3                       _6;_1;_16_33_3                          1     1     0
# 443 _6_1_3_4                           _6;_1;_3;_4                             1     1     0
# 446 _6_2_4_3_3                         _6;_2;_4_3_3                            1     1     0
# 447 _6_2_4_3_4_3                       _6;_2;_4_3_4_3                          1     1     0
# 449 _6_3_16_33                         _6_3_16_33                              1     1     0
# 453 _6_3_16_33_4_16_33_3               _6_3_16_33;_4;_16_33_3                  1     1     0
# 456 _6_3_16_33_4_3_4                   _6_3_16_33;_4_3;_4                      1     1     0
# 458 _6_3_3_16_16                       _6_3_3_16_16                            1     1     0
# 459 _6_3_3_16_16_33_33                 _6_3_3_16_16_33_33                      1     1     0
# 460 _6_3_3_1_5_2_4_5                   _6_3_3_1;_5;_2;_4;_5                    1     1     0
# 462 _6_3_3_3_4_6_16_33                 _6_3_3_3;_4;_6_16_33                    1     1     0
# 463 _6_3_3_3_4_6_3_3_4                 _6_3_3_3;_4;_6_3_3_4                    1     1     0
# 470 _6_3_3_3_6_5                       _6_3_3_3_6_5                            1     1     0
# 486 _6_3_3_4_6_3_3_4                   _6_3_3;_4;_6_3_3_4                      1     1     0
# 488 _6_3_3_4_6_3_4_3_4_6_3_4_3         _6_3_3;_4;_6_3_4_3_4_6_3_4_3            1     1     0
# 489 _6_3_3_5_10                        _6_3_3_5;_10                            1     1     0
# 493 _6_3_3_6_1                         _6_3_3_6;_1                             1     1     0
# 494 _6_3_3_6_16_33_3                   _6_3_3_6_16_33_3                        1     1     0
# 495 _6_3_3_6_16_33_3_3                 _6_3_3_6_16_33_3_3                      1     1     0
# 496 _6_3_3_6_3_3_2                     _6_3_3_6_3_3_2                          1     1     0
# 497 _6_3_3_6_5_16_16_33_33             _6_3_3_6_5;_16_16_33_33                 1     1     0
# 498 _6_3_3_6_5_3_4                     _6_3_3_6_5_3;_4                         1     1     0
# 499 _6_3_4_10_5                        _6_3_4_10;_5                            1     1     0
# 502 _6_3_4_16_33_16_33_4_3             _6_3_4_16_33_16_33;_4_3                 1     1     0
# 505 _6_3_4_16_33_4_10_3_4              _6_3_4_16_33;_4;_10_3;_4                1     1     0
# 509 _6_3_4_3_16_16_33_33               _6_3_4_3_16_16_33_33                    1     1     0
# 510 _6_3_4_3_16_33_4_3                 _6_3_4_3_16_33;_4_3                     1     1     0
# 511 _6_3_4_3_1_1_1_2_4_2_4             _6_3_4_3_1;_1;_1;_2;_4;_2;_4            1     1     0
# 512 _6_3_4_3_2_4_2                     _6_3_4_3_2;_4;_2                        1     1     0
# 514 _6_3_4_3_3_2_4                     _6_3_4_3_3_2;_4                         1     1     0
# 528 _6_3_4_3_4_6_3_4_3_4_6_3_3         _6_3_4_3;_4;_6_3_4_3_4_6_3_3            1     1     0
# 533 _6_3_4_3_6_5_3                     _6_3_4_3_6_5_3                          1     1     0
# 534 _6_3_4_3_6_6_5_16_16_16_33_33_4_33 _6_3_4_3_6;_6_5_16_16_16_33_33_4_33     1     1     0
# 535 _6_3_4_5_10                        _6_3;_4_5;_10                           1     1     0
# 536 _6_3_4_5_16_33                     _6_3_4_5;_16_33                         1     1     0
# 539 _6_3_5_3                           _6_3;_5_3                               1     1     0
# 545 _6_5_16_33_3_3_4                   _6_5_16_33_3_3;_4                       1     1     0
# 551 _6_5_16_3_4_3_4_33_4               _6_5_16_3_4_3_4_33;_4                   1     1     0
# 554 _6_5_2_4_13                        _6_5_2;_4;_13                           1     1     0
# 555 _6_5_3_3_16_33                     _6_5_3_3_16_33                          1     1     0
# 571 _6_6_16_33                         _6;_6_16_33                             1     1     0
# 577 _6_6_3_4_3_3_3                     _6;_6_3_4_3_3_3                         1     1     0
# 578 _6_6_3_4_3_4_3_3_4                 _6;_6_3_4_3_4_3_3;_4                    1     1     0
# 581 _6_6_3_6_5                         _6;_6_3_6_5                             1     1     0
# 582 _6_6_3_6_5_3_3                     _6;_6_3_6_5_3_3                         1     1     0
# 584 _6_6_5_3_3_3_3                     _6;_6_5_3_3_3_3                         1     1     0
# 585 _6_6_5_3_3_3_4_3                   _6;_6_5_3_3_3_4_3                       1     1     0
# 586 _6_6_6_5                           _6;_6_6_5                               1     1     0
















cod = stringr::str_replace_all(cod, "(_18\\(.\\))(\\[1\\])", "\\1[0]"),
cod = stringr::str_replace_all(cod, "(_6\\(18.{1}\\))(\\[1\\])", "\\1[0]"))

  
x <- c(108, 109, 110, 117, 119, 120, 123, 124, 128, 129, 135, 151, 154, 160, 167, 186, 193, 194)  
xdf1 %>% dplyr::group_split(modcod) %>% '['(x)

  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_10") & np_val > 1, stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_10") & np_val > 1, stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])((_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_10_4"), stringr::str_replace(cod, "(_10\\(.+_4\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_10_4"), stringr::str_replace(cod, "(_10\\(.+_4\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_10_4"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])(\\;)(_4)", "\\1\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_10"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[1\\])(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_5_10"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[1\\])((_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_10\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_5"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[1\\])(_5\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_13"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_6"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_16"), stringr::str_replace(cod, "(_5\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_12_10"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_12_16"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_12_(3|33|3_3|3_4_3_4_2)$"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])", "\\1;"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_12_33(_4|_4_16)$"), stringr::str_remove_all(cod, "\\;"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_12_33(_4|_4_16)$"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])", "\\1;"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_12_5"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])(_5\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_12_5"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])((_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_5\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_12_6"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_12_6"), stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])((_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_6"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_6_6"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])((_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_16_16_13$"), stringr::str_replace(cod, "(_16\\(.{1,3}\\)\\[0\\])(_)", "\\1;2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_10"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_10)", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_33_10"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])((_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_10)", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_33_13$"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_[0-9]+)", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_10_5"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])(_5\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_10_5"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])([0-9]+\\(.{1,3}\\)\\[0\\])+(_5\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_16_33_3_4_6_3_3$|^_16_6_16$|^_16_6_33_3_16_33_3$|^_16_6_33_3"), stringr::str_replace(cod, "(_6\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_16_33_4_16(_33|_33_4)$"), stringr::str_remove_all(cod, "\\;"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_16_33_4_16(_33|_33_4)$"), stringr::str_replace(cod, "(_4\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_16_33_4_16"), stringr::str_replace(cod, "(\\;)(_4\\(.{1,3}\\)\\[1\\])(\\;)", "\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "_16_33_4_16"), stringr::str_replace(cod, "(_4\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_16_33(_6|_6_3)"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_[0-9]+\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_16_5_33(_6)"), stringr::str_replace(cod, "(_33\\(.{1,3}\\)\\[0\\])(_[0-9]+\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_16_3_6_3_4_3$"), stringr::str_replace(cod, "(_3\\(.{1,3}\\)\\[0\\])(_[0-9]+\\(.{1,3}\\)\\[.\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_3_6_16_3"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_1_3_4_6$"), stringr::str_remove_all(cod, "\\;"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_1_3_4_6$"), stringr::str_replace(cod, "(_1\\(.{1,3}\\)\\[1\\])", "\\1;"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_1_6(_10|_10_5)$"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])", ";\\1"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_1_6_10_5$"), stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])(\\;)", "\\1"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_16_16_16_16_16_33_3|_6_16_16_33_33_3|_6_16_16_33_4_33_3|_6_16_16_33_4_33_4_3|_6_16_33_16_33_3|_6_16_33_3"), stringr::str_replace(cod, "(_16\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_6_16_33$|_6_16_33_4$"), stringr::str_replace(cod, "(_6\\(.{1,3}\\)\\[1\\])(_16\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod)) %>% 
  change_cod12() %>% 
  change_cod13() %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_1_13_13$"), stringr::str_replace(cod, "(_1\\(.{1,3}\\)\\[.\\])", "\\1;"), cod))
  
  dplyr::mutate(x = stringr::str_split(cod, ";")) %>% 
  tidyr::unnest(x) %>%
  dplyr::mutate(cod = ifelse(x != cod, x, cod)) %>% 
  dplyr::select(-x) %>% 
  dplyr::mutate(cod = stringr::str_remove_all(cod, "\\[.\\]")) %>% 
  dplyr::mutate(cod = stringr::str_replace_all(cod, '\\)', ')[1]')) %>% 
  dplyr::mutate(np = stringr::str_count(cod, "_")) %>% 
  change_cod() %>% 
  simplify_cod() %>% 
  put_ind_column() %>%
  change_cod_free_throws() %>% 
  change_cod_rebounds() %>% 
  change_cod_turnovers() %>%
  change_cod12() %>% 
  change_cod13() %>% 
  dplyr::mutate(cod = ifelse(modcod == '_4_5', stringr::str_replace(cod, "(_5\\(37t\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_10_4"), stringr::str_replace(cod, "(_10\\(.+_4\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_10_4"), stringr::str_replace(cod, "(_10\\(.+_4\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod == "_10_10_4", stringr::str_replace(cod, "(_10\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  put_useful_columns() %>% 
  dplyr::mutate(ind11 = stringr::str_detect(val, "11"),
                ind11 = as.integer(ind11)) %>%
  dplyr::mutate(cod = stringr::str_replace(cod, "(_18\\(.\\))(\\[0\\])(_13\\(.\\))(\\[1\\])", "\\1\\4\\3\\2"))

xdf2 <- xdf1 %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_10") & np_val > 1, stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_10") & np_val > 1, stringr::str_replace(cod, "(_10\\(.{1,3}\\)\\[1\\])((_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_12") & np_val > 1, stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_12") & stringr::str_detect(modcod, "^_12_16", negate = TRUE) & np_val > 1, stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])((_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(modcod, "^_12") & stringr::str_detect(modcod, "^_12_16") & np_val > 1, stringr::str_replace(cod, "(_12\\(.{1,3}\\)\\[1\\])((_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "^_6_6") & np_val > 1, stringr::str_replace(cod, "^(_6\\(.{1,3}\\)\\[1\\])", "\\1;"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_5") & np_val > 1, stringr::str_replace(cod, "^(_5\\(.{1,3}\\)\\[1\\])", "\\1;"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_5_10") & np_val > 1, stringr::str_replace(cod, "^(_5\\(.{1,3}\\)\\[1\\])", "\\1;"), cod)) %>% 
  dplyr::mutate(x = stringr::str_split(cod, ";")) %>% 
  tidyr::unnest(x) %>%
  dplyr::mutate(cod = ifelse(x != cod, x, cod)) %>% 
  dplyr::select(-x) %>% 
  dplyr::mutate(cod = stringr::str_remove_all(cod, "\\[.\\]")) %>% 
  dplyr::mutate(cod = stringr::str_replace_all(cod, '\\)', ')[1]')) %>% 
  dplyr::mutate(np = stringr::str_count(cod, "_")) %>% 
  change_cod() %>% 
  simplify_cod() %>% 
  put_ind_column() %>%
  change_cod_free_throws() %>% 
  change_cod_rebounds() %>% 
  change_cod_turnovers() %>%
  change_cod12() %>% 
  change_cod13() %>% 
  dplyr::mutate(cod = ifelse(modcod == '_4_5', stringr::str_replace(cod, "(_5\\(37t\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_10_4"), stringr::str_replace(cod, "(_10\\(.+_4\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(modcod, "_10_4"), stringr::str_replace(cod, "(_10\\(.+_4\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(modcod == "_10_10_4", stringr::str_replace(cod, "(_10\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  put_useful_columns() %>% 
  dplyr::mutate(ind11 = stringr::str_detect(val, "11"),
                ind11 = as.integer(ind11)) %>%
  dplyr::mutate(cod = stringr::str_replace(cod, "(_18\\(.\\))(\\[0\\])(_13\\(.\\))(\\[1\\])", "\\1\\4\\3\\2"))
  


xdf2 %>% 
  dplyr::arrange(modcod, cod) %>% 
  dplyr::mutate(done = ifelse(np_val <= 1, 1, 0)) %>% 
  dplyr::filter(done == 0) %>% 
  dplyr::group_by(modcod, np_val) %>% 
  dplyr::summarise(n = sum(n), .groups = 'drop_last') %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(np_val = paste0("npval", np_val)) %>%
  tidyr::spread(np_val, cnt) %>% 
  print(., n = 900)

xdf1 %>% 
  dplyr::arrange(modcod, cod) %>% 
  dplyr::mutate(done = ifelse(np_val <= 1, 1, 0)) %>% 
  dplyr::filter(done == 0) %>% 
  dplyr::group_by(modcod, np_val) %>% 
  dplyr::summarise(n = sum(n), .groups = 'drop_last') %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(np_val = paste0("npval", np_val)) %>%
  tidyr::spread(np_val, cnt) %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 900)


xdf1 %>% 
  dplyr::arrange(modcod, cod) %>% 
  dplyr::mutate(done = ifelse(np_val <= 1, 1, 0)) %>% 
  dplyr::filter(done == 0) %>% 
  dplyr::group_by(ind, modcod, np_val) %>% 
  dplyr::summarise(n = sum(n), .groups = 'drop_last') %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(np_val = paste0("npval", np_val)) %>%
  tidyr::spread(np_val, cnt) %>% 
  print(., n = 900)

xdf1 %>% 
  dplyr::arrange(modcod, cod) %>% 
  dplyr::mutate(done = ifelse(np_val <= 1, 1, 0)) %>% 
  dplyr::filter(done == 0) %>% 
  dplyr::mutate(first_action = stringr::str_extract(modcod, "_[0-9]+")) %>% 
  dplyr::group_by(first_action) %>% 
  dplyr::summarise(cnt = sum(n), .groups = 'drop')


xdf1 %>% 
  dplyr::filter(modcod == '_4_13')

xdf %>% 
  dplyr::filter(np > 1) %>% 
  dplyr::distinct(modcod)
 

# erros cod2
## 


# cod = ifelse(stringr::str_detect(cod2, "(_3|_33)_4(_3|_33)"), stringr::str_replace_all(cod, "(_[3|33]\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])(_[3|33]\\(.{1,3}\\)\\[0\\])", "\\1\\2[0]\\4"), cod),

xdf1 <- xdf %>% 
  dplyr::mutate(erro = ifelse(stringr::str_detect(cod2, "^_3_3_4|^_3_6_3|^_3_13|_13_1|_6_1_3|_13_2_4|_6_1_3|_6_4_3_3|_6_1_16_33_3|_6_1_3_4|_16_16_2_4_33_33|_13_16_33_4|_6_6_3_3_3_3|_6_6_3_3_3_4_3|_6_6_3_3_3_4_3_4|_6_6_3_4_3_3_3|_6_6_3_4_3_4_3_3_4|_6_6_5_3_3_3_3|_6_6_5_3_3_3_4_3"), 1, 0)) %>% 
  dplyr::filter(erro == 0) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod2, '_5_6'), stringr::str_replace(cod, '(_5\\(.{1,3}\\)\\[1\\])(_6\\(.{1,3}\\)\\[1\\])', "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(cod2, '_5_6'), stringr::str_replace(cod, '(_5\\(.{1,3}\\)\\[1\\])((_[0-9]+\\(.{1,3}\\)\\[0\\])+)(_6\\(.{1,3}\\)\\[1\\])', "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod2, "_10_4"), stringr::str_replace(cod, "(_10\\(.+_4\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod2, "_10_4"), stringr::str_replace(cod, "(_10\\(.+_4\\(.{1,3}\\))(\\[1\\])", "\\1[0]"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod2, "_10_(6|16)"), stringr::str_replace(cod, '(_10\\(.\\)\\[1\\])', "\\1;"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod, "_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\]") & stringr::str_detect(cod, "(\\[1\\])(_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\])"), stringr::str_replace_all(cod, "(\\[1\\])(_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(cod, "_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\]") & stringr::str_detect(cod, "(_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])"), stringr::str_replace_all(cod, "(_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(cod, "_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\]") & stringr::str_detect(cod, "(\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[0\\])+(_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\])") & np_val > 1, stringr::str_replace_all(cod, "(_[0-9]+\\(.{1,3}\\)\\[0\\])(_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\])", "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(cod, "_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\]") & stringr::str_detect(cod, "(_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\])(_[0-9]+\\(.{1,3}\\)\\[0\\])+(_[0-9]+\\(.{1,3}\\)\\[1\\])"), stringr::str_replace_all(cod, "(_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\])(_[0-9]+\\(.{1,3}\\)\\[0\\])+(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1\\2;\\3"), cod),
                cod = ifelse(stringr::str_detect(cod, "_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\]"), stringr::str_replace_all(cod, "(_[0-9]+\\(.{1,3}\\)\\[0\\])+(\\;)(_16)", "\\2\\1\\3"), cod),
                cod = ifelse(stringr::str_detect(cod, "_6\\(13a\\)\\[1\\]\\;_5\\(13p\\)\\[0\\]"), stringr::str_replace_all(cod, "(_6\\(13a\\)\\[1\\])(\\;)(_5\\(13p\\))", "\\1\\3\\2"), cod)) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod2, "^_6_6"), stringr::str_replace_all(cod, '(_6\\(.{1,3}\\)\\[1\\])(_6\\(.{1,3}\\)\\[1\\])', "\\1;\\2"), cod),
                cod = ifelse(stringr::str_detect(cod2, "^_6_6"), stringr::str_replace_all(cod, '(_6\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[0\\])+(_6\\(.{1,3}\\)\\[1\\])', "\\1;\\2\\3"), cod)) %>% 
  dplyr::mutate(x = stringr::str_split(cod, ";")) %>% 
  tidyr::unnest(x) %>%
  dplyr::mutate(cod1 = ifelse(x != cod, cod, "")) %>% 
  dplyr::mutate(cod2 = ifelse(x != cod, stringr::str_remove_all(x, "_(7|8|9|11|18)\\(.{1,3}\\)\\[.\\]"), cod2),
                cod2 = ifelse(x != cod, stringr::str_remove_all(cod2, "_(10)\\(0\\)\\[0\\]"), cod2),
                cod2 = ifelse(x != cod, stringr::str_remove_all(cod2, "_(16)\\(4.{2}\\)\\[0\\]"), cod2),
                cod2 = ifelse(x != cod, stringr::str_remove_all(cod2, "_(6)\\(18.{1}\\)\\[0\\]"), cod2),
                cod2 = ifelse(x != cod, stringr::str_remove_all(cod2, "\\(.{1,3}\\)|\\[.\\]"), cod2),) %>% 
  dplyr::mutate(cod = ifelse(x != cod, x, cod)) %>% 
  dplyr::select(-x) %>% 
  dplyr::mutate(ind = ifelse(stringr::str_detect(cod2, "_6(.+)?_16(.+)?_33(.+)?_3"), 1, 0),
                ind = ifelse(stringr::str_detect(cod2, "_6(.+)?_16(.+)?_3(.+)?_33"), 1, ind),
                ind = ifelse(stringr::str_detect(cod2, "_3(.+)?_4(.+)?_3"), ind + 2, ind),
                ind = ifelse(stringr::str_count(cod2, "_16") > 1, ind + 3, ind),
                ind = ifelse(stringr::str_detect(cod2, "_1_3"), ind + 7, ind),
                ind = ifelse(stringr::str_detect(cod2, "_10_4"), ind + 10, ind)) %>% 
  dplyr::mutate(val = stringr::str_replace_all(cod, "_[0-9]+\\(.+?\\)", ""),
                val = stringr::str_replace_all(val, "\\[|\\]", "")) %>% 
  dplyr::mutate(np_val = stringr::str_count(val, "1")) %>% 
  dplyr::mutate(val_play = "",
                val_play = ifelse(np_val == 1, stringr::str_extract(cod, '_[0-9]+\\(.{1,3}\\)\\[1\\]'), val_play)) %>% 
  dplyr::mutate(aux = stringr::str_detect(val, "11")) %>% 
  dplyr::arrange(cod) %>% 
  dplyr::mutate(np = stringr::str_count(cod, "_")) %>% 
  dplyr::group_by(np, cod, cod2, ind, val, np_val, val_play, aux) %>% 
  dplyr::summarise(n = sum(n), .groups = 'drop')

dplyr::filter(stringr::str_detect(cod2, '_4$')) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod2, '_4$') & stringr::str_count(cod, "_4\\(.{1,3}\\)\\[1\\]") == 1, stringr::str_replace(cod, "(_4\\(.{1,3}\\)\\[1\\])", ";\\1"), cod)) %>% 
  
  
xdf1 %>% 
  dplyr::mutate(aux2 = 0,
                aux2 = ifelse(stringr::str_detect(cod, "(_3\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\)\\[1\\])(_3\\(.{1,3}\\)\\[0\\])"), 1, aux2),
                aux2 = ifelse(stringr::str_detect(cod, "(_33\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\)\\[1\\])(_3\\(.{1,3}\\)\\[0\\])"), 1, aux2),
                aux2 = ifelse(stringr::str_detect(cod, "(_3\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\)\\[1\\])(_33\\(.{1,3}\\)\\[0\\])"), 1, aux2),
                aux2 = ifelse(stringr::str_detect(cod, "(_33\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\)\\[1\\])(_33\\(.{1,3}\\)\\[0\\])"), 1, aux2)) %>% 
  dplyr::filter(aux2 == 1) %>%
  dplyr::mutate(cod = stringr::str_replace_all(cod, "(_3\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])(_3\\(.{1,3}\\)\\[0\\])", "\\1\\2[0]\\4"),
                cod = stringr::str_replace_all(cod, "(_33\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])(_3\\(.{1,3}\\)\\[0\\])", "\\1\\2[0]\\4"),
                cod = stringr::str_replace_all(cod, "(_3\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])(_33\\(.{1,3}\\)\\[0\\])", "\\1\\2[0]\\4"),
                cod = stringr::str_replace_all(cod, "(_33\\(.{1,3}\\)\\[0\\])(_4\\(.{1,3}\\))(\\[1\\])(_33\\(.{1,3}\\)\\[0\\])", "\\1\\2[0]\\4"))
#xdf1 <- 
  
# xdf1 <- xdf1 %>% 
#   dplyr::mutate(cod = ifelse(cod2 %in% c("_6_3_3_4", "_6_3_4", "_6_3_4_3_4", "_16_33_4", "_6_3_3_3_4"), stringr::str_replace(cod, '(_4\\(.{1,3}\\)\\[1\\])', ";\\1"), cod)) %>% 
#   dplyr::mutate(x = stringr::str_split(cod, ";")) %>% 
#   tidyr::unnest(x) %>%
#   dplyr::mutate(cod1 = ifelse(x != cod, cod, "")) %>% 
#   dplyr::mutate(cod2 = ifelse(x != cod, stringr::str_remove_all(x, "_(7|8|9|11|18)\\(.{1,3}\\)\\[.\\]"), cod2),
#                 cod2 = ifelse(x != cod, stringr::str_remove_all(cod2, "_(10)\\(0\\)\\[0\\]"), cod2),
#                 cod2 = ifelse(x != cod, stringr::str_remove_all(cod2, "_(16)\\(4.{2}\\)\\[0\\]"), cod2),
#                 cod2 = ifelse(x != cod, stringr::str_remove_all(cod2, "_(6)\\(18.{1}\\)\\[0\\]"), cod2),
#                 cod2 = ifelse(x != cod, stringr::str_remove_all(cod2, "\\(.{1,3}\\)|\\[.\\]"), cod2),) %>% 
#   dplyr::mutate(cod = ifelse(x != cod, x, cod)) %>% 
#   dplyr::select(-x) %>% 
#   dplyr::mutate(ind = ifelse(stringr::str_detect(cod2, "_6(.+)?_16(.+)?_33(.+)?_3"), 1, 0),
#                 ind = ifelse(stringr::str_detect(cod2, "_6(.+)?_16(.+)?_3(.+)?_33"), 1, ind),
#                 ind = ifelse(stringr::str_detect(cod2, "_3(.+)?_4(.+)?_3"), ind + 2, ind),
#                 ind = ifelse(stringr::str_count(cod2, "_16") > 1, ind + 3, ind),
#                 ind = ifelse(stringr::str_detect(cod2, "_1_3"), ind + 7, ind),
#                 ind = ifelse(stringr::str_detect(cod2, "_10_4"), ind + 10, ind)) %>% 
#   dplyr::mutate(val = stringr::str_replace_all(cod, "_[0-9]+\\(.+?\\)", ""),
#                 val = stringr::str_replace_all(val, "\\[|\\]", "")) %>% 
#   dplyr::mutate(np_val = stringr::str_count(val, "1")) %>% 
#   dplyr::mutate(val_play = "",
#                 val_play = ifelse(np_val == 1, stringr::str_extract(cod, '_[0-9]+\\(.{1,3}\\)\\[1\\]'), val_play)) %>% 
#   dplyr::mutate(aux = stringr::str_detect(val, "11")) %>% 
#   dplyr::arrange(cod) %>% 
#   dplyr::mutate(np = stringr::str_count(cod, "_")) %>% 
#   dplyr::group_by(np, cod, cod2, ind, val, np_val, val_play, aux) %>% 
#   dplyr::summarise(n = sum(n), .groups = 'drop')

xdf1 %>% 
  dplyr::filter(stringr::str_detect(cod, "._4\\(.{1,3}\\)\\[1\\]_3")) %>% 
  dplyr::mutate(cod = ifelse(stringr::str_detect(cod, "._4\\(.{1,3}\\)\\[1\\]_3"), stringr::str_replace_all(cod, "(._4\\(.{1,3}\\))(\\[1\\])(_3)", "\\1[0]\\3"), cod)) %>% 
  dplyr::arrange(cod2, desc(n))

xdf1 %>% 
  dplyr::arrange(cod2, cod) %>% 
  dplyr::mutate(done = ifelse(np_val <= 1, 1, 0)) %>% 
  dplyr::filter(done == 0) %>% 
  dplyr::filter(stringr::str_detect(cod, "_6\\(13a\\)\\[1\\]_5\\(13p\\)\\[0\\]")) %>% 
  View()
  

xdf1 %>% 
  dplyr::arrange(cod2, cod) %>% 
  dplyr::mutate(done = ifelse(np_val <= 1, 1, 0)) %>% 
  dplyr::filter(done == 0) %>% 
  dplyr::group_by(np_val, cod2) %>% 
  dplyr::summarise(cnt = sum(n), .groups = 'drop') %>% 
  dplyr::mutate(np_val = paste0("npval", np_val)) %>%
  tidyr::spread(np_val, cnt) %>% 
  dplyr::arrange(desc(npval2)) %>% 
  print(., n = 900)
  
 xdf1 %>% 
  dplyr::filter(np_val > 1, stringr::str_detect(cod2, "_4$"), stringr::str_detect(cod2, '_10_4', negate =T)) %>% 
  dplyr::mutate(cod = ifelse(np_val > 1 & stringr::str_detect(cod2, "_4$"), stringr::str_replace_all(cod, "(_4\\(.{1,3}\\)\\[1\\]$)", ";\\1"), cod),
                cod = ifelse(np_val > 1 & stringr::str_detect(cod2, "_4$"), stringr::str_replace_all(cod, "(_4\\(.{1,3}\\)\\[1\\])(_[7|8|9|11|18]\\(.{1,3}\\)\\[0\\])+", ";\\1\\2"), cod))
  dplyr::arrange(cod) %>% 
  dplyr::group_by(np_val, cod2) %>% 
  dplyr::summarise(cnt = sum(n), .groups = 'drop') %>% 
  dplyr::mutate(np_val = paste0("npval", np_val)) %>%
  tidyr::spread(np_val, cnt) %>%
  dplyr::arrange(desc(npval3), desc(npval2), desc(npval1)) %>% 
  print(., n = 900)

  # 
  
xdf1 %>% 
  dplyr::group_split(np_val) %>% 
  '[['(3)



    dplyr::mutate(cod2 = stringr::str_remove_all(cod, "_[0-9]+\\(.{1,3}\\)\\[0\\]"),
                cod2 = stringr::str_remove_all(cod2, "\\(.{1,3}\\)\\[1\\]")) %>% 
  dplyr::group_by(cod, cod2) %>% 
  dplyr::summarise()
  
  dplyr::filter(stringr::str_detect(cod2, "_1_|_1$")) %>% 
  
  dplyr::filter(stringr::str_detect(cod, "\\;", negate = F)) %>% 
  dplyr::arrange(cod2, val) %>% 
  dplyr::slice(1:900) %>% 
  print(., n = 900)


# 10 primeiros feitos
xdf %>% 
  dplyr::mutate(done = 0,
                done = ifelse(np == 1 & np_val == 1, 1, done),
                done = ifelse(np == 2 & np_val == 2, 1, done),
                done = ifelse(np == 2 & np_val == 1, 1, done),
                done = ifelse(np == 3 & np_val == 1, 1, done),
                done = ifelse(np == 4 & np_val == 1, 1, done),
                done = ifelse(np == 3 & np_val == 2 & aux == 1, 1, done),
                done = ifelse(np == 5 & np_val == 1, 1, done),
                done = ifelse(np == 1 & np_val == 0, 1, done),
                done = ifelse(np == 3 & np_val == 3, 1, done),
                done = ifelse(np == 4 & np_val == 2, 1, done),
                )
  dplyr::mutate(cod = ifelse(cod2 == '_10_4', stringr::str_replace(cod, "(_4\\(.{1,3}\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(cod2 == '_2_10_4', stringr::str_replace(cod, "(_4\\(.{1,3}\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(np_val == 2 & np == 2 & aux == 1 & cod2 == '_13_16', stringr::str_replace(cod, "(_16\\(.{1,3}\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(cod2 == '_16_16', stringr::str_replace(cod, "(_16\\(.{1,3}\\))\\[1\\]$", "\\1[0]"), cod),
                cod = ifelse(cod2 == '_4_5' & stringr::str_detect(cod, "_5\\(37t\\)"), stringr::str_replace(cod, "(_5\\(37t\\))\\[1\\]$", "\\1[0]"), cod),
                cod = ifelse(cod2 == '_2_4_5' & stringr::str_detect(cod, "_5\\(37t\\)"), stringr::str_replace(cod, "(_5\\(37t\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(np_val == 1 & np == 2 & aux == 0 & cod == '_10(0)[0]_12(0)[1]', stringr::str_replace(cod, "(_12\\(0\\))\\[1\\]$", "\\1[0]"), cod),
                cod = ifelse(np_val == 1 & np == 2 & aux == 0 & cod2 %in% c("_5_1", "_5_2"), stringr::str_replace(cod, "(_5\\(13p\\))\\[0\\]$", "\\1[1]"), cod),
                cod = ifelse(np_val == 2 & np == 3 & aux == 1 & cod2 == "_6_5_3", stringr::str_replace(cod, "(_5\\(20p\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(cod2 == "_16_12_33", stringr::str_replace(cod, "(_12\\(.\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(cod2 == "_33_16_12", stringr::str_replace(cod, "(_12\\(.\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(cod2 == "_33_4_12", stringr::str_replace(cod, "(_12\\(.\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(cod2 == "_6_12", stringr::str_replace(cod, "(_12\\(.\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(cod2 == "_16_5_33", stringr::str_replace(cod, "(_5\\(.{1,3}\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(cod2 == "_6_5_3", stringr::str_replace(cod, "(_5\\(.{1,3}\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(cod2 == '_1_16_16', stringr::str_replace(cod, "(_16\\(.{1,3}\\))\\[1\\]$", "\\1[0]"), cod),
                cod = ifelse(cod2 == '_5_16_16', stringr::str_replace(cod, "(_16\\(.{1,3}\\))\\[1\\]$", "\\1[0]"), cod),
                cod = ifelse(cod2 == '_6_16_33_3', stringr::str_replace(cod, "(_16\\(.{1,3}\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(cod2 == '_16_16_33_33', stringr::str_replace(cod, "(_16\\(.{1,3}\\)\\[1\\]_16\\(.{1,3}\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(np_val == 3 & np == 3 & cod2 == "_1_6_5", stringr::str_replace(cod, "(_5\\(.{1,3}\\))\\[1\\]", "\\1[0]"), cod),
                )


xdf %>% 
  dplyr::group_by(np, aux, np_val) %>% 
  dplyr::summarise(cnt = sum(n), n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::mutate(aux = as.integer(aux)) %>% 
  dplyr::mutate(perc = 100 * cnt / sum(cnt), 
                perc = round(perc, 2)) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  dplyr::mutate(row = dplyr::row_number()) %>% 
  dplyr::mutate(n = paste0(n, " (", perc, "%) [", row, "]")) %>% 
  dplyr::select(-c(cnt, perc, row)) %>% 
  tidyr::spread(np_val, n) %>% 
  print(., n = 40)


xdf %>%   
  dplyr::mutate(done = 0,
                done = ifelse(np_val == 0 & stringr::str_detect(cod, '_3|_5', negate = T), 1, done),
                done = ifelse(np_val == 1 & stringr::str_detect(cod, "^_5", negate = T) & stringr::str_detect(cod, "_3|_5"), 1, done),
                done = ifelse(np_val == 1 & stringr::str_detect(cod, "^_5") & stringr::str_detect(cod, "_3|_5\\(00p\\)|_5\\(20p\\)", negate = T), 1, done)) %>% 
  dplyr::mutate(cod = ifelse(np == 2 & np_val == 2 & cod2 == '_10_4', stringr::str_replace(cod, "(_4\\(.{1,3}\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(np == 2 & np_val == 2 & cod2 == '_13_16', stringr::str_replace(cod, "(_16\\(.{1,3}\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(np == 2 & np_val == 2 & cod2 == '_16_16', stringr::str_replace_all(cod, "(_16\\(.{1,3}\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(np == 2 & np_val == 2 & cod2 == '_4_5', stringr::str_replace_all(cod, "(_5\\(37t\\))\\[1\\]", "\\1[0]"), cod),
                cod = ifelse(np == 2 & np_val == 2 & cod2 == '_6_5', stringr::str_replace_all(cod, "(_5\\(20p\\))\\[1\\]", "\\1[0]"), cod)) %>% 
  dplyr::mutate(x = stringr::str_split(cod, ";")) %>% 
  tidyr::unnest(x) %>%

_12_33_4,_12_1, _13_13,_16_16, _16_33_12, _16_33_5, _1_3_6               

xdf %>% 
  dplyr::mutate(aux = ifelse(np_val == 2 & stringr::str_detect(val, '^11') & cod2 == '_4_5' & stringr::str_detect(cod, "_5\\(37t\\)"), 1, 0)) %>% 
  dplyr::mutate(cod = ifelse(aux == 1, stringr::str_replace(cod, '_5\\(37t\\)\\[1\\]', "_5(37t)[0]"), cod),
                np_val = ifelse(aux == 1, 1, np_val),
                val = ifelse(aux == 1, stringr::str_replace(val, "^11", "10"), val)) %>% 
  dplyr::mutate(aux = ifelse(np_val == 2 & stringr::str_detect(val, '^11') & stringr::str_detect(cod, "_5\\(20p\\)"), 1, 0)) %>% 
  dplyr::mutate(cod = ifelse(aux == 1, stringr::str_replace(cod, '_5\\(20p\\)\\[1\\]', "_5(20p)[0]"), cod),
                np_val = ifelse(aux == 1, 1, np_val),
                val = ifelse(aux == 1, stringr::str_replace(val, "^11", "10"), val)) 

# errado
# cod2 
## _13_1              
## _6_1_3
## _6_4_3_3             
## _6_5_3               
## _6_5_3_3             
## _6_5_3_4_3           


_16_16
_16_16_33_33
_16_5_33
_16_6_33_3_3
_16_6_33_3_4_3
_6_16_33_3           
_6_16_33_3_3         
_6_16_33_3_3_3       
_6_16_33_3_3_4_3     
_6_16_33_3_4_3       
_6_16_3_3            
_6_16_3_33           
_6_16_3_33           
_6_16_3_3_3          
_6_16_3_3_33         


               

tp_plays <- xdf %>% 
  dplyr::filter(np_val == 2) %>% 
  dplyr::filter(stringr::str_detect(val, '^11')) %>% 
  dplyr::arrange(cod2)

xdf %>% 
  dplyr::filter(np_val == 2) %>% 
  dplyr::group_split(val_play) %>% 
  '['(75:96) %>%
  dplyr::bind_rows() %>% 
  print(., n = 900)


xdf2 <- xdf %>%
  dplyr::mutate(cod = stringr::str_replace(cod, "(_18\\(.\\))(\\[0\\])(_13\\(.\\))(\\[1\\])", "\\1\\4\\3\\2")) %>% 
  
  dplyr::mutate(aux = ifelse(stringr::str_detect(cod, "(_10\\([1|2]\\))"), 1, 0)) %>% 
  dplyr::mutate(cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_33\\(.{1,3}\\))\\[0\\]", "\\1[2]"), cod), 
                cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_3\\(.{1,3}\\))\\[0\\]", "\\1[2]"), cod), 
                cod = ifelse(aux == 1, stringr::str_replace_all(cod, "(_7\\(6p\\))\\[0\\]", "\\1[2]"), cod)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(cod_ = ifelse(ind == 0, 
                              sub_all_patterns(string = cod, pattern = "(_[0-9]+\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])", replacement = "\\1,\\2"), 
                              cod)) %>% 
  dplyr::mutate(cod_ = ifelse(ind == 0 & aux == 1, 
                              sub_all_patterns(string = cod_, pattern = "(_[0-9]+\\(.{1,3}\\))(\\[0\\])(_[0-9]+\\(.{1,3}\\))(\\[1\\])", replacement = "\\1\\4\\3\\2"), 
                              cod_)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cod_ = stringr::str_replace_all(cod_, "\\[2\\]", "[0]")) %>% 
  dplyr::mutate(cod_ = stringr::str_split(cod_, "\\,")) %>% 
  tidyr::unnest(cod_) %>% 
  dplyr::group_by(cod_) %>% 
  dplyr::mutate(n_ = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(np_ = stringr::str_count(cod_, "_")) %>% 
  dplyr::mutate(sc = stringr::str_remove_all(cod_, "_[0-9]+\\(.{1,3}\\)\\[0\\]")) %>% 
  dplyr::mutate(val_ = stringr::str_replace_all(cod_, "_[0-9]+\\(.+?\\)", ""),
                val_ = stringr::str_replace_all(val_, "\\[|\\]", ""))


xdf2 %>% 
  dplyr::distinct(ind, np_, cod_, n_, val_) %>% 
  dplyr::mutate(np_val = stringr::str_count(val_, "1")) %>% 
  dplyr::filter(ind == 0) %>% 
  dplyr::group_split(np_val)

tp_plays <- xdf2 %>% 
  dplyr::filter(ind == 0, np == np_, np > 1, stringr::str_count(val_, "1") > 1) %>% 
  dplyr::select(-cod_) %>% 
  dplyr::group_split(val_)

# ta errado 
## cod2
###  _3_3_4_6, _33_6_16, _6_3_33_3_16, _6_5_3_3    

## cod
### _6(19a)[1]_6(20a)[1]_5(00p)[0]_18(0)[0]_3(51c)[0]_3(52c)[0]_8(0)[0]_3(51c)[0]_3(52c)[0]

tp_plays <- xdf2 %>%
  dplyr::filter(cod != cod2) %>% 
  dplyr::group_split(cod2)

tp_plays <- xdf %>% 
  dplyr::group_by(cod2) %>% 
  dplyr::summarise(n = sum(n)) %>% 
  dplyr::mutate(np = stringr::str_count(cod2, '_')) %>%
  dplyr::mutate(ind = ifelse(stringr::str_detect(cod2, "_6(.+)?_16(.+)?_33(.+)?_3"), 1, 0),
                ind = ifelse(stringr::str_detect(cod2, "_6(.+)?_16(.+)?_3(.+)?_33"), 1, ind),
                ind = ifelse(stringr::str_detect(cod2, "_3(.+)?_4(.+)?_3"), ind + 2, ind),
                ind = ifelse(stringr::str_count(cod2, "_16") > 1, ind + 3, ind),
                ind = ifelse(stringr::str_detect(cod2, "_1_3"), ind + 7, ind),
                ind = ifelse(stringr::str_detect(cod2, "_10_4"), ind + 10, ind)) %>% 
  dplyr::select(ind, np, cod2, n) %>% 
  dplyr::arrange(np) %>% 
  dplyr::group_split(ind)

tp_plays %>% 
  plyr::l_ply(.data = ., .fun = function(x){
    
    x %>% 
      dplyr::arrange(desc(n)) %>% 
      print(., n = 900)
    
    scan(what = 'd')
    
  })




xdf2


'_12(1)[1]_10(0)[0]_2(4b)[1]' %>% 
  sub_all_patterns(., "(_[0-9]+\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2") %>% 
  sub_all_patterns(., "(_[0-9]+\\(.{1,3}\\)\\[0\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2")

xdf1 <- xdf %>%
  dplyr::mutate(cod = stringr::str_replace(cod, "^(_[0-9]+\\(.{1,3}\\))(\\[0\\])(_[0-9]+\\(.{1,3}\\))(\\[1\\])", "\\1\\4\\3\\2")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(cod = sub_all_patterns(cod, "(_[0-9]+\\(.{1,3}\\)\\[1\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2"),
                cod = sub_all_patterns(cod, "(_[0-9]+\\(.{1,3}\\)\\[0\\])(_[0-9]+\\(.{1,3}\\)\\[1\\])", "\\1;\\2")) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(x = stringr::str_split(cod, ";")) %>% 
  tidyr::unnest(x) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(x) %>% 
  dplyr::summarise(n = sum(n)) %>% 
  dplyr::mutate(simp_cod = stringr::str_extract(x, "(_[0-9]+\\(.{1,3}\\)\\[1\\])")) %>% 
  dplyr::group_by(simp_cod) %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(simp_cod, cnt, x, n) %>% 
  dplyr::mutate(np = stringr::str_count(x, "_")) %>% 
  dplyr::arrange(simp_cod, np) %>% 
  dplyr::group_split(simp_cod)
  
  
xdf1[[10]] %>% print(., n = 300)
  