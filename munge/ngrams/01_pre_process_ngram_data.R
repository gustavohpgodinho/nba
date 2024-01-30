
# git token
#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

require(tidyverse)

put_features_cod <- function(df){
  
  df1 <- df %>% 
    dplyr::group_by(joinpcod, joincod, si) %>%
    dplyr::mutate(fullcod = paste0(cod, "[", row, ",", importance, "]")) %>% 
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
  
  return(df1)
  
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
                  join = ifelse(stringr::str_detect(cod, "_(3|10|11|14|4|7\\(2|7\\(3|7\\(4|7\\(6)"), 1, join),
                  join = ifelse(stringr::str_detect(cod, "_5\\((38|39|40)"), 0, join),
                  join = ifelse(stringr::str_detect(cod, "_5\\(00|_5\\(13|_5\\(19|_5\\(20|_5\\(37"), 1, join)) %>%
    dplyr::group_by(joincod) %>%
    dplyr::mutate(sum_join = cumsum(join)) %>%
    dplyr::mutate(ref = ifelse(stringr::str_detect(cod, "_(6|16)\\((0|10|11|12|13|14|15|16|17|19|20|21|22)") & importance == 1, sum_join, 1000)) %>% 
    dplyr::arrange(joincod, desc(row)) %>% 
    dplyr::mutate(ref = cummin(ref),
                  max_si = max(si)) %>% 
    dplyr::arrange(joincod, row) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(si = ifelse(ref < 1000 & join == 0 & (sum_join + 1) == ref & sum_join > 0 & si < max_si, si + 1, si)) %>% 
    dplyr::select(-c(join, sum_join, ref, max_si))
  
}

group_plays_around_turnovers <- function(df){
  
  # REF É O GRUPO QUE DESEJAMOS MOVER A AÇÃO
  # SUM_JOIN É O GRUPO ATUAL DA AÇÃO
  # A AÇÃO SÓ PODE MOVER PARA O GRUPO SEGUINTE
  
  # MOVA A AÇÃO SE 
  ## HÁ UMA AÇÃO DE INTERESSE NO GRUPO SEGUINTE (EXEMPLO FALTA), >> (ou seja, se ref == 1000, não há uma ação de interesse no grupo seguinte)
  ## E ESSA AÇÃO PODE SER MOVIDA (join = 0), 
  ## E SE O GRUPO FUTURO É SEGUINTE AO GRUPO ATUAL (sum_join + 1) == ref
  ## E SE O GRUPO ATUAL (sum_join) NÃO É O IDENTIFICADO COMO GRUPO 0 (sum_join != 0, isso já está tratado em outra parte do código)
  ## E SE EXISTIR UM GRUPO EM si SEGUINTE AO ATUAL DA AÇÃO A SER MOVIDA (ou seja, si < max(si))
  
  
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

put_nums <- function(df){
  
  df %>% 
    dplyr::mutate(valcod = stringr::str_replace_all(valcod, '(_)(6\\(0)', '\\11\\2'),
                  valcod = stringr::str_replace_all(valcod, '(_)(3\\(4)', '\\13\\2'),
                  valcod = stringr::str_replace_all(valcod, '(_3\\(.{1,3}\\))_4', '\\1_14'),
                  valcod = stringr::str_replace_all(valcod, '(_33\\(.{1,3}\\))_4', '\\1_14')) %>% 
    dplyr::mutate(modcod = stringr::str_remove_all(valcod, "\\(.{1,3}\\)")) %>% 
    dplyr::mutate(modcod = stringr::str_remove_all(modcod, '_14')) %>% 
    dplyr::mutate(sum_foul = stringr::str_count(valcod, "_6\\((1|2)"), 
                  sum_ft = stringr::str_count(valcod, "_3\\("), 
                  sum_tech = stringr::str_count(valcod, "_16\\(0"), 
                  sum_techft = stringr::str_count(valcod, "_33\\("),
                  sum_offfoul = stringr::str_count(valcod, "_6\\(.{1,3}\\)_5\\((00|13)p"),
                  sum_offturn = stringr::str_count(valcod, "_5\\((00|13)p"),
                  sum_shcturn = stringr::str_count(valcod, "_5\\(37t\\)"),
                  sum_lanturn = stringr::str_count(valcod, "_5\\((19|20)"),
                  sum_lanevio = stringr::str_count(joincod, "_7\\(6"),
                  sum_jumpbal = stringr::str_count(valcod, "_10\\(.\\)_4"),
                  sum_start = stringr::str_count(modcod, "._12"),
                  sum_end = stringr::str_count(modcod, "_13_"),
                  sum_turnov = stringr::str_count(modcod, "(_6_5_3|_16_5_33|_3_5_3|_33_5_33)"),
                  sum_ftfoul = stringr::str_count(modcod, "(_3_6|_33_16)"),
                  sum_jumpbt = stringr::str_count(valcod, "_5\\(21p\\)_10\\(.\\)"),
                  sum_doubtu = stringr::str_count(valcod, "_5\\(26p\\)"),
                  sum_teamtu = stringr::str_count(valcod, "_5\\((39|40)t\\)"),
    ) %>% 
    dplyr::mutate_at(.vars = c('sum_foul', 'sum_ft', 'sum_tech', 'sum_techft', 
                               'sum_offfoul', 'sum_offturn', 'sum_shcturn',
                               'sum_lanturn', 'sum_lanevio', 'sum_jumpbal',
                               'sum_start', 'sum_end', 'sum_turnov', 'sum_ftfoul', 
                               'sum_jumpbt', 'sum_doubtu', 'sum_teamtu'), 
                     .funs = function(x){ifelse(x >= 10, 9, x)}) %>% 
    dplyr::mutate(nums = paste0(sum_foul, sum_ft, sum_tech, sum_techft, 
                                sum_offfoul, sum_offturn, sum_shcturn,
                                sum_lanturn, sum_lanevio, sum_jumpbal,
                                sum_start, sum_end, sum_turnov, sum_ftfoul,
                                sum_jumpbt, sum_doubtu, sum_teamtu)) %>% 
    dplyr::mutate(tags = "",
                  tags = ifelse(sum_foul > 0, paste0(tags, sum_foul, " fouls;"), tags),
                  tags = ifelse(sum_ft > 0, paste0(tags, sum_ft, " freethrow;"), tags),
                  tags = ifelse(sum_tech > 0, paste0(tags, sum_tech, " techfoul;"), tags),
                  tags = ifelse(sum_techft > 0, paste0(tags, sum_techft, " techfreethrow;"), tags),
                  tags = ifelse(sum_offfoul > 0, paste0(tags, sum_offfoul, " offfoul;"), tags),
                  tags = ifelse(sum_offturn > 0, paste0(tags, sum_offturn, " offturn;"), tags),
                  tags = ifelse(sum_shcturn > 0, paste0(tags, sum_shcturn, " shotclock;"), tags),
                  tags = ifelse(sum_lanturn > 0, paste0(tags, sum_lanturn, " laneturn;"), tags),
                  tags = ifelse(sum_lanevio > 0, paste0(tags, sum_lanevio, " lanevio;"), tags),
                  tags = ifelse(sum_jumpbal > 0, paste0(tags, sum_jumpbal, " jbrebound;"), tags),
                  tags = ifelse(sum_start > 0, paste0(tags, sum_start, " beforestart;"), tags),
                  tags = ifelse(sum_end > 0, paste0(tags, sum_end, " afterend;"), tags),
                  tags = ifelse(sum_turnov > 0, paste0(tags, sum_turnov, " wrongturnover;"), tags),
                  tags = ifelse(sum_ftfoul > 0, paste0(tags, sum_ftfoul, " ftbeforefoul;"), tags),
                  tags = ifelse(sum_jumpbt > 0, paste0(tags, sum_jumpbt, " jbturnover;"), tags),
                  tags = ifelse(sum_doubtu > 0, paste0(tags, sum_doubtu, " doubleturnover;"), tags),
                  tags = ifelse(sum_teamtu > 0, paste0(tags, sum_teamtu, " teamturnover;"), tags)) %>% 
    dplyr::select(-c(sum_foul, sum_ft, sum_tech, sum_techft, sum_offfoul, sum_offturn, 
                     sum_shcturn, sum_lanturn, sum_lanevio, sum_jumpbal, sum_start, 
                     sum_end, sum_turnov, sum_ftfoul, sum_jumpbt, sum_doubtu, 
                     sum_teamtu))
  
  
}

put_codf <- function(df){
  
  df %>% 
    dplyr::mutate(codf = NA_character_,
                  codf = ifelse(is.na(codf) & valcod == "", valcod, codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_12\\(.\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_13\\(.\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_1\\(.+?\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_5\\(37t\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_2\\(.+?\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_5\\(21p\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_10\\(.+?\\)"), codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(valcod, "_7\\(4"), "_10(1)", codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_5\\(19p\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_5\\(20p\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(33c\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(33w\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(22c\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(22w\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(11c\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(11w\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(52c\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(52w\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(72c\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(72w\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(83c\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(83w\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(61c\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(61w\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_33\\(40c\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_33\\(40w\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_7\\(6p\\)$"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_4\\(.+?\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(22c\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(33c\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(11c\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(52c\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(22w\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(33w\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_3\\(11w\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_5\\(13p\\)"), codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(valcod, "_5\\(00p\\)"), "_5(13p)", codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "_5\\(.+?\\)"), codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(valcod, "_3", neg = T), stringr::str_extract(valcod, "_6\\(.+?\\)"), codf),
                  codf = ifelse(is.na(codf), stringr::str_extract(valcod, "^_14\\(.+?\\)"), codf),
                  codf = ifelse(is.na(codf) & stringr::str_detect(valcod, "^(_16\\(.+?\\))+$"), "", codf),
                  codf = ifelse(is.na(codf) & valcod %in% c("_6(11a)_3(13w)_14(1t)_3(23c)", 
                                                            "_6(15p)_3(12c)", "_6(20a)_3(51c)", "_6(11p)_3(12c)", 
                                                            "_6(12a)_3(12w)"), "_5(20p)", codf),
                  codf = ifelse(is.na(codf) & valcod %in% c("_6(19p)_3(51c)", "_6(19a)_3(51c)", "_6(20a)_3(51c)"), "_3(61c)", codf),
                  codf = ifelse(is.na(codf) & valcod %in% c("_6(11a)_3(12c)_3(12c)",
                                                            "_6(10p)_3(00w)_14(0t)_3(00c)"), "_3(22c)", codf), 
                  codf = ifelse(is.na(codf) & valcod %in% c("_3(23c)"), "_3(33c)", codf),
                  codf = ifelse(codf == "_14(0u)", "_14(0p)", codf),
                  codf = ifelse(codf == "_4(0u)", "_4(0p)", codf),
                  codf = ifelse(codf == "_5(19p)" & valcod != "_5(19p)", "_5(20p)", codf),
                  codf = ifelse(codf == "_6(13a)", "_5(13p)", codf),
                  codf = ifelse(codf == "_3(11c)" & stringr::str_detect(valcod, "_6\\(22.\\)"), "_6(22x)_3(11c)", codf),
                  codf = ifelse(codf == "_3(11w)" & stringr::str_detect(valcod, "_6\\(22.\\)"), "_6(22x)_3(11w)", codf),
                  codf = ifelse(codf == "_3(11c)" & stringr::str_detect(valcod, "_6\\((14|15).\\)"), "_6(15x)_3(11c)", codf),
                  codf = ifelse(codf == "_3(11w)" & stringr::str_detect(valcod, "_6\\((14|15).\\)"), "_6(15x)_3(11w)", codf),
                  codf = ifelse(codf == "_3(11c)" & valcod %in% c("_16(05p)_3(11c)", "_16(01p)_3(11c)", "_3(11c)_16(01c)", "_16(06t)_3(11c)"), "_33(40c)", codf),
                  codf = ifelse(codf == "_3(22c)" & valcod %in% c("_6(17a)_3(12c)_3(22c)"), "_3(72c)", codf),
                  codf = ifelse(codf == "_3(22w)" & valcod %in% c("_6(17p)_3(12c)_3(22w)"), "_3(72w)", codf),
                  codf = ifelse(codf == "_3(22w)" & valcod %in% c("_6(19a)_3(12c)_3(22w)"), "_3(52w)", codf),
                  codf = ifelse(codf == "_3(33c)" & valcod %in% c("_6(19p)_3(81c)_3(23c)_3(33c)", "_6(19a)_3(51c)_3(52c)_3(33c)", 
                                                                  "_6(19a)_3(81c)_3(23c)_3(33c)", "_6(19a)_3(81w)_14(1t)_3(23c)_3(33c)"), "_3(83c)", codf),
                  codf = ifelse(codf == "_33(40c)" & stringr::str_detect(valcod, "_33\\(40w\\)$"), "_33(40w)", codf),
                  codf = ifelse(codf == "_33(40c)" & valcod %in% c("_6(10p)_16(01p)_33(40w)_14(1t)_16(01p)_33(40c)_3(11c)_3(12c)", 
                                                                   "_6(11a)_3(12c)_33(40c)_3(22c)_16(01p)", "_6(21p)_3(12c)_33(40c)_3(22c)_16(01p)",
                                                                   "_16(01c)_33(40c)_3(12c)_3(22c)_6(11a)", "_6(11p)_33(40c)_3(12c)_3(22c)_16(01c)"), "_3(22c)", codf),
                  codf = ifelse(codf == "_33(40c)" & valcod %in% c("_6(11p)_16(08t)_33(40c)_3(11c)_5(39t)"), "_3(11c)", codf),
                  codf = ifelse(codf == "_5(13p)" & valcod %in% c("_6(11a)_3(12c)_5(00p)", "_6(11p)_3(12c)_5(00p)"), "_5(20p)", codf)) %>% 
    dplyr::mutate(pcodf = NA_character_,
                  pcodf = ifelse(codf == "", "", pcodf),
                  pcodf = ifelse(codf == '_1(1)', stringr::str_extract(fullpcod, "p[0-9]{3}_1\\(1\\)"), pcodf),
                  pcodf = ifelse(codf == '_1(1a)', stringr::str_extract(fullpcod, "p[0-9]{3}_1\\(1a\\)"), pcodf),
                  pcodf = ifelse(codf == '_1(2)', stringr::str_extract(fullpcod, "p[0-9]{3}_1\\(2\\)"), pcodf),
                  pcodf = ifelse(codf == '_1(2a)', stringr::str_extract(fullpcod, "p[0-9]{3}_1\\(2a\\)"), pcodf),
                  pcodf = ifelse(codf == '_1(3)', stringr::str_extract(fullpcod, "p[0-9]{3}_1\\(3\\)"), pcodf),
                  pcodf = ifelse(codf == '_1(3a)', stringr::str_extract(fullpcod, "p[0-9]{3}_1\\(3a\\)"), pcodf),
                  pcodf = ifelse(codf == '_1(4)', stringr::str_extract(fullpcod, "p[0-9]{3}_1\\(4\\)"), pcodf),
                  pcodf = ifelse(codf == '_1(4a)', stringr::str_extract(fullpcod, "p[0-9]{3}_1\\(4a\\)"), pcodf),
                  pcodf = ifelse(codf == '_1(5)', stringr::str_extract(fullpcod, "p[0-9]{3}_1\\(5\\)"), pcodf),
                  pcodf = ifelse(codf == '_1(5a)', stringr::str_extract(fullpcod, "p[0-9]{3}_1\\(5a\\)"), pcodf),
                  pcodf = ifelse(codf == '_12(0)', stringr::str_extract(fullpcod, "p[0-9]{3}_10\\(0\\)"), pcodf),
                  pcodf = ifelse(codf == '_12(0)' & stringr::str_detect(valcod, "_7\\(4p\\)"), stringr::str_extract(fullpcod, "p[0-9]{3}_7\\(4p\\)"), pcodf),
                  pcodf = ifelse(codf == '_12(0)' & pcodf == "p401_7(4p)", "p455_10(0)", pcodf),
                  pcodf = ifelse(codf == '_12(0)' & pcodf == "p501_7(4p)", "p454_10(0)", pcodf),
                  pcodf = ifelse(codf == '_12(0)' & is.na(pcodf), "p454_10(0)", pcodf),
                  pcodf = ifelse(codf == '_12(0)', paste0("p000_12(0)", pcodf), pcodf),
                  pcodf = ifelse(codf == '_12(1)', stringr::str_extract(fullpcod, "p[0-9]{3}_10\\(0\\)"), pcodf),
                  pcodf = ifelse(codf == '_12(1)' & stringr::str_detect(valcod, "_7\\(4p\\)"), stringr::str_extract(fullpcod, "p[0-9]{3}_7\\(4p\\)"), pcodf),
                  pcodf = ifelse(codf == '_12(1)' & pcodf == "p401_7(4p)", "p455_10(0)", pcodf),
                  pcodf = ifelse(codf == '_12(1)' & pcodf == "p501_7(4p)", "p454_10(0)", pcodf),
                  pcodf = ifelse(codf == '_12(1)' & is.na(pcodf), "p454_10(0)", pcodf),
                  pcodf = ifelse(codf == '_12(1)', paste0("p000_12(1)", pcodf), pcodf),
                  pcodf = ifelse(codf == '_12(2)', stringr::str_extract(fullpcod, "p[0-9]{3}_12\\(2\\)"), pcodf),
                  pcodf = ifelse(codf == '_12(3)', stringr::str_extract(fullpcod, "p[0-9]{3}_12\\(3\\)"), pcodf),
                  pcodf = ifelse(codf == '_12(4)', stringr::str_extract(fullpcod, "p[0-9]{3}_12\\(4\\)"), pcodf),
                  pcodf = ifelse(codf == '_10(1)', stringr::str_extract(fullpcod, "p[0-9]{3}_10\\(1\\)"), pcodf),
                  pcodf = ifelse(codf == '_10(1)' & valcod == "_7(4p)", stringr::str_extract(fullpcod, "p[0-9]{3}_7\\(4p\\)"), pcodf),
                  pcodf = ifelse(codf == '_10(1)' & valcod == "_7(4p)" & pcodf == "p401_7(4p)", "p455_10(1)", pcodf),
                  pcodf = ifelse(codf == '_10(1)' & valcod == "_7(4p)" & pcodf == "p501_7(4p)", "p454_10(1)", pcodf),
                  pcodf = ifelse(codf == '_10(2)', stringr::str_extract(fullpcod, "p[0-9]{3}_10\\(2\\)"), pcodf),
                  pcodf = ifelse(codf == '_13(0)', stringr::str_extract(fullpcod, "p[0-9]{3}_13\\(0\\)"), pcodf),
                  pcodf = ifelse(codf == '_13(1)', stringr::str_extract(fullpcod, "p[0-9]{3}_13\\(1\\)"), pcodf),
                  pcodf = ifelse(codf == '_13(2)', stringr::str_extract(fullpcod, "p[0-9]{3}_13\\(2\\)"), pcodf),
                  pcodf = ifelse(codf == '_13(3)', stringr::str_extract(fullpcod, "p[0-9]{3}_13\\(3\\)"), pcodf),
                  pcodf = ifelse(codf == '_13(4)', stringr::str_extract(fullpcod, "p[0-9]{3}_13\\(4\\)"), pcodf),
                  pcodf = ifelse(codf == '_14(0p)', stringr::str_extract(fullpcod, "p[0-9]{3}_4\\(0p\\)"), pcodf),
                  pcodf = ifelse(codf == '_14(0p)' & stringr::str_detect(valcod, '_14\\(0u\\)'), "p400_4(0p)", pcodf),
                  pcodf = ifelse(codf == '_14(0t)', stringr::str_extract(fullpcod, "p[0-9]{3}_4\\(0t\\)"), pcodf),
                  pcodf = ifelse(codf == '_14(1t)', stringr::str_extract(fullpcod, "p[0-9]{3}_4\\(1t\\)"), pcodf),
                  pcodf = ifelse(codf == '_2(1)', stringr::str_extract(fullpcod, "p[0-9]{3}_2\\(1\\)"), pcodf),
                  pcodf = ifelse(codf == '_2(1b)', stringr::str_extract(fullpcod, "p[0-9]{3}_2\\(1b\\)"), pcodf),
                  pcodf = ifelse(codf == '_2(2)', stringr::str_extract(fullpcod, "p[0-9]{3}_2\\(2\\)"), pcodf),
                  pcodf = ifelse(codf == '_2(2b)', stringr::str_extract(fullpcod, "p[0-9]{3}_2\\(2b\\)"), pcodf),
                  pcodf = ifelse(codf == '_2(3)', stringr::str_extract(fullpcod, "p[0-9]{3}_2\\(3\\)"), pcodf),
                  pcodf = ifelse(codf == '_2(3b)', stringr::str_extract(fullpcod, "p[0-9]{3}_2\\(3b\\)"), pcodf),
                  pcodf = ifelse(codf == '_2(4)', stringr::str_extract(fullpcod, "p[0-9]{3}_2\\(4\\)"), pcodf),
                  pcodf = ifelse(codf == '_2(4b)', stringr::str_extract(fullpcod, "p[0-9]{3}_2\\(4b\\)"), pcodf),
                  pcodf = ifelse(codf == '_2(5)', stringr::str_extract(fullpcod, "p[0-9]{3}_2\\(5\\)"), pcodf),
                  pcodf = ifelse(codf == '_2(5b)', stringr::str_extract(fullpcod, "p[0-9]{3}_2\\(5b\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(11c)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(11c\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(11w)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(11w\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(22c)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(22c\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(22c)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_3"), pcodf),
                  pcodf = ifelse(codf == '_3(22c)' & stringr::str_detect(fullpcod, "p400"), "p400_3(22c)", pcodf),
                  pcodf = ifelse(codf == '_3(22c)' & stringr::str_detect(fullpcod, "p500"), "p500_3(22c)", pcodf),
                  pcodf = ifelse(codf == '_3(22w)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(22w\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(33c)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(33c\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(33c)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_3"), pcodf),
                  pcodf = ifelse(codf == '_3(33c)' & stringr::str_detect(pcodf, "p400"), "p400_3(33c)", pcodf),
                  pcodf = ifelse(codf == '_3(33c)' & stringr::str_detect(pcodf, "p500"), "p500_3(33c)", pcodf),
                  pcodf = ifelse(codf == '_3(33w)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(33w\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(52c)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(52c\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(52w)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(52w\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(52w)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_3"), pcodf),
                  pcodf = ifelse(codf == '_3(52w)' & stringr::str_detect(pcodf, "p400"), "p400_3(52w)", pcodf),
                  pcodf = ifelse(codf == '_3(52w)' & stringr::str_detect(pcodf, "p500"), "p500_3(52w)", pcodf),
                  pcodf = ifelse(codf == '_3(61c)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(61c\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(61c)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_3"), pcodf),
                  pcodf = ifelse(codf == '_3(61c)' & stringr::str_detect(pcodf, "p400"), "p400_3(61c)", pcodf),
                  pcodf = ifelse(codf == '_3(61c)' & stringr::str_detect(pcodf, "p500"), "p500_3(61c)", pcodf),
                  pcodf = ifelse(codf == '_3(72c)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(72c\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(72c)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_3"), pcodf),
                  pcodf = ifelse(codf == '_3(72c)' & stringr::str_detect(pcodf, "p400"), "p400_3(72c)", pcodf),
                  pcodf = ifelse(codf == '_3(72c)' & stringr::str_detect(pcodf, "p500"), "p500_3(72c)", pcodf),
                  pcodf = ifelse(codf == '_3(72w)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(72w\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(72w)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_3"), pcodf),
                  pcodf = ifelse(codf == '_3(72w)' & stringr::str_detect(pcodf, "p400"), "p400_3(72w)", pcodf),
                  pcodf = ifelse(codf == '_3(72w)' & stringr::str_detect(pcodf, "p500"), "p500_3(72w)", pcodf),
                  pcodf = ifelse(codf == '_3(83c)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(83c\\)"), pcodf),
                  pcodf = ifelse(codf == '_3(83c)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_3"), pcodf),
                  pcodf = ifelse(codf == '_3(83c)' & stringr::str_detect(pcodf, "p400"), "p400_3(83c)", pcodf),
                  pcodf = ifelse(codf == '_3(83c)' & stringr::str_detect(pcodf, "p500"), "p500_3(83c)", pcodf),
                  pcodf = ifelse(codf == '_3(83w)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(83w\\)"), pcodf),
                  pcodf = ifelse(codf == '_33(40c)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(40c\\)"), pcodf),
                  pcodf = ifelse(codf == '_33(40c)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_3"), pcodf),
                  pcodf = ifelse(codf == '_33(40c)' & stringr::str_detect(pcodf, "p400"), "p400_3(40c)", pcodf),
                  pcodf = ifelse(codf == '_33(40c)' & stringr::str_detect(pcodf, "p500"), "p500_3(40c)", pcodf),
                  pcodf = ifelse(codf == '_33(40w)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(40w\\)"), pcodf),
                  pcodf = ifelse(codf == '_4(0p)', stringr::str_extract(fullpcod, "p[0-9]{3}_4\\(0p\\)"), pcodf),
                  pcodf = ifelse(codf == '_4(0p)' & stringr::str_detect(valcod, '_4\\(0u\\)'), "p400_4(0p)", pcodf),
                  pcodf = ifelse(codf == '_4(0t)', stringr::str_extract(fullpcod, "p[0-9]{3}_4\\(0t\\)"), pcodf),
                  pcodf = ifelse(codf == '_4(1t)', stringr::str_extract(fullpcod, "p[0-9]{3}_4\\(1t\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(01p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(01p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(11p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(11p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(12p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(12p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(13p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(13p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(13p)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_5"), pcodf),
                  pcodf = ifelse(codf == '_5(13p)' & pcodf == "p400_5", "p400_5(13p)", pcodf),
                  pcodf = ifelse(codf == '_5(13p)' & pcodf == "p500_5", "p500_5(13p)", pcodf),
                  pcodf = ifelse(codf == '_5(13p)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_6"), pcodf),
                  pcodf = ifelse(codf == '_5(13p)' & pcodf == "p451_6", "p400_5(13p)", pcodf),
                  pcodf = ifelse(codf == '_5(13p)' & pcodf == "p541_6", "p500_5(13p)", pcodf),
                  pcodf = ifelse(codf == '_5(14p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(14p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(15p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(15p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(16p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(16p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(17p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(17p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(18p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(18p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(19p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(19p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(20p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(20p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(20p)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_5"), pcodf),
                  pcodf = ifelse(codf == '_5(20p)' & is.na(pcodf), stringr::str_extract(fullpcod, "p[0-9]{3}_6"), pcodf),
                  pcodf = ifelse(codf == '_5(20p)' & pcodf == "p400_5", "p400_5(20p)", pcodf),
                  pcodf = ifelse(codf == '_5(20p)' & pcodf == "p500_5", "p500_5(20p)", pcodf),
                  pcodf = ifelse(codf == '_5(20p)' & pcodf == "p451_6", "p500_5(20p)", pcodf),
                  pcodf = ifelse(codf == '_5(20p)' & pcodf == "p541_6", "p400_5(20p)", pcodf),
                  pcodf = ifelse(codf == '_5(21p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(21p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(22p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(22p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(23p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(23p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(24p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(24p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(25t)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(25t\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(26p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(26p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(27p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(27p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(28p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(28p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(29p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(29p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(30p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(30p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(31p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(31p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(32p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(32p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(33p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(33p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(34p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(34p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(35p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(35p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(36t)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(36t\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(37t)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(37t\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(38t)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(38t\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(41p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(41p\\)"), pcodf),
                  pcodf = ifelse(codf == '_5(43p)', stringr::str_extract(fullpcod, "p[0-9]{3}_5\\(43p\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(10a)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(10a\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(10p)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(10p\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(11a)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(11a\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(11p)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(11p\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(12a)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(12a\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(12p)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(12p\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(14a)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(14a\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(14p)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(14p\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(15a)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(15a\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(15p)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(15p\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(15x)_3(11c)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(11c\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(15x)_3(11c)' & !is.na(pcodf), paste0("_6(15)", pcodf), pcodf),
                  pcodf = ifelse(codf == '_6(15x)_3(11w)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(11w\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(15x)_3(11w)' & !is.na(pcodf), paste0("_6(15)", pcodf), pcodf),
                  pcodf = ifelse(codf == '_6(16a)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(16a\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(16p)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(16p\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(17a)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(17a\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(17p)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(17p\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(19a)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(19a\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(19p)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(19p\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(21a)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(21a\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(21p)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(21p\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(22a)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(22a\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(22p)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(22p\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(22x)_3(11c)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(11c\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(22x)_3(11c)' & !is.na(pcodf), paste0("_6(22)", pcodf), pcodf),
                  pcodf = ifelse(codf == '_6(22x)_3(11w)', stringr::str_extract(fullpcod, "p[0-9]{3}_3\\(11w\\)"), pcodf),
                  pcodf = ifelse(codf == '_6(22x)_3(11w)' & !is.na(pcodf), paste0("_6(22)", pcodf), pcodf),
                  pcodf = ifelse(codf == '_7(6p)', stringr::str_extract(fullpcod, "p[0-9]{3}_6\\(.+?\\)"), pcodf),
                  pcodf = ifelse(codf == '_7(6p)' & stringr::str_detect(pcodf, "p451"), "p501_5(20p)", pcodf),
                  pcodf = ifelse(codf == '_7(6p)' & stringr::str_detect(pcodf, "p541"), "p401_5(20p)", pcodf),
                  codf = ifelse(codf == '_7(6p)', "_5(20p)", codf)) %>% 
    dplyr::mutate(pcodf = ifelse(codf == "_10(2)", stringr::str_replace(pcodf, "_10\\(2\\)", "_10(1)"), pcodf),
                  pcodf = ifelse(codf == "_3(33c)", stringr::str_replace(pcodf, "_3\\(33c\\)", "_3(22c)"), pcodf),
                  pcodf = ifelse(codf == "_3(33w)", stringr::str_replace(pcodf, "_3\\(33w\\)", "_3(22w)"), pcodf),
                  pcodf = ifelse(codf == "_3(61c)", stringr::str_replace(pcodf, "_3\\(61c\\)", "_3(52c)"), pcodf),
                  pcodf = ifelse(codf == "_3(72c)", stringr::str_replace(pcodf, "_3\\(72c\\)", "_3(52c)"), pcodf),
                  pcodf = ifelse(codf == "_3(83c)", stringr::str_replace(pcodf, "_3\\(83c\\)", "_3(52c)"), pcodf),
                  pcodf = ifelse(codf == "_3(72w)", stringr::str_replace(pcodf, "_3\\(72w\\)", "_3(52w)"), pcodf),
                  pcodf = ifelse(codf == "_3(83w)", stringr::str_replace(pcodf, "_3\\(83w\\)", "_3(52w)"), pcodf),
                  pcodf = ifelse(codf == "_4(1t)", stringr::str_replace(pcodf, "_4\\(1t\\)", "_4(0t)"), pcodf),
                  pcodf = ifelse(codf == "_5(01p)", stringr::str_replace(pcodf, "_5\\(01p\\)", "_5(11p)"), pcodf),
                  pcodf = ifelse(codf == "_5(14p)", stringr::str_replace(pcodf, "_5\\(14p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(15p)", stringr::str_replace(pcodf, "_5\\(15p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(16p)", stringr::str_replace(pcodf, "_5\\(16p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(17p)", stringr::str_replace(pcodf, "_5\\(17p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(18p)", stringr::str_replace(pcodf, "_5\\(18p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(21p)", stringr::str_replace(pcodf, "_5\\(21p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(22p)", stringr::str_replace(pcodf, "_5\\(22p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(23p)", stringr::str_replace(pcodf, "_5\\(23p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(24p)", stringr::str_replace(pcodf, "_5\\(24p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(26p)", stringr::str_replace(pcodf, "_5\\(26p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(27p)", stringr::str_replace(pcodf, "_5\\(27p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(28p)", stringr::str_replace(pcodf, "_5\\(28p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(29p)", stringr::str_replace(pcodf, "_5\\(29p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(30p)", stringr::str_replace(pcodf, "_5\\(30p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(33p)", stringr::str_replace(pcodf, "_5\\(33p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(41p)", stringr::str_replace(pcodf, "_5\\(41p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(43p)", stringr::str_replace(pcodf, "_5\\(43p\\)", "_5p"), pcodf),
                  pcodf = ifelse(codf == "_5(25t)", stringr::str_replace(pcodf, "_5\\(25t\\)", "_5t"), pcodf),
                  pcodf = ifelse(codf == "_5(36t)", stringr::str_replace(pcodf, "_5\\(36t\\)", "_5t"), pcodf),
                  pcodf = ifelse(codf == "_5(38t)", stringr::str_replace(pcodf, "_5\\(38t\\)", "_5t"), pcodf),
                  pcodf = ifelse(codf == "_6(10p)", stringr::str_replace(pcodf, "_6\\(10p\\)", "_6(10a)"), pcodf),
                  pcodf = ifelse(codf == "_6(11a)", stringr::str_replace(pcodf, "_6\\(11a\\)", "_6(10a)"), pcodf),
                  pcodf = ifelse(codf == "_6(11p)", stringr::str_replace(pcodf, "_6\\(11p\\)", "_6(10a)"), pcodf),
                  pcodf = ifelse(codf == "_6(12p)", stringr::str_replace(pcodf, "_6\\(12p\\)", "_6(12a)"), pcodf),
                  pcodf = ifelse(codf == "_6(14a)", stringr::str_replace(pcodf, "_6\\(14a\\)", "_6(10a)"), pcodf),
                  pcodf = ifelse(codf == "_6(15a)", stringr::str_replace(pcodf, "_6\\(15a\\)", "_6(10a)"), pcodf),
                  pcodf = ifelse(codf == "_6(16a)", stringr::str_replace(pcodf, "_6\\(16a\\)", "_6(10a)"), pcodf),
                  pcodf = ifelse(codf == "_6(17a)", stringr::str_replace(pcodf, "_6\\(17a\\)", "_6(10a)"), pcodf),
                  pcodf = ifelse(codf == "_6(19a)", stringr::str_replace(pcodf, "_6\\(19a\\)", "_6(10a)"), pcodf),
                  pcodf = ifelse(codf == "_6(21a)", stringr::str_replace(pcodf, "_6\\(21a\\)", "_6(10a)"), pcodf),
                  pcodf = ifelse(codf == "_6(21p)", stringr::str_replace(pcodf, "_6\\(21p\\)", "_6(10a)"), pcodf),
                  pcodf = ifelse(codf == "_6(22a)", stringr::str_replace(pcodf, "_6\\(22a\\)", "_6(10a)"), pcodf))
  
}

tokenizer_plays <- function(df){
  
  obj <- df %>% 
    dplyr::group_by(game_id, period, clock) %>%
    dplyr::mutate(n = dplyr::n(),
                  joinpcod = paste0(pcod, collapse = ""),
                  row = dplyr::row_number()) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(joincod = stringr::str_remove_all(joinpcod, "p[0-9]{3}")) %>% 
    dplyr::mutate(importance = 1)
  
  obj2 <- obj %>% 
    dplyr::group_by(joinpcod, joincod, row) %>% 
    dplyr::summarise(pcod = unique(pcod), 
                     cod = unique(cod), 
                     n = unique(n), 
                     importance = unique(importance), 
                     cnt = dplyr::n(), 
                     .groups = 'drop') %>%
    nulling_cods_always_null() %>% 
    dplyr::mutate(si = 0) %>% 
    put_features_cod() %>% 
    put_nums() %>% 
    dplyr::mutate(cod = ifelse(stringr::str_detect(cod, "_4") & stringr::str_detect(before_cod, "_3\\(.{1,3}\\)$"), stringr::str_replace(cod, "_4", "_14"), cod), 
                  cod = ifelse(stringr::str_detect(cod, "_6\\(0"), stringr::str_replace(cod, "_6", "_16"), cod),
                  cod = ifelse(stringr::str_detect(cod, "_3\\(40"), stringr::str_replace(cod, "_3", "_33"), cod)) %>% 
    dplyr::group_by(joinpcod) %>% 
    dplyr::mutate(joincod = paste0(cod, collapse = '')) %>% 
    dplyr::ungroup() %>% 
    put_features_cod() %>% 
    dplyr::mutate(erro = ifelse(modcod %in% c('_3_3_2_4_6', '_3_4_6_3', '_4_2_4_6_3_4_3', '_6_2_4_3_3', '_6_3_10_3', 
                                              '_6_3_3_4', '_6_3_3_4_2', '_6_4_3_3', '_2_4_6_1_3', '_6_1_3', 
                                              '_12_16_33_10', '_6_3_16_33_10_3', '_33_6_16', '_6_1_16_33_3',
                                              '_2_10_10_4', '_3_3_6_6_3_3', '_6_3_6_3', '_16_16_2_4_33_33', 
                                              '_6_16_16_33_33_3_6_3_3_3', '_3_3_3_4'), 1, 0))
  obj3 <- obj2 %>% 
    dplyr::mutate(i = 0,
                  importance = ifelse(cod == "_10(0)", 0, importance),
                  importance = ifelse(stringr::str_detect(cod, "_5\\((00|13)p\\)"), 0, importance),
                  importance = ifelse(nums %in% c("01100000000000000","02310000000000000","11000000000000000","11000000100000000","11000000200000000","11000001000000000","11000001000010000",
                                                  "11000001100000000","11000010000000000","11000100000000000","11001100000010000","11110000000000000","11110000000000001","11110001000000000",
                                                  "11200000000000000","11220000000000000","11220000000001000","11330000000001000","11510000000000000","12000000000000000","12000000000000010",
                                                  "12000000000010000","12000000010000000","12000000100000000","12000000200000000","12000001000000000","12000010000000000","12000100000000000",
                                                  "12001100000010000","12010000000000000","12100000000000000","12110000000000000","12110000000001000","12110000000010001","12110010000000000",
                                                  "12111100000000000","12111100000010000","12111100010010000","12200000000000000","12210000000000000","12220000000000000","12220000000001000",
                                                  "12310000000000000","12330000000000000","12330000000001000","12330000000002000","12410000000000000","13000000000000000","13000001000000000",
                                                  "13100000000000000","13100000000001000","13110000000000000","13110000000001000","13220000000000000","13220000000001000","13330000000000000",
                                                  "13400000000000000","21000000000000000","21000000000001000","21000000100000000","21000001000000000","21001100000000000","21001100000001000",
                                                  "21001100200001000","21001101000000000","21101100000001000","21110000000000000","21110000000001000","22000000000000000","22000000000001000",
                                                  "22000000000010000","22000000000011000","22001100000000000","22001100000001000","22001100000010000","22002200000010000","22110000000000000",
                                                  "22110000000001000","22111100000000000","22111100000010000","22112200000000000","22130000000000000","22210000000000000","22220000000002000",
                                                  "22221100000001000","23000000000000000","23000000000001000","23000001000001000","23001100000000000","23001100000001000","23001100000011000",
                                                  "23110000000001000","23111100000001000","23220000000001000","24000000000000000","24000000000001000","24001100000010000","24110000000001000",
                                                  "24110001000000000","24220000000001000","25000000000001000","25110000000001000","31001100000001000","32000000000000000","32331100000001000",
                                                  "33000000000001000","33001100000001000","33001100000011000","34000000000002000",
                                                  "36000000000002000") & stringr::str_detect(cod, "_3"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00010000000000000","00110000000000000","00110000000000001","00110000000001000","00110000000010000","00110000000010001","00110000000100000",
                                                  "00110000001000000","00110000001001000","00110001000000000","00110010000000000","00220000000000000","00220000000001000","00220000001000000",
                                                  "00220000001001000","00220000001002000","00220001000000000","00220010000000000","00220010000001000","00310000000000000","00330000000000000",
                                                  "00330000000001000","00330000000002000","00420000000000000","00440000000001000","00510000000000000","00620000000000000","01000000000000000",
                                                  "01100000000000000","02000000000000000","02310000000000000","03000000000000000","10110001000000000","10111100000010001","10220000000000000",
                                                  "10220000000001000","10221100000000000","10221100000001000","10311100000000000","10330000000000000","10331100000002000","10440000000000000",
                                                  "10440000000001000","11000000000000000","11000000000001000","11000000100000000","11000000200000000","11000001000000000","11000001000010000",
                                                  "11000001100000000","11000010000000000","11000100000000000","11001100000010000","11110000000000000","11110000000000001","11110001000000000",
                                                  "11220000000000000","11220000000001000","11330000000001000","11510000000000000","12000000000000000","12000000000000010","12000000000001000", 
                                                  "12000000000010000","12000000010000000","12000000100000000","12000000200000000","12000001000000000","12000010000000000","12000100000000000",
                                                  "12001100000010000","12010000000000000","12100000000000000","12110000000000000","12110000000001000","12110000000010001","12110010000000000",
                                                  "12111100000000000","12111100000010000","12111100010010000","12200000000000000","12210000000000000","12220000000000000","12220000000001000",
                                                  "12310000000000000","12330000000000000","12330000000001000","12330000000002000","12410000000000000","13000000000000000","13000000000001000",
                                                  "13000001000000000","13100000000000000","13100000000001000","13110000000000000","13110000000001000","13220000000000000","13220000000001000",
                                                  "13330000000000000","13400000000000000","20110000000000000","20111100000000000","20112200000000000","21000000000000000","21000000000001000",
                                                  "21000000100000000","21000001000000000","21001100000000000","21001100000001000","21001100200001000","21001101000000000","21101100000001000",
                                                  "21110000000000000","21110000000001000","22000000000000000","22000000000001000","22000000000010000","22000000000011000","22001100000000000",
                                                  "22001100000001000","22001100000010000","22002200000010000","22110000000000000","22110000000001000","22111100000000000","22111100000010000",
                                                  "22112200000000000","22130000000000000","22210000000000000","22220000000002000","22221100000001000","23000000000000000","23000000000001000",
                                                  "23000001000001000","23001100000000000","23001100000001000","23001100000011000","23110000000001000","23111100000001000","23220000000001000",
                                                  "24000000000000000","24000000000001000","24001100000010000","24110000000001000","24110001000000000","24220000000001000","25000000000001000",
                                                  "25110000000001000","31001100000001000","32000000000000000","32331100000001000","33000000000001000","33001100000001000","33001100000011000",
                                                  "34000000000002000","36000000000002000") & stringr::str_detect(cod, "_14"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00110000000000000","00110000000000001","00110000000010000","00110000000010001","00110000000100000","00110000001000000","00110000001001000",
                                                  "00110001000000000","00110010000000000","00220000000000000","00220000000001000","00220000001000000","00220000001001000","00220000001002000",
                                                  "00220001000000000","00220010000000000","00220010000001000","00310000000000000","00330000000000000","00330000000001000","00330000000002000",
                                                  "00420000000000000","00440000000001000","00510000000000000","00620000000000000","02310000000000000","10110000000000000","10110001000000000",
                                                  "10111100000000000","10111100000010001","10220000000000000","10220000000001000","10221100000000000","10221100000001000","10311100000000000",
                                                  "10330000000000000","10331100000002000","10440000000000000","10440000000001000","11110000000000000","11110000000000001","11110001000000000",
                                                  "11220000000000000","11330000000001000","11510000000000000","12010000000000000","12110000000000000","12110000000001000","12110000000010001",
                                                  "12110010000000000","12111100000000000","12111100000010000","12111100010010000","12210000000000000","12220000000000000","12220000000001000",
                                                  "12310000000000000","12330000000000000","12330000000001000","12330000000002000","12410000000000000","13100000000001000","13110000000000000",
                                                  "13110000000001000","13220000000000000","13220000000001000","13330000000000000","20110000000000000","20111100000000000","20112200000000000",
                                                  "21110000000000000","21110000000001000","22110000000000000","22110000000001000","22111100000000000","22111100000010000","22112200000000000",
                                                  "22130000000000000","22210000000000000","22220000000002000","22221100000001000","23110000000001000","23111100000001000","23220000000001000",
                                                  "24110000000001000","24110001000000000","24220000000001000","25110000000001000",
                                                  "32331100000001000") & stringr::str_detect(cod, "_33"), 0, importance),
                  
                  importance = ifelse(nums %in% c("10101100000000000","10110001000000000","11110000000000000","11110000000000001","11220000000000000","11220000000001000","11330000000001000",
                                                  "11510000000000000","12100000000000000","12100000000001000","12110000000000000","12110000000001000","12110010000000000","12111100000000000",
                                                  "12111100000010000","12111100010010000","12210000000000000","12220000000000000","12220000000001000","12310000000000000","12330000000000000",
                                                  "12330000000001000","12330000000002000","12410000000000000","13100000000000000","13100000000001000","13110000000000000","13110000000001000",
                                                  "13220000000000000","13220000000001000","13330000000000000","21101100000001000","21110000000000000","21110000000001000","22110000000000000",
                                                  "22110000000001000","22112200000000000","22130000000000000","22210000000000000","23110000000001000","23111100000001000","23220000000001000",
                                                  "24110000000001000","24110001000000000","24220000000001000","25110000000001000",
                                                  "32331100000001000") & stringr::str_detect(modcod, "_6(.+)?_16") & stringr::str_detect(cod, "_16"), 0, importance),
                  
                  importance = ifelse(nums %in% c("10220000000000000","10221100000000000","10221100000001000","10440000000001000","11110000000000000","11110000000000001","11220000000000000",
                                                  "11220000000001000","11330000000001000","11510000000000000","12100000000000000","12110000000000000","12110000000001000","12110010000000000",
                                                  "12111100000000000","12111100000010000","12111100010010000","12210000000000000","12220000000000000","12220000000001000","12310000000000000",
                                                  "12330000000000000","12330000000001000","12330000000002000","12410000000000000","13100000000001000","13110000000000000","13110000000001000",
                                                  "13220000000000000","13220000000001000",
                                                  "24220000000001000") & stringr::str_detect(modcod, "_16(.+)?_6") & stringr::str_detect(cod, "_6"), 0, importance),
                  
                  importance = ifelse(nums %in% c("10220000000000000","10221100000000000","10221100000001000","10440000000001000","11220000000000000","11220000000001000","11330000000001000",
                                                  "12210000000000000","12220000000000000","12220000000001000","12310000000000000","12330000000000000","12330000000001000","12330000000002000",
                                                  "12410000000000000","13220000000000000","13220000000001000",
                                                  "21110000000000000") & stringr::str_detect(modcod, "_16(.+)?_6") & stringr::str_detect(cod, "_16") & stringr::str_detect(before_cod, "_16"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00000000010000000","00000010010000000","10000000010000000","12000000010000000",
                                                  "12111100010010000") & stringr::str_detect(last_cod, "_10\\(.\\)") & stringr::str_detect(cod, "_4"), 0, importance),
                  
                  importance = ifelse(nums %in% c("11000001000000000","11000001000010000","11000001100000000","11110001000000000","12000001000000000","13000001000000000","21000001000000000",
                                                  "23000001000001000","24110001000000000") & stringr::str_detect(cod, "_5\\((19|20)p\\)") & stringr::str_detect(modcod, "_3_5"), 0, importance),
                  
                  importance = ifelse(nums %in% c("10000001000000000","10000001100000000","11000001000010000","11000001100000000","11110001000000000","21000001000000000",
                                                  "21001101000000000") & stringr::str_detect(cod, "_5\\((19|20)p\\)") & stringr::str_detect(modcod, "_(6|10)_5"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00110001000000000","00220001000000000",
                                                  "10110001000000000") & stringr::str_detect(cod, "_5\\((19|20)p\\)") & stringr::str_detect(modcod, "_(33|16)_5"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00110000000001000","00220000000000000","00220000000001000","00220001000000000","00220010000000000","00220010000001000","00310000000000000",
                                                  "00330000000000000","00330000000001000","00330000000002000","00420000000000000","00440000000001000","00510000000000000","00620000000000000",
                                                  "02310000000000000","10220000000000000","10220000000001000","10221100000000000","10221100000001000","10311100000000000","10330000000000000",
                                                  "10331100000002000","10440000000000000","10440000000001000","22220000000002000",
                                                  "22221100000001000") & stringr::str_detect(cod, "_16") & stringr::str_detect(last_cod, "_16|_33|_14"), 0, importance),
                  
                  importance = ifelse(nums %in% c("10000100000000000","11000000000001000","12000000000001000","13000000000001000") & stringr::str_detect(cod, "_6"), 0, importance),
                  
                  importance = ifelse(nums %in% c("02000000000000000","03000000000000000","12000000000001000","12100000000001000",
                                                  "13000000000001000") & stringr::str_detect(cod, "_3") & stringr::str_detect(before_cod, "_3"), 0, importance),
                  
                  importance = ifelse(nums %in% c("22001100000001000") & stringr::str_detect(cod, "_6") & modcod %in% c('_3_3_6_5_6') & stringr::str_detect(last_cod, "_3"), 0, importance),
                  importance = ifelse(nums %in% c("22001100000001000") & stringr::str_detect(cod, "_3") & modcod %in% c('_3_3_6_5_6') & last_cod == "", 1, importance),
                  
                  importance = ifelse(nums %in% c("00010000001000000","00100000000000000","00110000000100000","00110000001000000","00110000001001000","00200000000000000","00220000001000000",
                                                  "00220000001001000","00220000001002000","00400000000000000","10200000000000000","10201100000000000","10400000000000000","11200000000000000",
                                                  "12200000000000000","13400000000000000") & stringr::str_detect(cod, "_16"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00000010000000000","00000010010000000","00110010000000000","00220010000000000","00220010000001000","10000010000000000","10001110000000000",
                                                  "11000010000000000","12000010000000000",
                                                  "12110010000000000") & stringr::str_detect(cod, "_5\\(37") & stringr::str_detect(last_cod, '_(2|4)'), 0, importance),
                  
                  importance = ifelse(nums %in% c("00000010000000000","00000010010000000","00110010000000000","00220010000000000","00220010000001000","10000010000000000","10001110000000000",
                                                  "11000010000000000","12000010000000000",
                                                  "12110010000000000") & stringr::str_detect(cod, "_4\\(") & stringr::str_detect(last_cod, '_2') & stringr::str_detect(next_cod, "_5\\(37"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00110000000000001","00110000000010001","10111100000010001","11110000000000001",
                                                  "12110000000010001") & stringr::str_detect(cod, "_5\\((39|40)"), 0, importance),
                  
                  importance = ifelse(nums %in% c("12000000000010000","22000000000010000","22000000000011000") & stringr::str_detect(cod, "_5") & stringr::str_detect(last_cod, "_(6|3)"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00110000000010000") & stringr::str_detect(cod, "_5") & stringr::str_detect(last_cod, "_(16|33)"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00000100000000000","10000100000000000") & stringr::str_detect(cod, "_5\\((00|13)"), 1, importance),
                  
                  importance = ifelse(nums %in% c("22130000000000000","24000000000000000",
                                                  "24001100000010000") & stringr::str_detect(cod, "_6") & stringr::str_detect(last_cod, "_6"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00000000000000100") & stringr::str_detect(cod, "_10") & stringr::str_detect(last_cod, "_5\\(21"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00000000000000010","12000000000000010") & stringr::str_detect(cod, "_5\\(26"), 0, importance),
                  
                  importance = ifelse(nums %in% c("00000000000100000","00100000000100000") & stringr::str_detect(before_cod, "_13\\("), 0, importance),
                  
                  importance = ifelse(nums %in% c("11110001000000000") & stringr::str_detect(cod, "_16\\(") & stringr::str_detect(last_cod, "_6"), 0, importance),
                  
                  importance = ifelse(nums %in% c("22111100000010000") & stringr::str_detect(cod, "_6\\(") & stringr::str_detect(last_cod, "_16"), 0, importance),
                  
                  importance = ifelse(nums %in% c("12000100000000000") & stringr::str_detect(cod, "_6\\(") & stringr::str_detect(last_cod, "_5\\(13p"), 0, importance),
                  importance = ifelse(nums %in% c("12000100000000000") & stringr::str_detect(cod, "_5\\(13p"), 1, importance),
                  
                  importance = ifelse(nums %in% c("12100000000001000") & stringr::str_detect(cod, "_6") & stringr::str_detect(last_cod, '_3\\('), 0, importance),
                  
                  importance = ifelse(nums %in% c("00010000001000000","00110000001001000","00220000001000000","00220000001001000",
                                                  "10000000001000000") & stringr::str_detect(after_cod, "_12"), 0, importance),
                  
                  i = ifelse(importance == 1, 1, i))  %>% 
    dplyr::group_by(joinpcod, joincod) %>% 
    dplyr::mutate(si = cumsum(i)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(si = ifelse(si == 0, 1, si)) %>% 
    dplyr::mutate(importance2 = importance) %>% 
    dplyr::mutate(importance = 1) %>%
    nulling_cods_always_null() %>% 
    dplyr::mutate(importance = ifelse(cod == "_7(4p)" & stringr::str_detect(joincod, "_10\\(", neg = T), 1, importance),
                  importance = ifelse(cod == "_7(6p)", 1, importance)) %>% 
    put_features_cod() %>% 
    dplyr::mutate(importance = importance2) %>% 
    dplyr::select(-importance2) %>% 
    dplyr::mutate(i = ifelse(stringr::str_detect(cod, "_14\\(") & after_cod %in% c("", "_16(01p)"), 1, i)) %>% 
    dplyr::group_by(joinpcod, joincod) %>% 
    dplyr::mutate(si = cumsum(i)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(si = ifelse(si == 0, 1, si)) %>% 
    dplyr::mutate(si = ifelse(joincod == "_6(11p)_6(19p)_9(0)_18(0)_3(12c)_3(22c)_3(61c)_8(0)", 1, si)) %>% 
    group_plays_around_jumpball() %>% 
    group_plays_around_fouls() %>% 
    group_plays_around_turnovers()
  
  obj4 <- obj3 %>% 
    dplyr::mutate(importance = 1) %>%
    nulling_cods_always_null() %>% 
    dplyr::mutate(importance = ifelse(cod == "_7(4p)" & stringr::str_detect(joincod, "_10\\(", neg = T), 1, importance),
                  importance = ifelse(cod == "_7(6p)", 1, importance)) %>% 
    put_features_cod() %>% 
    put_nums() %>% 
    dplyr::group_by(joinpcod, joincod, si) %>% 
    dplyr::mutate(fullpcod = paste0(pcod, collapse = "")) %>% 
    dplyr::ungroup() %>% 
    put_codf() %>% 
    dplyr::mutate(erro = ifelse(valcod %in% c("_3(23c)", "_3(33c)"), 1, erro)) %>% 
    dplyr::select(joinpcod, row, si, codf, pcodf, erro) %>% 
    dplyr::filter(erro == 0)
  
  obj5 <- obj %>% 
    dplyr::left_join(obj4, by = c('joinpcod', 'row')) %>% 
    dplyr::select(-c(n, joinpcod, row, joincod, importance))
  
  obj5
  
}

FOLDER_PROCESSED_DATA <- "D:/Mestrado/NBA/nba/data/processed/"

load(paste0(FOLDER_PROCESSED_DATA, "pbp.RData"))

pbp_tokenized <- tokenizer_plays(pbp)

save(pbp_tokenized, file = paste0(FOLDER_PROCESSED_DATA, "pbp_tokenized.RData"))

pbp_moments <- pbp_tokenized %>% 
  dplyr::group_by(season, game_id, period, 
                  clock, si, pcodf, codf) %>% 
  dplyr::summarise(pcod = paste0(pcod, collapse = ''), 
                   .groups = 'drop') %>% 
  dplyr::arrange(game_id, period, desc(clock), si)

save(pbp_moments, file = paste0(FOLDER_PROCESSED_DATA, "pbp_moments.RData"))

# se há ejeção antes das faltas, devemos mudar isso? caso: 0041800125      4 01:50:00
# em algumas jogadas mais confusas, há varias interpretações do que aconteceu, caso: 0021500784      4 09:35:00
# as bases parecem ser igualmente incosistentes, para o caso:0020800075      2 02:16:00, nba melhor que br
# se for 10(.)_5(13p) ou 6(18a)_5(13p) _5(13p) tem que ter importancia 0 
# as vezes uma ação aconteceu antes, mas so é registrada no instante seguinte do relogio. caso 0021300960      2 09:44:00
# _5(41p) atleta de ataque fica 5 segundos de costas para a cesta. Pode ser juntado com outro tipo de turnover? 
# _5(43p) representa um turnover de opposite basket, quando um jogador marca a cesta contra o seu proprio time, geralmente é seguido por um tip shot

