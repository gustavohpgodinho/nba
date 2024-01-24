require(tidyverse)

set_ind <- function(df){
  
  df %>% 
    dplyr::mutate(ind = 0) %>% 
    dplyr::mutate(description = ifelse(is.na(description), "", description)) %>% 
    dplyr::mutate(ind = ifelse(stringr::str_detect(description, 'BLOCK'), ind + 1, ind),
                  ind = ifelse(stringr::str_detect(description, 'AST'), ind + 1, ind),
                  ind = ifelse(stringr::str_detect(description, "3PT"), ind + 10, ind),
                  ind = ifelse(tp_event == 3 & stringr::str_detect(description, "^MISS"), ind + 5, ind),
                  ind = ifelse(stringr::str_detect(description, 'STEAL'), ind + 1, ind),
                  ind = ifelse(stringr::str_detect(description, '\\(P[0-9]+\\)'), ind + 10, ind),
                  ind = ifelse(stringr::str_detect(description, 'P[0-9]+\\.PN'), ind + 3, ind),
                  ind = ifelse(stringr::str_detect(description, 'P[0-9]+\\.T[0-9]+'), ind + 6, ind),
                  ind = ifelse(stringr::str_detect(description, 'T\\#'), ind + 9, ind),
                  ind = ifelse(clock == "12:00:00" & period == 1 & tp_event == 10, ind + 1, ind),
                  ind = ifelse(clock == "05:00:00" & period >= 5 & tp_event == 10, ind + 2, ind),
                  ind = ifelse(tp_event %in% c(12, 13), period + 10, ind)) %>% 
    dplyr::mutate(ind = ifelse(tp_event %in% c(12, 13) & ind > 14, 15, ind))
}

create_columns_pre_process <- function(df){
  
  
  df %>% 
    dplyr::mutate(p = stringr::str_replace_all(persons, '1', 'r'),
                  p = stringr::str_replace_all(p, '2|3', 't'),
                  p = stringr::str_replace_all(p, '4|5', 'p'),
                  p = stringr::str_replace_all(p, '6|7', 'c')) %>% 
    dplyr::mutate(p = ifelse(tp_event %in% c(5, 7, 18), stringr::str_sub(p, 1, 2), p)) %>% 
    dplyr::mutate(tp = ifelse(tp_event > 2, tp_subevent, "")) %>% 
    dplyr::mutate(tp = ifelse(tp_event == 1 & ind == 10 & persons %in% c('400', '500'), "5", tp),
                  tp = ifelse(tp_event == 1 & ind == 11 & persons %in% c('440', '550'), "51", tp),
                  tp = ifelse(tp_event == 1 & ind == 0 & stringr::str_detect(tp_subaction, "Dunk"), "1", tp),
                  tp = ifelse(tp_event == 1 & ind == 1 & stringr::str_detect(tp_subaction, "Dunk"), "11", tp),
                  tp = ifelse(tp_event == 1 & ind == 0 & stringr::str_detect(tp_subaction, "Hook"), "2", tp),
                  tp = ifelse(tp_event == 1 & ind == 1 & stringr::str_detect(tp_subaction, "Hook"), "21", tp),
                  tp = ifelse(tp_event == 1 & ind == 0 & stringr::str_detect(tp_subaction, "Layup"), "3", tp),
                  tp = ifelse(tp_event == 1 & ind == 1 & stringr::str_detect(tp_subaction, "Layup"), "31", tp),
                  tp = ifelse(tp_event == 1 & ind == 0 & stringr::str_detect(tp_subaction, "Jump") & tp == "", "4", tp),
                  tp = ifelse(tp_event == 1 & ind == 1 & stringr::str_detect(tp_subaction, "Jump") & tp == "", "41", tp), 
                  tp = ifelse(tp_event == 1 & ind == 0 & stringr::str_detect(tp_subaction, "Tip") & tp == "", "3", tp),
                  tp = ifelse(tp_event == 1 & ind == 1 & stringr::str_detect(tp_subaction, "Tip") & tp == "", "31", tp),
                  tp = ifelse(tp_event == 1 & ind == 0 & stringr::str_detect(tp_subaction, "Bank") & tp == "", "4", tp),
                  tp = ifelse(tp_event == 1 & ind == 1 & stringr::str_detect(tp_subaction, "Bank") & tp == "", "41", tp),
                  tp = ifelse(tp_event == 1 & ind == 0 & stringr::str_detect(tp_subaction, "Fadeaway") & tp == "", "4", tp),
                  tp = ifelse(tp_event == 1 & ind == 1 & stringr::str_detect(tp_subaction, "Fadeaway") & tp == "", "41", tp),
                  tp = ifelse(tp_event == 1 & ind == 0 & tp == "", "4", tp),
                  tp = ifelse(tp_event == 1 & ind == 1 & tp == "", "41", tp),
                  tp = ifelse(tp_event == 2 & ind == 10 & persons %in% c('400', '500'), "5", tp),
                  tp = ifelse(tp_event == 2 & ind == 11 & persons %in% c('405', '504', '501'), "52", tp),
                  tp = ifelse(tp_event == 2 & ind == 0 & stringr::str_detect(tp_subaction, "Dunk"), "1", tp),
                  tp = ifelse(tp_event == 2 & ind == 1 & stringr::str_detect(tp_subaction, "Dunk"), "12", tp),
                  tp = ifelse(tp_event == 2 & ind == 0 & stringr::str_detect(tp_subaction, "Hook"), "2", tp),
                  tp = ifelse(tp_event == 2 & ind == 1 & stringr::str_detect(tp_subaction, "Hook"), "22", tp),
                  tp = ifelse(tp_event == 2 & ind == 0 & stringr::str_detect(tp_subaction, "Layup"), "3", tp),
                  tp = ifelse(tp_event == 2 & ind == 1 & stringr::str_detect(tp_subaction, "Layup"), "32", tp),
                  tp = ifelse(tp_event == 2 & ind == 0 & stringr::str_detect(tp_subaction, "Jump") & tp == "", "4", tp),
                  tp = ifelse(tp_event == 2 & ind == 1 & stringr::str_detect(tp_subaction, "Jump") & tp == "", "42", tp), 
                  tp = ifelse(tp_event == 2 & ind == 0 & stringr::str_detect(tp_subaction, "Tip") & tp == "", "3", tp),
                  tp = ifelse(tp_event == 2 & ind == 1 & stringr::str_detect(tp_subaction, "Tip") & tp == "", "32", tp),
                  tp = ifelse(tp_event == 2 & ind == 0 & stringr::str_detect(tp_subaction, "Bank") & tp == "", "4", tp),
                  tp = ifelse(tp_event == 2 & ind == 1 & stringr::str_detect(tp_subaction, "Bank") & tp == "", "42", tp),
                  tp = ifelse(tp_event == 2 & ind == 0 & stringr::str_detect(tp_subaction, "Fadeaway") & tp == "", "4", tp),
                  tp = ifelse(tp_event == 2 & ind == 1 & stringr::str_detect(tp_subaction, "Fadeaway") & tp == "", "42", tp),
                  tp = ifelse(tp_event == 2 & ind == 0 & tp == "", "4", tp),
                  tp = ifelse(tp_event == 2 & ind == 1 & tp == "", "42", tp))
  
}

set_cod <- function(df){
  
  # Turnover: Bad Pass (without STEAL) and Out of Bounds - Bad Pass Turnover are grouped at 5(34p)
  # Fouls: Offensive and Offensive Charge are grouped together at 6(13)
  # Tech Fouls: Non-Unsportsmanlike Tech and Hanging Tech are grouped with Tech at 6(01)

  df %>% 
    dplyr::mutate(cod = "") %>% 
    dplyr::mutate(cod = ifelse(tp_event == 1 & tp == '1', "_1(1)", cod),
                  cod = ifelse(tp_event == 1 & tp == '11', "_1(1a)", cod),
                  cod = ifelse(tp_event == 1 & tp == '2', "_1(2)", cod),
                  cod = ifelse(tp_event == 1 & tp == '21', "_1(2a)", cod),
                  cod = ifelse(tp_event == 1 & tp == '3', "_1(3)", cod),
                  cod = ifelse(tp_event == 1 & tp == '31', "_1(3a)", cod),
                  cod = ifelse(tp_event == 1 & tp == '4', "_1(4)", cod),
                  cod = ifelse(tp_event == 1 & tp == '41', "_1(4a)", cod), 
                  cod = ifelse(tp_event == 1 & tp == '5', "_1(5)", cod),
                  cod = ifelse(tp_event == 1 & tp == '51', "_1(5a)", cod), 
                  cod = ifelse(tp_event == 1 & is.na(tp) & ind == 0, '_1(4)', cod),
                  cod = ifelse(tp_event == 1 & is.na(tp) & ind == 1, '_1(4a)', cod),
                  
                  cod = ifelse(tp_event == 2 & tp == '1', "_2(1)", cod),
                  cod = ifelse(tp_event == 2 & tp == '12', "_2(1b)", cod),
                  cod = ifelse(tp_event == 2 & tp == '2', "_2(2)", cod),
                  cod = ifelse(tp_event == 2 & tp == '22', "_2(2b)", cod),
                  cod = ifelse(tp_event == 2 & tp == '3', "_2(3)", cod),
                  cod = ifelse(tp_event == 2 & tp == '32', "_2(3b)", cod),
                  cod = ifelse(tp_event == 2 & tp == '4', "_2(4)", cod),
                  cod = ifelse(tp_event == 2 & tp == '42', "_2(4b)", cod), 
                  cod = ifelse(tp_event == 2 & tp == '5', "_2(5)", cod),
                  cod = ifelse(tp_event == 2 & tp == '52', "_2(5b)", cod), 
                  cod = ifelse(tp_event == 2 & is.na(tp) & ind == 0, '_2(4)', cod),
                  cod = ifelse(tp_event == 2 & is.na(tp) & ind == 1, '_2(4b)', cod),
                  
                  cod = ifelse(tp_event == 3 & tp == '10' & ind == 0, "_3(11c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '10' & ind == 5, "_3(11w)", cod),
                  cod = ifelse(tp_event == 3 & tp == '11' & ind == 0, "_3(12c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '11' & ind == 5, "_3(12w)", cod),
                  cod = ifelse(tp_event == 3 & tp == '12' & ind == 0, "_3(22c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '12' & ind == 5, "_3(22w)", cod),
                  cod = ifelse(tp_event == 3 & tp == '13' & ind == 0, "_3(13c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '13' & ind == 5, "_3(13w)", cod),
                  cod = ifelse(tp_event == 3 & tp == '14' & ind == 0, "_3(23c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '14' & ind == 5, "_3(23w)", cod),
                  cod = ifelse(tp_event == 3 & tp == '15' & ind == 0, "_3(33c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '15' & ind == 5, "_3(33w)", cod),
                  cod = ifelse(tp_event == 3 & tp == '16' & ind == 0, "_3(40c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '16' & ind == 5, "_3(40w)", cod),
                  
                  cod = ifelse(tp_event == 3 & tp == '21' & ind == 0, "_3(40c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '21' & ind == 5, "_3(40w)", cod),
                  cod = ifelse(tp_event == 3 & tp == '22' & ind == 0, "_3(40c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '22' & ind == 5, "_3(40w)", cod),
                  
                  cod = ifelse(tp_event == 3 & tp == '18' & ind == 0, "_3(51c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '18' & ind == 5, "_3(51w)", cod),
                  cod = ifelse(tp_event == 3 & tp == '19' & ind == 0, "_3(52c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '19' & ind == 5, "_3(52w)", cod),
                  cod = ifelse(tp_event == 3 & tp == '20' & ind == 0, "_3(61c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '20' & ind == 5, "_3(61w)", cod),
                  
                  cod = ifelse(tp_event == 3 & tp == '25' & ind == 0, "_3(71c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '25' & ind == 5, "_3(71w)", cod),
                  cod = ifelse(tp_event == 3 & tp == '26' & ind == 0, "_3(72c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '26' & ind == 5, "_3(72w)", cod),
                  cod = ifelse(tp_event == 3 & tp == '27' & ind == 0, "_3(81c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '27' & ind == 5, "_3(81w)", cod), 
                  cod = ifelse(tp_event == 3 & tp == '28' & ind == 0, "_3(82c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '28' & ind == 5, "_3(82w)", cod), 
                  cod = ifelse(tp_event == 3 & tp == '29' & ind == 0, "_3(83c)", cod),
                  cod = ifelse(tp_event == 3 & tp == '29' & ind == 5, "_3(83w)", cod),
                  
                  cod = ifelse(tp_event == 4 & tp == '0' & p == 't00', "_4(0t)", cod),
                  cod = ifelse(tp_event == 4 & tp == '0' & p == 'p00', "_4(0p)", cod),
                  cod = ifelse(tp_event == 4 & tp == '1' & p == 't00', "_4(1t)", cod),
                  
                  cod = ifelse(tp_event == 5 & tp == '42' & p == '00' & ind == 0, "_5(39u)", cod),
                  cod = ifelse(tp_event == 5 & tp == '42' & p == 't0' & ind == 0, "_5(39t)", cod),
                  cod = ifelse(tp_event == 5 & tp == '44' & p == '00' & ind == 0, "_5(40u)", cod),
                  
                  cod = ifelse(tp_event == 5 & tp == '37' & p == 'p0' & ind == 6, "_5(13p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '45' & p == 'p0' & ind == 6, "_5(34p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '4' & p == 'p0' & ind == 6, "_5(12p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '40' & p == 'p0' & ind == 6, "_5(32p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '39' & p == 'p0' & ind == 6, "_5(31p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '8' & p == 'p0' & ind == 6, "_5(16p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '13' & p == 'p0' & ind == 6, "_5(18p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '15' & p == 'p0' & ind == 6, "_5(19p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '1' & p == 'p0' & ind == 6, "_5(34p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '2' & p == 'p0' & ind == 6, "_5(33p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '21' & p == 'p0' & ind == 6, "_5(24p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '6' & p == 'p0' & ind == 6, "_5(14p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '7' & p == 'p0' & ind == 6, "_5(15p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '0' & p == 'p0' & ind == 6, "_5(00p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '12' & p == 'p0' & ind == 6, "_5(17p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '19' & p == 'p0' & ind == 6, "_5(22p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '17' & p == 'p0' & ind == 6, "_5(20p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '20' & p == 'p0' & ind == 6, "_5(23p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '18' & p == 'p0' & ind == 6, "_5(21p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '36' & p == 'p0' & ind == 6, "_5(30p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '35' & p == 'p0' & ind == 6, "_5(29p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '9' & p == 'p0' & ind == 6, "_5(41p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '33' & p == 'p0' & ind == 6, "_5(27p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '34' & p == 'p0' & ind == 6, "_5(28p)", cod),
                  
                  cod = ifelse(tp_event == 5 & tp == '1' & p == 'pp' & ind == 7, "_5(11p)", cod),
                  cod = ifelse(tp_event == 5 & tp == '2' & p == 'pp' & ind == 7, "_5(35p)", cod),
                  
                  cod = ifelse(tp_event == 5 & tp == '11' & p == 't0' & ind == 9, "_5(37t)", cod),
                  cod = ifelse(tp_event == 5 & tp == '9' & p == 't0' & ind == 9, "_5(38t)", cod),
                  cod = ifelse(tp_event == 5 & tp == '10' & p == 't0' & ind == 9, "_5(36t)", cod),
                  cod = ifelse(tp_event == 5 & tp == '44' & p == 't0' & ind == 9, "_5(40t)", cod),
                  
                  cod = ifelse(tp_event == 6 & tp == '17' & p == 'p0r' & ind == 0, "_6(05p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '11' & p == 'c0r' & ind == 0, "_6(01c)", cod),
                  cod = ifelse(tp_event == 6 & tp == '16' & p == 'ppr' & ind == 0, "_6(4pp)", cod),
                  cod = ifelse(tp_event == 6 & tp == '18' & p == 't0r' & ind == 0, "_6(06t)", cod),
                  cod = ifelse(tp_event == 6 & tp == '10' & p == 'ppr' & ind == 0, "_6(18a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '11' & p == 't0r' & ind == 0, "_6(01t)", cod),
                  cod = ifelse(tp_event == 6 & tp == '18' & p == '00r' & ind == 0, "_6(06u)", cod),
                  cod = ifelse(tp_event == 6 & tp == '25' & p == 't0r' & ind == 0, "_6(08t)", cod),
                  cod = ifelse(tp_event == 6 & tp == '30' & p == 't0r' & ind == 0, "_6(09t)", cod),
                  cod = ifelse(tp_event == 6 & tp == '11' & p == '00r' & ind == 0, "_6(01u)", cod),
                  cod = ifelse(tp_event == 6 & tp == '16' & p == 'cpr' & ind == 0, "_6(4pc)", cod),
                  cod = ifelse(tp_event == 6 & tp == '12' & p == 'c0r' & ind == 0, "_6(01c)", cod),
                  cod = ifelse(tp_event == 6 & tp == '16' & p == 'ccr' & ind == 0, "_6(4cc)", cod),
                  cod = ifelse(tp_event == 6 & tp == '25' & p == '00r' & ind == 0, "_6(08u)", cod),
                  cod = ifelse(tp_event == 6 & tp == '10' & p == 'p0r' & ind == 0, "_6(18a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '16' & p == 'p0r' & ind == 0, "_6(4pp)", cod),
                  cod = ifelse(tp_event == 6 & tp == '16' & p == 'pcr' & ind == 0, "_6(4pc)", cod),
                  cod = ifelse(tp_event == 6 & tp == '18' & p == 'tcr' & ind == 0, "_6(06t)", cod),
                  cod = ifelse(tp_event == 6 & tp == '18' & p == 'tpr' & ind == 0, "_6(06t)", cod),
                  
                  cod = ifelse(tp_event == 6 & tp == '2' & p == 'ppr' & ind == 3, "_6(11p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '1' & p == 'ppr' & ind == 3, "_6(10p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '28' & p == 'ppr' & ind == 3, "_6(21p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '3' & p == 'ppr' & ind == 3, "_6(12p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '26' & p == 'ppr' & ind == 3, "_6(13a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '11' & p == 'p0r' & ind == 3, "_6(01p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '14' & p == 'ppr' & ind == 3, "_6(19p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '6' & p == 'ppr' & ind == 3, "_6(15p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '9' & p == 'ppr' & ind == 3, "_6(17p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '31' & p == 'ppr' & ind == 3, "_6(22p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '15' & p == 'ppr' & ind == 3, "_6(20p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '12' & p == 'ppr' & ind == 3, "_6(01p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '12' & p == 'p0r' & ind == 3, "_6(01p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '13' & p == 'ppr' & ind == 3, "_6(01p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '13' & p == 'p0r' & ind == 3, "_6(01p)", cod),
                  
                  cod = ifelse(tp_event == 6 & tp == '2' & p == 'ppr' & ind == 6, "_6(11a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '1' & p == 'ppr' & ind == 6, "_6(10a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '3' & p == 'ppr' & ind == 6, "_6(12a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '28' & p == 'ppr' & ind == 6, "_6(21a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '11' & p == 'p0r' & ind == 6, "_6(01p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '26' & p == 'ppr' & ind == 6, "_6(13a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '14' & p == 'ppr' & ind == 6, "_6(19a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '6' & p == 'ppr' & ind == 6, "_6(15a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '9' & p == 'ppr' & ind == 6, "_6(17a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '31' & p == 'ppr' & ind == 6, "_6(22a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '15' & p == 'ppr' & ind == 6, "_6(20a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '13' & p == 'p0r' & ind == 6, "_6(01p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '12' & p == 'p0r' & ind == 6, "_6(01p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '1' & p == 'p0r' & ind == 6, "_6(10a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '13' & p == 'ppr' & ind == 6, "_6(01p)", cod),
                  cod = ifelse(tp_event == 6 & tp == '14' & p == 'p0r' & ind == 6, "_6(19a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '8' & p == 'ppr' & ind == 6, "_6(16a)", cod),
                  
                  cod = ifelse(tp_event == 6 & tp == '4' & p == 'ppr' & ind == 10, "_6(13a)", cod),
                  cod = ifelse(tp_event == 6 & tp == '26' & p == 'ppr' & ind == 10, "_6(13a)", cod),
                  
                  cod = ifelse(tp_event == 7 & tp == '1' & p == '00', "_7(1u)", cod),
                  cod = ifelse(tp_event == 7 & tp == '1' & p == 't0', "_7(1t)", cod),
                  cod = ifelse(tp_event == 7 & tp == '2' & p == 'p0', "_7(2p)", cod),
                  cod = ifelse(tp_event == 7 & tp == '3' & p == 'p0', "_7(3p)", cod),
                  cod = ifelse(tp_event == 7 & tp == '4' & p == 'p0', "_7(4p)", cod),
                  cod = ifelse(tp_event == 7 & tp == '5' & p == 'p0', "_7(5p)", cod),
                  cod = ifelse(tp_event == 7 & tp == '6' & p == 'p0', "_7(6p)", cod),
                  
                  cod = ifelse(tp_event == 8, "_8(0)", cod),
                  
                  cod = ifelse(tp_event == 9 & tp == '1', "_9(0)", cod),
                  cod = ifelse(tp_event == 9 & tp == '7', "_9(1)", cod),
                  
                  cod = ifelse(tp_event == 10 & tp == '0' & ind %in% c(1, 2), "_10(0)", cod),
                  cod = ifelse(tp_event == 10 & tp == '0' & ind == 0, "_10(1)", cod),
                  cod = ifelse(tp_event == 10 & tp == '1', "_10(2)", cod),
                  cod = ifelse(tp_event == 10 & tp == '0' & is.na(ind), "_10(0)", cod),
                  
                  cod = ifelse(tp_event == 11 & p == 'c0r', "_11(1)", cod),
                  cod = ifelse(tp_event == 11 & p == 'p0r', "_11(0)", cod),
                  
                  cod = ifelse(tp_event == 12 & ind == 11, "_12(1)", cod),
                  cod = ifelse(tp_event == 12 & ind == 12, "_12(2)", cod),
                  cod = ifelse(tp_event == 12 & ind == 13, "_12(3)", cod),
                  cod = ifelse(tp_event == 12 & ind == 14, "_12(4)", cod),
                  cod = ifelse(tp_event == 12 & ind == 15, "_12(0)", cod),
                  
                  cod = ifelse(tp_event == 13 & ind == 11, "_13(1)", cod),
                  cod = ifelse(tp_event == 13 & ind == 12, "_13(2)", cod),
                  cod = ifelse(tp_event == 13 & ind == 13, "_13(3)", cod),
                  cod = ifelse(tp_event == 13 & ind == 14, "_13(4)", cod),
                  cod = ifelse(tp_event == 13 & ind == 15, "_13(0)", cod),
                  
                  cod = ifelse(tp_event == 18 & tp == '0', "_18(0)", cod),
                  cod = ifelse(tp_event == 18 & tp == '1', "_18(1)", cod),
                  cod = ifelse(tp_event == 18 & tp == '2', "_18(2)", cod),
                  cod = ifelse(tp_event == 18 & tp == '3', "_18(3)", cod),
                  cod = ifelse(tp_event == 18 & tp == '4', "_18(4)", cod),
                  cod = ifelse(tp_event == 18 & tp == '5', "_18(5)", cod),
                  cod = ifelse(tp_event == 18 & tp == '6', "_18(6)", cod),
                  cod = ifelse(tp_event == 18 & tp == '7', "_18(7)", cod)) %>% 
    
    dplyr::mutate(cod = ifelse(tp_event == 3 & tp == '24' & ind == 0, '_3(00c)', cod),
                  cod = ifelse(tp_event == 3 & tp == '23' & ind == 5, '_3(00w)', cod),
                  cod = ifelse(tp_event == 4 & p == '000' & cod == "", '_4(0u)', cod),
                  cod = ifelse(tp_event == 4 & p == 'p00' & cod == "", '_4(0p)', cod),
                  cod = ifelse(tp_event == 4 & p == 't00' & cod == "", '_4(1t)', cod),
                  
                  cod = ifelse(tp_event == 5 & tp == '0' & cod == "", '_5(01p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '1' & cod == "", '_5(34p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '2' & cod == "", '_5(33p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '3' & cod == "", '_5(31p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '4' & cod == "", '_5(12p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '5' & cod == "", '_5(13p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '8' & cod == "", '_5(16p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '11' & cod == "", '_5(37t)', cod),
                  cod = ifelse(tp_event == 5 & tp == '12' & cod == "", '_5(25t)', cod),
                  cod = ifelse(tp_event == 5 & tp == '13' & cod == "", '_5(18p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '15' & cod == "", '_5(19p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '17' & cod == "", '_5(20p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '18' & cod == "", '_5(21p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '19' & cod == "", '_5(22p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '20' & cod == "", '_5(23p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '23' & cod == "", '_5(26p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '32' & cod == "", '_5(43p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '37' & cod == "", '_5(13p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '38' & cod == "", '_5(38t)', cod),
                  cod = ifelse(tp_event == 5 & tp == '40' & cod == "", '_5(35p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '41' & ind == 6 & cod == "", '_5(33p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '41' & ind == 9 & cod == "", '_5(33p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '41' & ind == 7 & cod == "", '_5(35p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '42' & cod == "", '_5(39t)', cod),
                  cod = ifelse(tp_event == 5 & tp == '43' & cod == "", '_5(31p)', cod),
                  cod = ifelse(tp_event == 5 & tp == '52' & cod == "", '_5(01p)', cod),
                  
                  cod = ifelse(tp_event == 6 & tp == '16' & cod == "", '_6(4cc)', cod),
                  cod = ifelse(tp_event == 6 & tp == '1' & ind == 0 & cod == "", '_6(10a)', cod),
                  cod = ifelse(tp_event == 6 & tp == '2' & ind == 0 & cod == "", '_6(11a)', cod),
                  cod = ifelse(tp_event == 6 & tp == '2' & ind == 3 & cod == "", '_6(11p)', cod),
                  cod = ifelse(tp_event == 6 & tp == '5' & ind == 3 & cod == "", '_6(14p)', cod),
                  cod = ifelse(tp_event == 6 & tp == '5' & ind == 6 & cod == "", '_6(14a)', cod),
                  cod = ifelse(tp_event == 6 & tp == '19' & ind == 3 & cod == "", '_6(01p)', cod),
                  cod = ifelse(tp_event == 6 & tp == '19' & ind == 6 & cod == "", '_6(01p)', cod),
                  cod = ifelse(tp_event == 6 & tp == '19' & cod == "", '_6(01p)', cod),
                  cod = ifelse(tp_event == 6 & tp == '27' & ind == 3 & cod == "", '_6(10p)', cod),
                  cod = ifelse(tp_event == 6 & tp == '27' & ind == 6 & cod == "", '_6(10a)', cod),
                  cod = ifelse(tp_event == 6 & tp == '29' & ind == 3 & cod == "", '_6(11p)', cod),
                  cod = ifelse(tp_event == 6 & tp == '29' & ind == 6 & cod == "", '_6(11a)', cod),
                  cod = ifelse(tp_event == 6 & tp == '32' & cod == "", '_6(01p)', cod),
                  
                  cod = ifelse(tp_event == 7 & tp == '0' & cod == "", '_7(0p)', cod),
                  cod = ifelse(tp_event == 7 & tp == '6' & cod == "", '_7(6p)', cod),
                  cod = ifelse(tp_event == 7 & tp == '18' & cod == "", '_7(0p)', cod),
                  
                  cod = ifelse(tp_event == 9 & tp == '2' & cod == "", '_9(0)', cod),
                  cod = ifelse(tp_event == 9 & tp == '4' & cod == "", '_9(2)', cod),
                  cod = ifelse(tp_event == 9 & tp == '0' & cod == "", '_9(3)', cod))
}

fix_plays_without_team <- function(df){
  
  df %>% 
    dplyr::mutate(location = ifelse(game_id == '0021900620' & action_id ==  81 & cod == '_6(01au)', 'h', location),
                  location = ifelse(game_id == '0021900691' & action_id ==  76 & cod == '_6(01au)', 'v', location),
                  location = ifelse(game_id == '0021900703' & action_id == 290 & cod == '_6(01au)', 'h', location),
                  cod = ifelse(cod == '_6(01au)', '_6(01at)', cod)) %>% 
    dplyr::mutate(location = ifelse(game_id == "0021900291" & action_id ==  65 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900291" & action_id == 375 & cod == '_6(06au)', 'h', location),
                  location = ifelse(game_id == "0021900324" & action_id == 192 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900326" & action_id == 489 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900341" & action_id == 247 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900342" & action_id == 316 & cod == '_6(06au)', 'h', location),
                  location = ifelse(game_id == "0021900361" & action_id == 249 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900361" & action_id == 296 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900379" & action_id == 282 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900387" & action_id == 273 & cod == '_6(06au)', 'h', location),
                  location = ifelse(game_id == "0021900395" & action_id == 363 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900404" & action_id == 541 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900460" & action_id == 477 & cod == '_6(06au)', 'h', location),
                  location = ifelse(game_id == "0021900487" & action_id == 255 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900524" & action_id == 399 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900535" & action_id == 159 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900682" & action_id == 113 & cod == '_6(06au)', 'h', location),
                  location = ifelse(game_id == "0021900757" & action_id == 245 & cod == '_6(06au)', 'h', location),
                  location = ifelse(game_id == "0021900844" & action_id == 318 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021900883" & action_id == 405 & cod == '_6(06au)', 'h', location),
                  location = ifelse(game_id == "0021901237" & action_id == 295 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0021901313" & action_id == 123 & cod == '_6(06au)', 'v', location),
                  location = ifelse(game_id == "0041900177" & action_id == 207 & cod == '_6(06au)', 'h', location),
                  location = ifelse(game_id == "0022000042" & action_id == 331 & cod == '_6(06au)', 'h', location),
                  location = ifelse(game_id == "0022000317" & action_id == 258 & cod == '_6(06au)', 'v', location),
                  cod = ifelse(cod == '_6(06au)', '_6(06at)', cod)) %>% 
    dplyr::mutate(location = ifelse(game_id == '0041800406' & action_id == 482 & cod == '_6(08au)', 'h', location),
                  location = ifelse(game_id == '0022000285' & action_id == 462 & cod == '_6(08au)', 'v', location),
                  cod = ifelse(cod == '_6(08au)', '_6(08at)', cod)) %>% 
    dplyr::mutate(location = ifelse(game_id == "0021800001" & action_id == 344 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800002" & action_id == 502 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800003" & action_id == 255 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800008" & action_id == 522 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800014" & action_id == 314 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800020" & action_id == 116 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800019" & action_id ==  23 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800031" & action_id == 384 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800035" & action_id == 364 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800037" & action_id == 427 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800038" & action_id == 147 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800045" & action_id == 248 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800048" & action_id == 387 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800058" & action_id == 173 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800058" & action_id == 438 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800060" & action_id == 206 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800065" & action_id == 247 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800069" & action_id == 161 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800073" & action_id == 233 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800073" & action_id == 360 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800075" & action_id ==  66 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800076" & action_id == 217 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800077" & action_id == 191 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800079" & action_id == 118 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800082" & action_id == 259 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800085" & action_id == 292 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800087" & action_id == 457 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800088" & action_id == 460 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800092" & action_id == 248 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800092" & action_id == 342 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800101" & action_id == 283 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800103" & action_id ==  43 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800108" & action_id == 201 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800108" & action_id == 316 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800109" & action_id ==  88 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800112" & action_id == 366 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800114" & action_id == 213 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800119" & action_id == 171 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800123" & action_id == 274 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800128" & action_id ==   2 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800129" & action_id == 178 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800132" & action_id == 404 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800134" & action_id == 306 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800136" & action_id == 427 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800140" & action_id == 345 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800144" & action_id == 345 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800145" & action_id ==  73 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800146" & action_id == 257 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800146" & action_id == 319 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800148" & action_id == 151 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800151" & action_id ==  41 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800152" & action_id == 188 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800154" & action_id ==  36 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800155" & action_id ==  94 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800157" & action_id == 399 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800158" & action_id == 255 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800160" & action_id == 417 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800161" & action_id == 388 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800163" & action_id == 243 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800163" & action_id == 372 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800164" & action_id == 246 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800168" & action_id ==  90 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800168" & action_id == 137 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800170" & action_id == 123 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800173" & action_id == 348 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800176" & action_id == 126 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800176" & action_id == 301 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800178" & action_id == 237 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800179" & action_id == 298 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800181" & action_id == 365 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800181" & action_id == 380 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800185" & action_id == 340 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800188" & action_id == 361 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800192" & action_id ==  51 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800194" & action_id == 416 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800196" & action_id == 390 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800197" & action_id == 363 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800198" & action_id == 516 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800200" & action_id == 216 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800209" & action_id == 114 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800210" & action_id == 195 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800212" & action_id ==   6 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800213" & action_id == 338 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800215" & action_id == 222 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800216" & action_id == 202 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800219" & action_id ==  56 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800223" & action_id ==  76 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800232" & action_id ==  92 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800235" & action_id == 491 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800236" & action_id ==  87 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800237" & action_id == 195 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800238" & action_id == 119 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800239" & action_id ==  99 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800243" & action_id == 107 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800246" & action_id == 371 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800247" & action_id == 507 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800254" & action_id == 222 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800254" & action_id == 239 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800255" & action_id == 300 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800258" & action_id ==  35 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800259" & action_id == 316 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800261" & action_id == 220 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800263" & action_id == 499 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800268" & action_id == 297 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800271" & action_id == 329 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800273" & action_id == 314 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800275" & action_id == 231 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800277" & action_id == 313 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800278" & action_id == 156 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800279" & action_id ==  90 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800282" & action_id == 180 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800283" & action_id == 342 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800293" & action_id == 355 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800301" & action_id == 160 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800302" & action_id == 129 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800305" & action_id ==  59 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800306" & action_id == 535 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800313" & action_id ==  53 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800313" & action_id == 213 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800315" & action_id == 113 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800317" & action_id == 317 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800321" & action_id == 225 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800322" & action_id ==  12 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800323" & action_id ==  30 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800325" & action_id == 220 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800328" & action_id == 504 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800329" & action_id == 383 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800331" & action_id == 175 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800336" & action_id ==  97 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800337" & action_id == 403 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800338" & action_id == 355 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800340" & action_id == 153 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800341" & action_id ==  80 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800343" & action_id == 267 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800344" & action_id == 333 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800346" & action_id == 157 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800347" & action_id ==  72 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800349" & action_id == 307 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800357" & action_id == 246 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800363" & action_id == 366 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800370" & action_id == 242 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800371" & action_id == 365 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800372" & action_id == 236 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800379" & action_id == 321 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800381" & action_id ==  22 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800383" & action_id == 418 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800386" & action_id == 471 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800387" & action_id == 244 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800396" & action_id == 192 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800396" & action_id == 420 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800398" & action_id == 363 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800400" & action_id == 282 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800402" & action_id == 280 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800404" & action_id == 270 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800408" & action_id == 357 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800412" & action_id == 163 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800420" & action_id ==  63 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800420" & action_id == 538 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800423" & action_id == 379 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800426" & action_id ==  25 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800428" & action_id == 248 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800429" & action_id == 259 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800446" & action_id == 245 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800447" & action_id == 451 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800450" & action_id == 186 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800450" & action_id == 292 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800452" & action_id == 148 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800462" & action_id == 207 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800463" & action_id == 363 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800477" & action_id ==  47 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800488" & action_id == 150 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800496" & action_id == 267 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800497" & action_id ==  86 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800497" & action_id == 347 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800500" & action_id == 417 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800502" & action_id == 433 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800508" & action_id ==  61 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800509" & action_id == 289 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800513" & action_id == 229 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800513" & action_id == 313 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800515" & action_id ==  10 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800521" & action_id == 396 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800522" & action_id == 451 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800524" & action_id == 307 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800527" & action_id == 168 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800538" & action_id == 225 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800541" & action_id == 356 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800551" & action_id ==  44 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800554" & action_id == 328 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800557" & action_id == 235 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800558" & action_id ==  63 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800558" & action_id == 159 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800559" & action_id ==  82 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800559" & action_id == 189 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800560" & action_id == 108 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800561" & action_id == 278 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800568" & action_id == 219 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800571" & action_id == 472 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800573" & action_id == 295 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800576" & action_id == 153 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800582" & action_id == 415 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800583" & action_id ==  78 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800583" & action_id == 130 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800586" & action_id == 482 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800588" & action_id == 223 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800591" & action_id ==  89 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800594" & action_id ==  45 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800599" & action_id == 190 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800603" & action_id == 280 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800605" & action_id == 136 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800606" & action_id == 435 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800608" & action_id ==  43 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800609" & action_id == 560 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800611" & action_id == 213 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800613" & action_id == 220 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800618" & action_id == 375 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800620" & action_id == 221 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800622" & action_id == 416 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800626" & action_id ==  52 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800629" & action_id == 273 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800629" & action_id == 411 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800630" & action_id ==  65 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800630" & action_id == 309 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800631" & action_id == 108 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800632" & action_id ==  26 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800633" & action_id == 267 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800638" & action_id == 267 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800649" & action_id == 437 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800653" & action_id == 479 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800656" & action_id == 131 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800659" & action_id == 453 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800660" & action_id == 230 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800661" & action_id == 192 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800674" & action_id == 264 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800678" & action_id == 496 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800680" & action_id == 229 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800680" & action_id == 315 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800690" & action_id == 260 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800692" & action_id == 437 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800693" & action_id ==  69 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800693" & action_id ==  90 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800696" & action_id ==  81 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800697" & action_id == 212 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800698" & action_id == 120 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800702" & action_id == 221 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800707" & action_id == 133 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800707" & action_id == 192 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800708" & action_id == 476 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800714" & action_id == 214 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800715" & action_id == 304 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800727" & action_id == 379 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800728" & action_id ==  64 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800730" & action_id == 387 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800731" & action_id == 165 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800733" & action_id == 337 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800733" & action_id == 499 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800734" & action_id == 496 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800738" & action_id == 283 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800739" & action_id == 350 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800741" & action_id == 201 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800743" & action_id == 127 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800747" & action_id == 393 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800750" & action_id == 399 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800751" & action_id == 255 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800763" & action_id ==  95 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800769" & action_id == 390 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800773" & action_id == 216 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800773" & action_id == 288 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800775" & action_id == 379 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800775" & action_id == 403 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800780" & action_id == 410 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800781" & action_id == 335 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800782" & action_id == 124 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800782" & action_id == 266 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800783" & action_id == 302 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800784" & action_id ==  73 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800785" & action_id == 234 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800785" & action_id == 260 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800795" & action_id == 419 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800806" & action_id ==  78 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800806" & action_id == 220 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800809" & action_id == 312 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800812" & action_id == 353 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800814" & action_id ==  46 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800815" & action_id == 332 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800818" & action_id == 166 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800819" & action_id == 217 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800819" & action_id == 381 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800820" & action_id == 108 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800826" & action_id == 139 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800830" & action_id == 332 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800832" & action_id == 492 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800836" & action_id ==  92 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800838" & action_id == 331 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800840" & action_id ==  55 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800841" & action_id == 113 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800841" & action_id == 407 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800843" & action_id == 298 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800845" & action_id == 232 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800848" & action_id == 182 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800854" & action_id == 480 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800866" & action_id == 223 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800868" & action_id == 308 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800868" & action_id == 320 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800869" & action_id == 145 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800878" & action_id == 332 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800881" & action_id == 293 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800883" & action_id == 357 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800885" & action_id == 374 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800886" & action_id == 361 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800892" & action_id == 310 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800894" & action_id == 210 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800896" & action_id == 191 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800900" & action_id == 452 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800904" & action_id == 513 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800905" & action_id == 281 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800912" & action_id == 331 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800915" & action_id == 368 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800921" & action_id == 240 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800924" & action_id == 248 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800925" & action_id == 386 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800927" & action_id ==  54 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800928" & action_id == 500 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800935" & action_id == 175 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800937" & action_id == 435 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800939" & action_id == 123 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800938" & action_id == 226 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800943" & action_id == 479 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800948" & action_id == 171 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800963" & action_id == 132 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800967" & action_id == 414 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800970" & action_id == 296 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800970" & action_id == 432 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800973" & action_id == 194 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800976" & action_id == 516 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800977" & action_id == 199 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800979" & action_id == 214 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800983" & action_id ==  14 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800984" & action_id == 118 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021800987" & action_id == 279 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800994" & action_id == 178 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021800999" & action_id == 324 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801000" & action_id == 365 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801001" & action_id == 223 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801001" & action_id == 402 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801002" & action_id == 464 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801004" & action_id == 299 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801009" & action_id == 107 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801009" & action_id == 321 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801012" & action_id == 202 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801013" & action_id == 334 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801015" & action_id == 393 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801014" & action_id == 278 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801021" & action_id == 372 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801033" & action_id == 192 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801035" & action_id ==  44 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801037" & action_id ==  45 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801037" & action_id == 404 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801045" & action_id == 237 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801044" & action_id == 420 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801046" & action_id == 173 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801061" & action_id == 156 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801063" & action_id == 344 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801066" & action_id ==  75 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801067" & action_id == 268 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801072" & action_id == 429 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801073" & action_id ==  67 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801074" & action_id == 214 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801079" & action_id ==  96 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801079" & action_id == 337 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801090" & action_id == 341 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801093" & action_id == 339 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801111" & action_id == 161 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801113" & action_id == 278 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801119" & action_id == 256 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801120" & action_id == 243 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801121" & action_id ==  51 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801123" & action_id ==  11 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801129" & action_id == 220 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801130" & action_id == 275 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801133" & action_id ==  70 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801138" & action_id == 233 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801140" & action_id == 244 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801147" & action_id ==  46 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801151" & action_id == 349 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801159" & action_id == 150 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801166" & action_id == 277 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801168" & action_id == 165 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801173" & action_id == 299 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801174" & action_id == 274 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801180" & action_id == 247 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801184" & action_id == 136 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801185" & action_id == 135 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801191" & action_id ==   2 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801193" & action_id == 330 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801193" & action_id == 379 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801195" & action_id == 412 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801198" & action_id == 194 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801207" & action_id ==  10 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801217" & action_id ==   2 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801220" & action_id == 483 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801221" & action_id == 122 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801222" & action_id == 354 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801227" & action_id == 287 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021801227" & action_id == 329 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021801230" & action_id == 322 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0041800142" & action_id == 271 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0041800142" & action_id == 283 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800152" & action_id ==  31 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800102" & action_id == 221 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800133" & action_id == 374 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800103" & action_id == 112 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800174" & action_id == 426 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800115" & action_id ==  35 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800165" & action_id == 269 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800213" & action_id ==  79 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800223" & action_id ==  75 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0041800225" & action_id == 367 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0041800226" & action_id ==  25 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800301" & action_id ==  23 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0041800303" & action_id == 288 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0041800305" & action_id == 206 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0041800306" & action_id == 271 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800402" & action_id == 244 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800404" & action_id == 404 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0041800404" & action_id == 422 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0041800405" & action_id ==  67 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900001" & action_id == 297 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900002" & action_id ==  71 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900008" & action_id == 344 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900014" & action_id == 194 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900017" & action_id == 237 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900021" & action_id == 209 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900025" & action_id ==  37 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900026" & action_id ==  99 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900027" & action_id == 111 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900031" & action_id ==   2 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900031" & action_id == 465 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900037" & action_id == 208 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900040" & action_id ==  97 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900045" & action_id == 190 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900045" & action_id == 291 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900048" & action_id == 100 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900055" & action_id == 251 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900056" & action_id == 310 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900060" & action_id == 416 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900062" & action_id == 253 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900065" & action_id == 160 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900065" & action_id == 260 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900070" & action_id == 144 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900078" & action_id == 240 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900081" & action_id ==  72 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900081" & action_id == 276 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900082" & action_id ==  63 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900082" & action_id == 134 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900087" & action_id == 295 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900091" & action_id == 427 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900100" & action_id == 233 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900105" & action_id == 127 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900105" & action_id == 532 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900107" & action_id == 192 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900109" & action_id == 142 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900121" & action_id == 294 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900128" & action_id == 130 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900128" & action_id == 194 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900134" & action_id == 334 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900136" & action_id == 279 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900136" & action_id == 509 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900140" & action_id == 177 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900148" & action_id == 283 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900149" & action_id == 161 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900152" & action_id == 492 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900154" & action_id == 389 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900157" & action_id == 180 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900159" & action_id == 259 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900159" & action_id == 392 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900160" & action_id == 458 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900164" & action_id == 259 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900164" & action_id == 415 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900168" & action_id == 369 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900172" & action_id == 351 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900177" & action_id == 317 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900178" & action_id == 339 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900188" & action_id == 313 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900193" & action_id == 381 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900200" & action_id == 269 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900205" & action_id == 268 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900213" & action_id == 303 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900216" & action_id == 277 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900217" & action_id == 234 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900219" & action_id == 454 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900221" & action_id == 186 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900227" & action_id == 188 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900230" & action_id == 147 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900256" & action_id == 194 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900260" & action_id == 239 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900262" & action_id ==  76 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900264" & action_id == 320 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900269" & action_id ==  80 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900271" & action_id == 123 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900271" & action_id == 372 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900273" & action_id == 252 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900273" & action_id == 355 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900276" & action_id == 171 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900283" & action_id == 334 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900285" & action_id ==  18 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900289" & action_id ==  92 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900291" & action_id ==  27 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900291" & action_id == 244 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900296" & action_id == 419 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900301" & action_id == 257 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900305" & action_id ==  21 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900306" & action_id ==  63 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900307" & action_id == 241 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900309" & action_id == 317 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900313" & action_id == 105 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900313" & action_id == 420 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900317" & action_id ==  67 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900318" & action_id ==  67 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900319" & action_id ==  29 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900323" & action_id == 271 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900324" & action_id ==  70 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900330" & action_id ==  99 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900332" & action_id ==  11 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900333" & action_id == 555 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900339" & action_id == 467 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900340" & action_id == 153 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900341" & action_id == 135 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900342" & action_id ==  67 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900344" & action_id == 261 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900344" & action_id == 344 & cod == '_7(1u)', "v", location),
                  location = ifelse(game_id == "0021900345" & action_id == 431 & cod == '_7(1u)', "h", location),
                  location = ifelse(game_id == "0021900351" & action_id == 307 & cod == '_7(1u)', "v", location),
                  cod = ifelse(cod == '_7(1u)', '_7(1t)', cod)) %>% 
    dplyr::mutate(location = ifelse(game_id == "0042100171" & action_id == 364, "v", location),
                  cod = ifelse(cod == '_5(40u)', '_5(40t)', cod)) %>% 
    dplyr::mutate(location = ifelse(game_id == "0022000285" & action_id == 463, "v", location),
                  location = ifelse(game_id == "0022000215" & action_id == 512, "v", location),
                  location = ifelse(game_id == "0022101135" & action_id == 465, "v", location),
                  location = ifelse(game_id == "0022200323" & action_id == 538, "v", location),
                  cod = ifelse(cod == '_5(39u)', '_5(39t)', cod))
  
}

process_data <- function(file){
  
  xx <- load(file)
  xx <- get(xx)
  
  xx %>% 
    set_ind() %>% 
    dplyr::mutate(tp_subaction = stringr::str_trim(string = tp_subaction),
                  tp_subaction = ifelse(tp_subaction == "", NA_character_, tp_subaction)) %>% 
    create_columns_pre_process() %>% 
    set_cod()
  
  
}

build_df_pbp_plays <- function(file){
  
  xx <- load(file)
  xx <- get(xx)
  
  xx %>% 
    dplyr::distinct() %>% 
    set_ind() %>% 
    dplyr::mutate(tp_action = stringr::str_trim(tp_action),
                  tp_subaction = stringr::str_trim(tp_subaction)) %>% 
    dplyr::select(season, game_id, num_event, action_id, period, 
                  clock, tp_event, tp_action, tp_subevent, 
                  tp_subaction, persons, location, ind, description) %>% 
    dplyr::mutate(action_id = as.integer(action_id)) %>% 
    dplyr::arrange(season, game_id, period, desc(clock), action_id) %>% 
    create_columns_pre_process() %>% 
    dplyr::mutate(p = ifelse(tp_event %in% c(5, 7, 18), stringr::str_sub(p, 1, 2), p)) %>% 
    dplyr::mutate(ind = ifelse(tp_event %in% c(12, 13) & ind > 14, 15, ind)) %>% 
    set_cod() %>% 
    fix_plays_without_team()
  
  
}

build_df_freq_plays <- function(file){
  
  xx <- load(file)
  xx <- get(xx)
  
  xx %>% 
    set_ind() %>% 
    create_columns_pre_process() %>% 
    dplyr::mutate(p = ifelse(tp_event %in% c(5, 7, 18), stringr::str_sub(p, 1, 2), p)) %>% 
    dplyr::mutate(ind = ifelse(tp_event %in% c(12, 13) & ind > 14, 15, ind)) %>% 
    set_cod() %>% 
    dplyr::group_by(season, game_id, tp_event, tp_action, 
                    tp_subevent, tp_subaction, period, cod) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop')
  
}

future::plan(future::multisession(), 
             workers = future::availableCores())

#df_freqandcod_pbp
df_freqandcod_pbp <- dir(path = "D:/Mestrado/NBA/data/pbp_checkpoint", full.names = TRUE) %>% 
  purrr::keep(.x = ., .p = stringr::str_detect, pattern = '\\.RData$') %>% 
  furrr::future_map(.x = ., .f = process_data, .progress = TRUE) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(tp_action = stringr::str_trim(tp_action),
                tp_action = ifelse(is.na(tp_action) & tp_event == 4, "Rebound", tp_action),
                tp_action = ifelse(is.na(tp_action) & tp_event == 3, "Free Throw", tp_action),
                tp_action = ifelse(is.na(tp_action) & tp_event == 5, "Turnover", tp_action),
                tp_action = ifelse(is.na(tp_action) & tp_event == 7, "Violation", tp_action)) %>%
  dplyr::mutate(tp_subaction = stringr::str_trim(tp_subaction),
                tp_subaction = ifelse(stringr::str_detect(cod, "_[1|2]\\(1"), "Dunk shot", tp_subaction),
                tp_subaction = ifelse(stringr::str_detect(cod, "_[1|2]\\(2"), "Hook shot", tp_subaction),
                tp_subaction = ifelse(stringr::str_detect(cod, "_[1|2]\\(3"), "Layup shot", tp_subaction),
                tp_subaction = ifelse(stringr::str_detect(cod, "_[1|2]\\(4"), "Jump shot", tp_subaction),
                tp_subaction = ifelse(stringr::str_detect(cod, "_[1|2]\\(5"), "3pt Jump shot", tp_subaction)) %>%
  dplyr::group_by(season) %>% 
  dplyr::mutate(num_jogos = dplyr::n_distinct(game_id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(tp_event, tp_action, tp_subaction, p, season, num_jogos, cod, ind) %>%
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%
  dplyr::group_by(tp_event, tp_action, tp_subaction, p, cod, ind) %>%
  dplyr::mutate(cnt = sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(n = 1000 * n / num_jogos) %>%
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4})(\\-)([0-9]{2})", "s\\3")) %>%
  dplyr::select(-num_jogos) %>%
  tidyr::spread(season, n) %>%
  dplyr::arrange(tp_event, cod) %>% 
  dplyr::mutate_if(.predicate = is.double, .funs = round, digits = 2)

future::plan(future::sequential())


df_freqandcod_pbp$cv <- df_freqandcod_pbp[,c('s09', 's10', 's11', 's12', 's13', 's14', 's15', 's16', 
                                             's17', 's18', 's19', 's20', 's21', 's22', 's23', 's24')] %>% 
  dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
  apply(X = ., MARGIN = 1, FUN = function(x){sd(x)/mean(x)})

df_freqandcod_pbp <- df_freqandcod_pbp %>% 
  dplyr::mutate_at(.vars = c('s09', 's10', 's11', 's12', 's13', 's14', 's15', 's16', 
                             's17', 's18', 's19', 's20', 's21', 's22', 's23', 's24'), 
                   .funs = round, digits = 2)

save(df_freqandcod_pbp, file = "D:/Mestrado/NBA/data/df_freqandcod_pbp.RData")

future::plan(future::multisession(), 
             workers = future::availableCores())

pbp_jogadas <- dir("D:/Mestrado/NBA/data/pbp_checkpoint/", full.names = TRUE) %>% 
  .[stringr::str_detect(string = ., pattern = "\\.RData")] %>% 
  furrr::future_map(.x = ., .f = build_df_pbp_plays, .progress = TRUE) %>% 
  dplyr::bind_rows() %>% 
  dplyr::select(-c(p, ind, tp))

future::plan(future::sequential())

save(pbp_jogadas, file = "D:/Mestrado/NBA/data/pbp_jogadas.RData")

future::plan(future::multisession(), 
             workers = future::availableCores())

freq_jogadas_periodo <- dir(path = "D:/Mestrado/NBA/data/pbp_checkpoint", 
                            full.names = TRUE) %>% 
  purrr::keep(.x = ., .p = stringr::str_detect, pattern = '\\.RData$') %>% 
  furrr::future_map(.x = ., .f = build_df_freq_plays, .progress = TRUE) %>% 
  dplyr::bind_rows() %>% 
  dplyr::as_tibble()

future::plan(future::sequential())

df_freq_jogadas_cod <- freq_jogadas_periodo %>% 
  dplyr::group_by(tp_event, tp_action, tp_subevent, tp_subaction, cod, season) %>% 
  dplyr::summarise(n = sum(n), .groups = 'drop_last') %>% 
  dplyr::mutate(cnt = sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(season = stringr::str_replace(season, "([0-9]{4}\\-)([0-9]{2})", '\\2')) %>% 
  dplyr::mutate(season = paste0('s', season)) %>% 
  tidyr::spread(season, n)

save(freq_jogadas_periodo, file = "D:/Mestrado/NBA/data/freq_jogadas_periodo.RData")
save(df_freq_jogadas_cod, file = "D:/Mestrado/NBA/data/df_freq_jogadas_cod.RData")

#####################################
# a partir daqui eu não terminei

load("D:/Mestrado/NBA/data/pbp_jogadas.RData")


extract_sintaxe_shots <- function(df){
  
  df %>% 
    dplyr::mutate(desc = description, sintaxe = 0) %>% 
    dplyr::mutate(cnt_pts = ifelse(stringr::str_detect(cod, "^_1\\("), stringr::str_extract(desc, "[0-9]+\\s+PTS"), NA_character_),
                  desc = ifelse(stringr::str_detect(cod, "^_1\\("), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc),
                  dist = ifelse(stringr::str_detect(cod, "^_[1|2]\\("), stringr::str_extract(desc, "[0-9]+\\'"), NA_character_),
                  desc = ifelse(stringr::str_detect(cod, "^_[1|2]\\("), stringr::str_replace(desc, "[0-9]+\\'", "[dist]"), desc)) %>% 
    dplyr::mutate(tp_shot = ifelse(stringr::str_detect(cod, "^_[1|2]\\(1"), stringr::str_extract(desc, "Running Alley Oop Dunk Shot|Alley Oop Dunk|Cutting Dunk Shot|Driving Dunk|Driving Reverse Dunk Shot|Driving Slam Dunk|Dunk Shot|Putback Dunk|Putback Slam Dunk|Reverse Dunk|Reverse Slam Dunk|Running Dunk|Running Reverse Dunk Shot|Running Slam Dunk|Slam Dunk|Tip Dunk Shot|Dunk"), NA_character_),
                  tp_shot = ifelse(stringr::str_detect(cod, "^_[1|2]\\(2"), stringr::str_extract(desc, "Driving Hook Shot|Driving Bank Hook Shot|Hook Bank Shot|Jump Hook Shot|Jump Bank Hook Shot|Running Bank Hook Shot|Running Hook Shot|Turnaround Hook Shot|Turnaround Bank Hook Shot|Hook Shot"), tp_shot),
                  tp_shot = ifelse(stringr::str_detect(cod, "^_[1|2]\\(3"), stringr::str_extract(desc, "Alley Oop Layup|Running Reverse Layup|Running Finger Roll Layup|Finger Roll Layup|Driving Reverse Layup|Running Layup|Driving Finger Roll Layup|Driving Layup|Cutting Layup Shot|Cutting Finger Roll Layup Shot|Putback Layup|Running Alley Oop Layup Shot|Tip Layup Shot|Running Tip Shot|Tip Shot|Reverse Layup|Layup"), tp_shot),
                  tp_shot = ifelse(stringr::str_detect(cod, "^_[1|2]\\(4"), stringr::str_extract(desc, "Turnaround Fadeaway Shot|Turnaround Fadeaway Bank Jump Shot|Driving Floating Bank Jump Shot|Driving Floating Jump Shot|Running Pull-Up Jump Shot|Step Back Bank Jump Shot|Turnaround Bank Shot|Turnaround Jump Shot|Step Back Jump Shot|Turnaround Fadeaway|Fadeaway Bank Shot|Floating Jump Shot|Driving Bank Shot|Driving Jump Shot|Running Bank Shot|Running Jump Shot|Pullup Bank Shot|Pullup Jump Shot|Fadeaway Jumper|Jump Bank Shot|Jump Shot|No Shot|Shot"), tp_shot),
                  tp_shot = ifelse(stringr::str_detect(cod, "^_[1|2]\\(5"), stringr::str_extract(desc, "3PT Driving Layup|3PT Hook Bank Shot|3PT Layup|3PT Driving Hook Shot|3PT Driving Bank Hook Shot|3PT Running Layup|3PT Turnaround Hook Shot|3PT Reverse Layup|3PT Tip Shot|3PT Running Hook Shot|3PT Dunk|3PT Jump Hook Shot|3PT Hook Shot|3PT Turnaround Fadeaway Shot|3PT Running Bank Hook Shot|3PT Turnaround Bank Hook Shot|3PT Turnaround Fadeaway Bank Jump Shot|3PT Driving Floating Bank Jump Shot|3PT Driving Floating Jump Shot|3PT Running Pull-Up Jump Shot|3PT Step Back Bank Jump Shot|3PT Turnaround Bank Shot|3PT Turnaround Jump Shot|3PT Step Back Jump Shot|3PT Turnaround Fadeaway|3PT Fadeaway Bank Shot|3PT Floating Jump Shot|3PT Driving Bank Shot|3PT Driving Jump Shot|3PT Running Bank Shot|3PT Running Jump Shot|3PT Pullup Bank Shot|3PT Pullup Jump Shot|3PT Fadeaway Jumper|3PT Jump Bank Shot|3PT Putback Layup|3PT Jump Shot|3PT No Shot|3PT Shot"), tp_shot),
                  desc = ifelse(!is.na(tp_shot) & stringr::str_detect(cod, "^_[1|2]"), stringr::str_replace(desc, tp_shot, "[SHOT]"), desc)) %>% 
    dplyr::mutate(player = ifelse(stringr::str_detect(cod, "^_1"), stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1"), NA_character_)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(cod, "^_1\\(.\\)$") & sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_1\\(.\\)$") & sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(\\(cnt PTS\\))", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(cod, "^_1\\(.\\)$") & sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[SHOT\\])\\s+(\\(cnt PTS\\))"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_1\\(.\\)$") & sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[SHOT\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    
    dplyr::mutate(cnt_ast = ifelse(stringr::str_detect(cod, "^_1\\(.a\\)$"), stringr::str_extract(desc, "[0-9]+\\s+AST"), NA_character_)) %>%
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_1\\(.a\\)$"), stringr::str_replace(desc, cnt_ast, "cnt AST"), desc)) %>%
    dplyr::mutate(player_ast = ifelse(stringr::str_detect(cod, "^_1\\(.a\\)$"), stringr::str_extract(desc, "\\s+\\(.+?\\s+cnt\\s+AST\\)$"), NA_character_),
                  player_ast = ifelse(!is.na(player_ast), stringr::str_remove_all(player_ast, "cnt\\s+PTS\\)|\\s+\\(|\\s+cnt\\s+AST\\)"), player_ast)) %>%
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_1\\(.a\\)$"), stringr::str_replace(desc, "\\((.+?)\\s+cnt\\s+AST\\)", "([player] cnt AST)"), desc)) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(cod, "^_1\\(.a\\)$") & sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_1\\(.a\\)$") & sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(.+?$)", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(cod, "^_1\\(.a\\)$") & sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[SHOT\\])\\s+(.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_1\\(.a\\)$") & sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[SHOT\\])\\s+(.+?$)", "[player] \\2 \\3"), desc)) %>% 
    
    dplyr::mutate(player = ifelse(stringr::str_detect(cod, "^_2"), stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1"), player)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(cod, "^_2\\(.\\)$") & sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_2\\(.\\)$") & sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])", "MISS [player] [dist] \\2"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(cod, "^_2\\(.\\)$") & sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_2\\(.\\)$") & sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])", "MISS [player] \\2"), desc)) %>% 
    
    dplyr::mutate(cnt_blk = ifelse(stringr::str_detect(cod, "^_2\\(.b\\)$"), stringr::str_extract(desc, "[0-9]+\\s+BLK"), NA_character_)) %>% 
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_2\\(.b\\)$"), stringr::str_replace(desc, cnt_blk, "cnt BLK"), desc)) %>%
    dplyr::mutate(player_blk = ifelse(stringr::str_detect(cod, "^_2\\(.b\\)$"), stringr::str_extract(desc, "\\[SHOT\\].+?\\s+BLOCK"), NA_character_),
                  player_blk = ifelse(!is.na(player_blk), stringr::str_remove_all(player_blk, "\\[SHOT\\]\\s+|\\s+BLOCK"), player_blk)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(cod, "^_2\\(.b\\)$") & sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])(.+?)\\s+(BLOCK.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_2\\(.b\\)$") & sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK.+?$)", "MISS [player] [dist] \\2 [player] \\4"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(cod, "^_2\\(.b\\)$") & sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_2\\(.b\\)$") & sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)", "MISS [player] \\2 [player] \\4"), desc)) %>% 
    dplyr::mutate(player = ifelse(sintaxe == 0, NA_character_, player),
                  player_ast = ifelse(sintaxe == 0, NA_character_, player_ast),
                  player_blk = ifelse(sintaxe == 0, NA_character_, player_blk),
                  tp_shot = ifelse(sintaxe == 0, NA_character_, tp_shot),
                  dist = ifelse(sintaxe == 0, NA_character_, dist),
                  cnt_pts = ifelse(sintaxe == 0, NA_character_, cnt_pts),
                  cnt_ast = ifelse(sintaxe == 0, NA_character_, cnt_ast),
                  cnt_blk = ifelse(sintaxe == 0, NA_character_, cnt_blk))
  
}

extract_sintaxe_free_throw <- function(df){
  
  df %>% 
    dplyr::mutate(desc = description, sintaxe = 0) %>% 
    dplyr::mutate(cnt_pts = ifelse(stringr::str_detect(cod, "^_3\\([0-9]{2}c\\)"), stringr::str_extract(desc, "[0-9]+\\s+PTS"), NA_character_),
                  desc = ifelse(stringr::str_detect(cod, "^_3\\(") & !is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_ft = ifelse(stringr::str_detect(cod, "^_3"), stringr::str_extract(desc, "Free Throw 1 of 1|Free Throw 1 of 2|Free Throw 1 of 3|Free Throw 2 of 2|Free Throw 2 of 3|Free Throw 3 of 3|Free Throw Technical 1 of 2|Free Throw Technical 2 of 2|Free Throw Technical|Free Throw Flagrant 1 of 1|Free Throw Flagrant 1 of 2|Free Throw Flagrant 2 of 2|Free Throw Clear Path 1 of 2|Free Throw Clear Path 2 of 2|Free Throw Flagrant 1 of 3|Free Throw Flagrant 2 of 3|Free Throw Flagrant 3 of 3"), NA_character_),
                  desc = ifelse(!is.na(tp_ft) & stringr::str_detect(cod, "^_3"), stringr::str_replace(desc, tp_ft, "[FREETHROW]"), desc)) %>%
    dplyr::mutate(player = ifelse(stringr::str_detect(cod, "^_3\\([0-9]{2}c\\)"), stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1"), NA_character_)) %>% 
    dplyr::mutate(player = ifelse(stringr::str_detect(cod, "^_3\\([0-9]{2}w\\)"), stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1"), player))  %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(cod, "^_3\\([0-9]{2}c\\)") & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1 & stringr::str_detect(cod, "^_3\\([0-9]{2}c\\)"), stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(cod, "^_3\\([0-9]{2}w\\)") & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1 & stringr::str_detect(cod, "^_3\\([0-9]{2}w\\)"), stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::mutate(player = ifelse(sintaxe == 0, NA_character_, player),
                  tp_ft = ifelse(sintaxe == 0, NA_character_, tp_ft),
                  cnt_pts = ifelse(sintaxe == 0, NA_character_, cnt_pts))
  
}

extract_sintaxe_rebound <- function(df){
  
  df %>% 
    dplyr::mutate(desc = description, sintaxe = 0) %>% 
    dplyr::mutate(cnt_rbd = ifelse(stringr::str_detect(cod, "^_4\\("), stringr::str_extract(desc, "Off\\:[0-9]+\\s+Def\\:[0-9]+"), NA_character_),
                  desc = ifelse(stringr::str_detect(cod, "^_4\\(") & !is.na(cnt_rbd), stringr::str_replace(desc, cnt_rbd, "Off:cnt Def:cnt"), desc)) %>% 
    dplyr::mutate(tp_rnb = ifelse(stringr::str_detect(cod, "^_4\\("), stringr::str_extract(desc, "REBOUND|Rebound"), NA_character_),
                  desc = ifelse(stringr::str_detect(cod, "^_4\\(") & !is.na(tp_rnb), stringr::str_replace(desc, tp_rnb, "[REBOUND]"), desc)) %>% 
    dplyr::mutate(player = ifelse(cod == '_4(0p)', stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1"), NA_character_)) %>% 
    dplyr::mutate(sintaxe = ifelse(cod == '_4(0p)'& sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[REBOUND\\])\\s+(\\(Off\\:cnt\\s+Def\\:cnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(cod == '_4(0p)'& sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[REBOUND\\])\\s+(\\(Off\\:cnt\\s+Def\\:cnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(team = ifelse(stringr::str_detect(cod, "^_4\\(.t"), stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1"), NA_character_)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(cod, "^_4\\(.t") & sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[REBOUND\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(stringr::str_detect(cod, "^_4\\(.t") & sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[REBOUND\\])", "[team] \\2"), desc)) %>% 
    dplyr::mutate(player = ifelse(sintaxe == 0, NA_character_, player),
                  tp_rnb = ifelse(sintaxe == 0, NA_character_, tp_rnb),
                  cnt_rbd = ifelse(sintaxe == 0, NA_character_, cnt_rbd))
  
}

extract_sintaxe_turnover <- function(df){
  
  df %>% 
    dplyr::mutate(desc = description, sintaxe = 0) %>% 
    dplyr::mutate(cnt_tnv = ifelse(stringr::str_detect(desc, "P[0-9]+\\.T[0-9]+"), stringr::str_extract(desc, 'P[0-9]+\\.T[0-9]+'), NA_character_),
                  cnt_tnv = ifelse(stringr::str_detect(desc, "T\\#[0-9]+"), stringr::str_extract(desc, "T\\#[0-9]+"), cnt_tnv)) %>% 
    dplyr::mutate(desc = ifelse(stringr::str_detect(cnt_tnv, "P[0-9]+\\.T[0-9]+"), stringr::str_replace(desc, cnt_tnv, 'Pcnt.Tcnt'), desc),
                  desc = ifelse(stringr::str_detect(cnt_tnv, "T\\#[0-9]+"), stringr::str_replace(desc, cnt_tnv, 'T#cnt'), desc)) %>% 
    dplyr::mutate(cnt_stl = ifelse(stringr::str_detect(cnt_tnv, "P[0-9]+\\.T[0-9]+"), stringr::str_extract(desc, "[0-9]+\\s+STL"), NA_character_),
                  desc = ifelse(!is.na(cnt_stl), stringr::str_replace(desc, cnt_stl, 'cnt STL'), desc)) %>% 
    dplyr::mutate(tp_tnv = ifelse(!is.na(cnt_tnv), stringr::str_extract(desc, "Turnover: 3 Second Violation|Turnover: Kicked Ball Violation|Turnover: Traveling|Turnover: Bad Pass|Turnover: Backcourt|Turnover: Inbound|Turnover: 8 Second Violation|Turnover: Shot Clock|Turnover: 5 Second Inbound|Turnover: 5 Second Violation|Turnover: Too Many Players|No Turnover|Turnover Turnover|Bad Pass Turnover|Traveling Turnover|Foul Turnover|Double Dribble Turnover|Discontinue Dribble Turnover|3 Second Violation Turnover|Inbound Turnover|Backcourt Turnover|Offensive Goaltending Turnover|Lane Violation Turnover|Jump Ball Violation Turnover|Kicked Ball Violation Turnover|Illegal Assist Turnover|Palming Turnover|Double Personal Turnover|Punched Ball Turnover|Swinging Elbows Turnover|Basket from Below Turnover|Illegal Screen Turnover|Step Out of Bounds Turnover|Out Of Bounds Turnover|Player Out of Bounds Violation Turnover|Out of Bounds Lost Ball Turnover|Poss Lost Ball Turnover|Lost Ball Turnover|Out of Bounds - Bad Pass Turnover Turnover|Bad Pass Turnover|Bad Pass Turnover|Lost Ball Turnover|Opposite Basket Turnover|5 Second Violation Turnover|Turnover: "), 
                                  NA_character_)) %>% 
    dplyr::mutate(desc = ifelse(!is.na(tp_tnv), stringr::str_replace(desc, tp_tnv, "[TURNOVER]"), desc)) %>% 
    dplyr::mutate(player = ifelse(stringr::str_detect(cnt_tnv, "P[0-9]+\\.T[0-9]+"), stringr::str_replace(desc, "(^.+?)(\\s+\\[.+?$)", "\\1"), NA_character_),
                  player_stl = ifelse(!is.na(cnt_stl), stringr::str_extract(desc, "\\)\\s+(.+?)\\s+STEAL"), NA_character_),
                  player_stl = ifelse(!is.na(player_stl), stringr::str_remove_all(player_stl, "\\)\\s+|\\s+STEAL"), player_stl)) %>% 
    dplyr::mutate(team = ifelse(stringr::str_detect(cnt_tnv, "T\\#[0-9]+"), stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1"), NA_character_)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(cnt_tnv, "P[0-9]+\\.T[0-9]+") & stringr::str_detect(desc, "(^.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe),
                  desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc),
                  sintaxe = ifelse(stringr::str_detect(cnt_tnv, "P[0-9]+\\.T[0-9]+") & stringr::str_detect(desc, "(^.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(.+?)\\s+(STEAL\\s+\\(cnt\\s+STL\\))$"), 2, sintaxe),
                  desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(^.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(.+?)\\s+(STEAL\\s+\\(cnt\\s+STL\\))$", "[player] \\2 \\3 [player] \\5"), desc),
                  sintaxe = ifelse(stringr::str_detect(cnt_tnv, "T\\#[0-9]+") & stringr::str_detect(desc, "(^.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T\\#cnt\\))$"), 3, sintaxe),
                  desc = ifelse(sintaxe == 3, stringr::str_replace(desc, "(^.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T\\#cnt\\))$", "[team] \\2 \\3"), desc)) %>% 
    dplyr::mutate(desc = ifelse(is.na(sintaxe), description, desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(is.na(sintaxe), 0, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 0, description, desc),
                  player = ifelse(sintaxe == 0, NA_character_, player),
                  player_stl = ifelse(sintaxe == 0, NA_character_, player_stl),
                  team = ifelse(sintaxe == 0, NA_character_, team),
                  tp_tnv = ifelse(sintaxe == 0, NA_character_, tp_tnv),
                  cnt_tnv = ifelse(sintaxe == 0, NA_character_, cnt_tnv),
                  cnt_stl = ifelse(sintaxe == 0, NA_character_, cnt_stl))
  
}

future::plan(future::multisession(), 
             workers = future::availableCores())

df0 <- pbp_jogadas %>% 
  dplyr::filter(tp_event == 5) %>% 
  dplyr::group_split(season) %>% 
  furrr::future_map(.x = ., .f = extract_sintaxe_turnover, .progress = TRUE) %>% 
  dplyr::bind_rows()

future::plan(future::sequential())

df <- pbp_jogadas %>% 
  dplyr::filter(tp_event == 6)

df0 <- df %>% 
  dplyr::select(cod, description) %>% 
  dplyr::mutate(desc = description, sintaxe = 0) %>% 
  dplyr::mutate(cnt_foul = ifelse(stringr::str_detect(desc, "\\(P[0-9]+\\.T[0-9]+\\)"), stringr::str_extract(desc, "\\(P[0-9]+\\.T[0-9]+\\)"), NA_character_),
                cnt_foul = ifelse(stringr::str_detect(desc, "\\(P[0-9]+\\.PN\\)"), stringr::str_extract(desc, "\\(P[0-9]+\\.PN\\)"), cnt_foul),
                cnt_foul = ifelse(stringr::str_detect(desc, "\\(P[0-9]+\\)"), stringr::str_extract(desc, "\\(P[0-9]+\\)"), cnt_foul),
                cnt_foul = ifelse(stringr::str_detect(desc, "\\([0-9]+\\s+PF\\)"), stringr::str_extract(desc, "\\([0-9]+\\s+PF\\)"), cnt_foul),
                cnt_foul = stringr::str_remove_all(cnt_foul, "\\(|\\)")) %>% 
  dplyr::mutate(desc = ifelse(stringr::str_detect(desc, "P[0-9]+\\.T[0-9]+"), stringr::str_replace(desc, "P[0-9]+\\.T[0-9]+", "Pcnt.Tcnt"), desc),
                desc = ifelse(stringr::str_detect(desc, "P[0-9]+\\.PN"), stringr::str_replace(desc, "P[0-9]+\\.PN", "Pcnt.PN"), desc),
                desc = ifelse(stringr::str_detect(desc, "P[0-9]+"), stringr::str_replace(desc, "P[0-9]+", "Pcnt"), desc),
                desc = ifelse(stringr::str_detect(desc, "[0-9]+\\s+PF"), stringr::str_replace_all(desc, "[0-9]+\\s+PF", "cnt PF"), desc)) %>% 
  dplyr::mutate(tp_foul = NA_character_,
                tp_foul = ifelse(cod == '_6(01c)', stringr::str_extract(desc, "Foul\\:T\\.FOUL|Foul\\:Non\\-Unsportsmanlike"), tp_foul),
                tp_foul = ifelse(cod == '_6(01p)', stringr::str_extract(desc, "T\\.FOUL|HANGING\\.TECH\\.FOUL|Non-Unsportsmanlike Tech Foul - Flopping|Non-Unsportsmanlike"), tp_foul),
                tp_foul = ifelse(cod == '_6(01t)', stringr::str_extract(desc, "Foul|T\\.FOUL"), tp_foul),
                tp_foul = ifelse(cod == '_6(05p)', stringr::str_extract(desc, "T\\.Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(06t)', stringr::str_extract(desc, "Foul|Delay"), tp_foul),
                tp_foul = ifelse(cod == '_6(08t)', stringr::str_extract(desc, "Excess Timeout Technical"), tp_foul),
                tp_foul = ifelse(cod == '_6(09t)', stringr::str_extract(desc, "Too Many Players Tech Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(09t)', stringr::str_extract(desc, "Too Many Players Tech Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(10a)', stringr::str_extract(desc, "P\\.FOUL|Personal Block Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(10p)', stringr::str_extract(desc, "P\\.FOUL|Personal Block Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(11a)', stringr::str_extract(desc, "S\\.FOUL|Shooting Block Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(11p)', stringr::str_extract(desc, "S\\.FOUL|Shooting Block Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(12a)', stringr::str_extract(desc, "L\\.B\\.FOUL"), tp_foul),
                tp_foul = ifelse(cod == '_6(12p)', stringr::str_extract(desc, "L\\.B\\.FOUL"), tp_foul),
                tp_foul = ifelse(cod == '_6(13a)', stringr::str_extract(desc, "OFF\\.Foul|Offensive Charge Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(14a)', stringr::str_extract(desc, "IN\\.FOUL"), tp_foul),
                tp_foul = ifelse(cod == '_6(14p)', stringr::str_extract(desc, "IN\\.FOUL"), tp_foul),
                tp_foul = ifelse(cod == '_6(15a)', stringr::str_extract(desc, "AWAY\\.FROM\\.PLAY\\.FOUL"), tp_foul),
                tp_foul = ifelse(cod == '_6(15p)', stringr::str_extract(desc, "AWAY\\.FROM\\.PLAY\\.FOUL"), tp_foul),
                tp_foul = ifelse(cod == '_6(16a)', stringr::str_extract(desc, "PUNCH\\.FOUL"), tp_foul),
                tp_foul = ifelse(cod == '_6(17a)', stringr::str_extract(desc, "C\\.P\\.FOUL"), tp_foul),
                tp_foul = ifelse(cod == '_6(17p)', stringr::str_extract(desc, "C\\.P\\.FOUL"), tp_foul),
                tp_foul = ifelse(cod == '_6(18a)', stringr::str_extract(desc, " Foul : Double Personal"), tp_foul),
                tp_foul = ifelse(cod == '_6(19a)', stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE1"), tp_foul),
                tp_foul = ifelse(cod == '_6(19p)', stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE1"), tp_foul),
                tp_foul = ifelse(cod == '_6(20a)', stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE2"), tp_foul),
                tp_foul = ifelse(cod == '_6(20p)', stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE2"), tp_foul),
                tp_foul = ifelse(cod == '_6(21a)', stringr::str_extract(desc, "Personal Take Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(21p)', stringr::str_extract(desc, "Personal Take Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(22a)', stringr::str_extract(desc, "Transition Take Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(22p)', stringr::str_extract(desc, "Transition Take Foul"), tp_foul),
                tp_foul = ifelse(cod == '_6(23a)', stringr::str_extract(desc, "Taunting"), tp_foul),
                tp_foul = ifelse(cod == '_6(23p)', stringr::str_extract(desc, "Taunting"), tp_foul),
                tp_foul = ifelse(cod == '_6(4cc)', stringr::str_extract(desc, "Foul\\:DOUBLE\\.TECHNICAL\\.FOUL|Double Technical \\-"), tp_foul),
                tp_foul = ifelse(cod == '_6(4pc)', stringr::str_extract(desc, "Foul\\:DOUBLE\\.TECHNICAL\\.FOUL|Double Technical \\-"), tp_foul),
                tp_foul = ifelse(cod == '_6(4pp)', stringr::str_extract(desc, "Double Technical \\-"), tp_foul)) %>% 
  dplyr::mutate(desc = ifelse(!is.na(tp_foul), stringr::str_replace(desc, tp_foul, '[FOUL]'), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(stringr::str_detect(cod, "_6\\((01|10|11|12|13|14|15|16|17|19|20|21|22|23)[a|p]\\)") & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(Pcnt\\.Tcnt\\)$"), 1, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\((01|10|11|12|13|14|15|16|17|19|20|21|22|23)[a|p]\\)") & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(Pcnt\\.PN\\)$"), 1, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\((01|10|11|12|13|14|15|16|17|19|20|21|22|23)[a|p]\\)") & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(Pcnt\\.Tcnt\\)\\s+\\(.+?\\)$"), 2, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\((01|10|11|12|13|14|15|16|17|19|20|21|22|23)[a|p]\\)") & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(Pcnt\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\((01c|4cc|4pc)\\)") & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]$"), 3, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\((01c|4cc|4pc)\\)") & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(.+?\\)$"), 4, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\((01t|06t|08t|09t)\\)") & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]$"), 5, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\((01t|06t|08t|09t)\\)") & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(.+?\\)$"), 6, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\(05p\\)") & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(Def\\.\\s+3\\s+Sec\\s+.+?\\)$"), 7, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\(13a\\)") & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(Pcnt\\)$"), 8, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\(13a\\)") & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(Pcnt\\)\\s+\\(.+?\\)$"), 9, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\(18a\\)") & stringr::str_detect(desc, "^\\[FOUL\\]\\s+\\-\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\;\\s+(.+?)\\s+\\(cnt\\s+PF\\)$"), 10, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\(18a\\)") & stringr::str_detect(desc, "^\\[FOUL\\]\\s+\\-\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\;\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\s+\\(.+?\\)$"), 11, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\((4pc|4pp)\\)") & stringr::str_detect(desc, "^\\[FOUL\\]\\s+(.+?)\\;\\s+(.+?)$"), 12, sintaxe),
                sintaxe = ifelse(stringr::str_detect(cod, "_6\\((4pc|4pp)\\)") & stringr::str_detect(desc, "^\\[FOUL\\]\\s+(.+?)\\;\\s+(.+?)\\s+\\(.+?\\)$"), 13, sintaxe))

# 
#   dplyr::mutate(sintaxe = ifelse(cod %in% c('_6(10a)', '_6(11a)', '_6(12a)', '_6(14a)', '_6(15a)', '_6(16a)', '_6(17a)', '_6(19a)', '_6(20a)', '_6(21a)', '_6(22a)', '_6(23a)') & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(Pcnt\\.Tcnt\\)$"), 1, sintaxe),
#                 sintaxe = ifelse(cod %in% c('_6(10p)', '_6(11p)', '_6(12p)', '_6(14p)', '_6(15p)', '_6(16p)', '_6(17p)', '_6(19p)', '_6(20p)', '_6(21p)', '_6(22p)', '_6(23p)') & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(Pcnt\\.PN\\)$"), 1, sintaxe),
#                 sintaxe = ifelse(cod %in% c('_6(10a)', '_6(11a)', '_6(12a)', '_6(14a)', '_6(15a)', '_6(16a)', '_6(17a)', '_6(19a)', '_6(20a)', '_6(21a)', '_6(22a)', '_6(23a)') & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(Pcnt\\.Tcnt\\)\\s+\\(.+?\\)$"), 2, sintaxe),
#                 sintaxe = ifelse(cod %in% c('_6(10p)', '_6(11p)', '_6(12p)', '_6(14p)', '_6(15p)', '_6(16p)', '_6(17p)', '_6(19p)', '_6(20p)', '_6(21p)', '_6(22p)', '_6(23p)') & stringr::str_detect(desc, "(^.+?)\\s+\\[FOUL\\]\\s+\\(Pcnt\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe),
#                 )



df0 %>% 
  dplyr::group_split(sintaxe) %>% '[['(1) %>% dplyr::group_split(cod)

df0 %>% 
  dplyr::group_split(sintaxe)

df0 %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  print(., n = 900)

df0 %>% 
  dplyr::group_by(tp_rnb) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  print(., n = 900)




df0 %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  print(., n = 100)



xdf[[113]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Foul\\:T\\.FOUL|Foul\\:Non\\-Unsportsmanlike"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(coach = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])$", "[coach] \\2"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)", "[coach] \\2 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[114]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+|P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "T\\.FOUL|HANGING\\.TECH\\.FOUL|Non-Unsportsmanlike Tech Foul - Flopping|Non-Unsportsmanlike"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[115]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Foul|T\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])$", "[team] \\2"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[117]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "T.Foul"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(player = stringr::str_extract(desc, "\\(.+?\\)"),
                player = stringr::str_remove_all(player, '\\(Def\\. 3 Sec\\s+|\\s+\\)')) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+\\(Def\\.\\s+3\\s+Sec\\s+.+?\\)$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+\\(Def\\.\\s+3\\s+Sec\\s+.+?\\)$", "[team] \\2 (Def. 3 Sec [player])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[118]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Foul|Delay"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])$", "[team] \\2"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[120]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Excess Timeout Technical"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])$", "[team] \\2"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[122]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Too Many Players Tech Foul"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])$", "[team] \\2"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[123]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "P\\.FOUL|Personal Block Foul"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[124]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "P\\.FOUL|Personal Block Foul"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[125]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "S\\.FOUL|Shooting Block Foul"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[126]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "S\\.FOUL|Shooting Block Foul"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[127]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "L\\.B\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[128]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "L\\.B\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[129]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN|P[0-9]+\\.T[0-9]+|P[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "OFF\\.Foul|Offensive Charge Foul"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[130]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "IN\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[131]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "IN\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[132]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "AWAY\\.FROM\\.PLAY\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[133]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "AWAY\\.FROM\\.PLAY\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[134]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "PUNCH\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[135]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
dplyr::mutate(tp_foul = stringr::str_extract(desc, "C\\.P\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

load("D:/Mestrado/NBA/data/pbpc_total.RData")
load("D:/Mestrado/NBA/pbp_players_pattern.RData")

remove_players_name_pbp <- function(df){
  
  
  player_pattern <- pbp_players_pattern %>% 
    dplyr::filter(game_id == df$game_id[1]) %>% 
    dplyr::pull(players_pattern)
  
  
  df %>% 
    dplyr::mutate(desc = stringr::str_replace_all(description, player_pattern)) %>% 
    dplyr::mutate(desc = stringr::str_replace_all(desc, "[0-9]+\\'", "[dist]"), 
                  desc = stringr::str_replace_all(desc, "[0-9]+\\s+(PTS|AST|BLK|STL|PF)", "cnt \\1"), 
                  desc = stringr::str_replace_all(desc, "P[0-9]+\\.T[0-9]+", "Pcnt.Tcnt"), 
                  desc = stringr::str_replace_all(desc, "P[0-9]+\\.PN", "Pcnt.PN"), 
                  desc = stringr::str_replace_all(desc, "T#[0-9]+", "T#cnt")) %>% 
    dplyr::select(game_id, period, clock, description, desc)
  
}


future::plan(future::multisession(), 
             workers = future::availableCores())

pbp_clean <- pbpc_total %>% 
  dplyr::group_split(game_id) %>% 
  furrr::future_map(.x = ., .f = remove_players_name_pbp, .progress = TRUE) %>% 
  dplyr::bind_rows()

future::plan(future::sequential())

xdf[[136]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "C\\.P\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[137]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul1 = stringr::str_extract(desc, "[0-9]+\\s+PF"),
                desc = ifelse(!is.na(cnt_foul1), stringr::str_replace(desc, cnt_foul1, "cnt PF"), desc),
                cnt_foul2 = stringr::str_extract(desc, "[0-9]+\\s+PF"),
                desc = ifelse(!is.na(cnt_foul2), stringr::str_replace(desc, cnt_foul2, "cnt PF"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, " Foul : Double Personal"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player1 = stringr::str_extract(desc, "\\-\\s+(.+?)\\s+\\("),
                player1 = stringr::str_remove_all(player1, "\\-\\s+|\\s+\\(")) %>% 
  dplyr::mutate(player2 = stringr::str_extract(desc, "\\;\\s+(.+?)\\s+\\("),
                player2 = stringr::str_remove_all(player2, "\\;\\s+|\\s+\\(")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "\\[FOUL\\]\\s+\\-\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\;\\s+(.+?)\\s+\\(cnt\\s+PF\\)$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "\\[FOUL\\]\\s+\\-\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\;\\s+(.+?)\\s+\\(cnt\\s+PF\\)$", "[FOUL] - [player] (cnt PF); [player] (cnt PF)"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "\\[FOUL\\]\\s+\\-\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\;\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\s+\\(.+?\\)$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "\\[FOUL\\]\\s+\\-\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\;\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\s+\\(.+?\\)$", "[FOUL] - [player] (cnt PF); [player] (cnt PF) ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[138]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE1"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[139]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE1"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[140]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE2"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[141]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE2"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[142]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Personal Take Foul"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[143]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Personal Take Foul"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[144]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Transition Take Foul"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[145]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Transition Take Foul"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[146]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Taunting"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[147]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Taunting"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[148]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Foul\\:DOUBLE\\.TECHNICAL\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(coach = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)", "[player] \\2 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[149]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Foul\\:DOUBLE\\.TECHNICAL\\.FOUL"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(coach = ifelse(!is.na(tp_foul), stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1"), NA_character_)) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\]$)"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\]$)", "[player] \\2"), desc)) %>% 
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)", "[player] \\2 ([referee])"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 

xdf[[150]] %>% 
  dplyr::select(game_id, description) %>%
  dplyr::mutate(desc = description) %>% 
  dplyr::mutate(tp_foul = stringr::str_extract(desc, "Double Technical \\-"),
                desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
  dplyr::mutate(player1 = stringr::str_extract(desc, "\\]\\s+(.+?)\\;"),
                player1 = stringr::str_remove_all(player1, "\\]\\s+|\\;")) %>% 
  dplyr::mutate(player2 = stringr::str_extract(desc, "\\;\\s+(.+?$)"),
                player2 = stringr::str_remove_all(player2, "\\;\\s+")) %>% 
  dplyr::mutate(sintaxe = 0) %>%
  dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "\\[FOUL\\]\\s+(.+?)\\;\\s+(.+?)$"), 1, sintaxe)) %>% 
  dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "\\[FOUL\\]\\s+(.+?)\\;\\s+(.+?)$", "[FOUL] [player]; [player]"), desc)) %>% 
  dplyr::group_by(sintaxe, desc) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
  dplyr::arrange(desc(n)) %>% 
  print(., n = 100) 








df1 %>% 
  dplyr::select(sintaxe, cod, description, desc, cnt_tnv, cnt_stl, tp_tnv, player, player_stl, team) %>% 
  dplyr::group_split(sintaxe)

df1 %>% 
  dplyr::mutate(player_stl = stringr::str_extract(desc, "\\)\\s+(.+?)\\s+STEAL"))%>% 
  dplyr::select(cod, desc, cnt_tnv, cnt_stl, tp_tnv, player, player_stl)



{

  
 
  
  
  xdf[[11]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to (.+)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to (.+)", "Jump Ball [player] vs. [player]: Tip to [player]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "Jump Ball .+? vs\\. .+?\\: Tip\\s+to", "Jump Ball [player] vs. [player]: Tip to"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[12]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to (.+)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to (.+)", "Jump Ball [player] vs. [player]: Tip to [player]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "Jump Ball .+? vs\\. .+?\\: Tip\\s+to", "Jump Ball [player] vs. [player]: Tip to"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[13]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball \\(CC\\) (.+?) vs\\. (.+?)\\: Tip to (.+)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to (.+)", "Jump Ball (CC) [player] vs. [player]: Tip to [player]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball \\(CC\\) (.+?) vs\\. (.+?)\\: Tip to"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "Jump Ball .+? vs\\. .+?\\: Tip\\s+to", "Jump Ball (CC) [player] vs. [player]: Tip to"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[14]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>%
    dplyr::mutate(tp_ejection = stringr::str_extract(desc, "Ejection\\:Other|Ejection\\:Second Technical|Ejection\\:No Ejection|Ejection\\:Second Flagrant Type 1|Ejection:First Flagrant Type 2|Ejection\\:"),
                  desc = stringr::str_replace(desc, tp_ejection, "[EJECTION]")) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?) \\[EJECTION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?) (\\[EJECTION\\]$)", "[player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[15]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>%
    dplyr::mutate(tp_ejection = stringr::str_extract(desc, "Ejection\\:Other|Ejection\\:Second Technical|Ejection\\:No Ejection|Ejection\\:Second Flagrant Type 1|Ejection:First Flagrant Type 2|Ejection\\:"),
                  desc = stringr::str_replace(desc, tp_ejection, "[EJECTION]")) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?) \\[EJECTION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?) (\\[EJECTION\\]$)", "[coach] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[16]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[17]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[18]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[19]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[20]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[21]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[22]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[23]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[24]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[25]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[26]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[27]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[28]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[29]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[30]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[31]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[32]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[33]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  

 
  xdf[[151]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:No Violation"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[152]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\: Delay of game Violation"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(team = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  team = stringr::str_remove_all(team, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[team] [VIOLATION]"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[153]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:Defensive Goaltending"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$", "[player] [VIOLATION] ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[154]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:Lane"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$", "[player] [VIOLATION] ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[155]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:Jump Ball"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$", "[player] [VIOLATION] ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[156]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:Kicked Ball"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$", "[player] [VIOLATION] ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[157]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:Double Lane"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$", "[player] [VIOLATION] ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[158]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(player1 = stringr::str_extract(desc, "SUB\\:\\s+(.+?)\\s+FOR"),
                  player1 = stringr::str_remove_all(player1, "SUB\\:\\s+|\\s+FOR")) %>% 
    dplyr::mutate(player2 = stringr::str_extract(desc, "FOR\\s+(.+?)$"),
                  player2 = stringr::str_remove_all(player2, "FOR\\s+")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "SUB\\:\\s+(.+?)\\s+FOR\\s+(.+?)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "SUB\\:\\s+(.+?)\\s+FOR\\s+(.+?)$", "SUB: [player] FOR [player]"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[159]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "Full\\s+[0-9]+\\s+Short\\s+[0-9]+|Reg\\.[0-9]+\\s+Short\\s+[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Full cnt Short cnt"), desc)) %>% 
    dplyr::mutate(tp_tout = stringr::str_extract(desc, "Timeout\\:\\s+Regular|Timeout\\:\\s+Short|Timeout\\:Short|Timeout\\:Regular"),
                  desc = ifelse(is.na(tp_tout), desc, stringr::str_replace(desc, tp_tout, "[TIMEOUT]"))) %>% 
    dplyr::mutate(team = stringr::str_extract(desc, "(^.+?)\\["),
                  team = stringr::str_remove_all(team, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$", "[team] [TIMEOUT] (Full cnt Short cnt)"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[160]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "Full\\s+[0-9]+\\s+Short\\s+[0-9]+|Reg\\.[0-9]+\\s+Short\\s+[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Full cnt Short cnt"), desc)) %>% 
    dplyr::mutate(tp_tout = stringr::str_extract(desc, "Timeout\\: Coach Challenge"),
                  desc = ifelse(is.na(tp_tout), desc, stringr::str_replace(desc, tp_tout, "[TIMEOUT]"))) %>% 
    dplyr::mutate(team = stringr::str_extract(desc, "(^.+?)\\["),
                  team = stringr::str_remove_all(team, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$", "[team] [TIMEOUT] (Full cnt Short cnt)"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[161]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_tout = stringr::str_extract(desc, "Timeout\\: Official"),
                  desc = ifelse(is.na(tp_tout), desc, stringr::str_replace(desc, tp_tout, "[TIMEOUT]"))) %>% 
    dplyr::mutate(sintaxe = ifelse(desc == '[TIMEOUT]', 1, 0)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[162]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "Full\\s+[0-9]+\\s+Short\\s+[0-9]+|Reg\\.[0-9]+\\s+Short\\s+[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Full cnt Short cnt"), desc)) %>% 
    dplyr::mutate(tp_tout = stringr::str_extract(desc, "Timeout: No Timeout|Timeout: Timeout"),
                  desc = ifelse(is.na(tp_tout), desc, stringr::str_replace(desc, tp_tout, "[TIMEOUT]"))) %>% 
    dplyr::mutate(team = stringr::str_extract(desc, "(^.+?)\\["),
                  team = stringr::str_remove_all(team, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$", "[team] [TIMEOUT] (Full cnt Short cnt)"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  
  ##################
  
  xdf[[109]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Foul:Non-Unsportsmanlike", "TECH_FOUL"),
                  description = stringr::str_replace(description, "Foul:T.FOUL", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)$", "[coach] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+(.+?)$", "[coach] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[110]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "HANGING.TECH.FOUL", "TECH_FOUL"),
                  description = stringr::str_replace(description, "Non-Unsportsmanlike", "TECH_FOUL"),
                  description = stringr::str_replace(description, "T.FOUL", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+.+$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+.+", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 3, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 3, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.PN\\)", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+.+$"), 4, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 4, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+.+", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[111]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Foul", "TECH_FOUL"),
                  description = stringr::str_replace(description, "T.FOUL", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)$", "[team] \\2"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[113]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "T.FOUL", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.PN\\)", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+.+$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+.+", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[114]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Foul:DOUBLE.TECHNICAL.FOUL", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+(.+?)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+(.+?)$", "[coach] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[115]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Foul:DOUBLE.TECHNICAL.FOUL", "TECH_FOUL"),
                  description = stringr::str_replace(description, "^Double Technical -", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)$", "[coach] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+(.+?)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+(.+?)$", "[coach] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(TECH_FOUL)\\s+(.+?)\\;\\s+(.+?)\\s+(\\(.+?\\))$"), 3, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 3, stringr::str_replace(description, "(TECH_FOUL)\\s+(.+?)\\;\\s+(.+?)\\s+(\\(.+?\\)$)", "\\1 [coach], [player] ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[116]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "^Double Technical -", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(TECH_FOUL)\\s+(.+?)\\;\\s+(.+?)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(TECH_FOUL)\\s+(.+?)\\;\\s+(.+?)$", "\\1 [player], [player]"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(TECH_FOUL)\\s+(.+?)\\;\\s+(.+?)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(TECH_FOUL)\\s+(.+?)\\,\\s+(.+?)\\s+(\\(.+?\\))$", "\\1 [player], [player] ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[117]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "T.Foul", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(Def\\. 3 Sec .+? \\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(Def\\. 3 Sec .+? \\)", "[team] \\2 (Def. 3 Sec [player] )"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(Def\\. 3 Sec .+? \\) \\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(Def\\. 3 Sec .+? \\) \\(.+?\\)", "[team] \\2 (Def. 3 Sec [player] ) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[118]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Delay", "TECH_FOUL"),
                  description = stringr::str_replace(description, "Foul$", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)", "[team] \\2"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[120]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Excess Timeout Technical", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)", "[team] \\2"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[122]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Too Many Players Tech Foul", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)", "[team] \\2"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[123]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "P.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[124]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "P.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[125]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "S.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[126]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "S.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[127]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "L.B.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[128]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "L.B.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[129]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "OFF.Foul", "FOUL"),
                  description = stringr::str_replace(description, "Offensive Charge Foul", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\)$", "[player] \\2 (Pcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 3, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 3, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\)\\s+\\(.+?\\)$"), 4, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 4, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt) ([referee])"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 5, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 5, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 6, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 6, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[130]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "AWAY.FROM.PLAY.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[131]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "AWAY.FROM.PLAY.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[132]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "PUNCH.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[133]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "C.P.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[134]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "C.P.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[135]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, " Foul : Double Personal -", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(FOUL)\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\;\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\s+\\(.+?\\)"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(FOUL)\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\;\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\s+\\(.+?\\)", "\\1 [player] (cnt PF); [player] (cnt PF) ([referee])"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(FOUL)\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\;\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(FOUL)\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\;\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)$", "\\1 [player] (cnt PF); [player] (cnt PF)"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[136]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "FLAGRANT.FOUL.TYPE1", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[137]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "FLAGRANT.FOUL.TYPE1", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[138]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "FLAGRANT.FOUL.TYPE2", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[139]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "FLAGRANT.FOUL.TYPE2", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[140]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Personal Take Foul", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[141]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Personal Take Foul", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[142]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Transition Take Foul", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[143]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Transition Take Foul", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[144]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Violation: Delay of game Violation", "VIOLATION")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)", "[team] \\2"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[146]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Violation:Defensive Goaltending", "VIOLATION")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)$", "[player] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)\\s+(.+?)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)(.+?)$", "[player] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[147]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Violation:Lane", "VIOLATION")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)$", "[player] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)\\s+(.+?)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)(.+?)$", "[player] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[148]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Violation:Jump Ball", "VIOLATION")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)$", "[player] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)\\s+(.+?)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)(.+?)$", "[player] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[149]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Violation:Kicked Ball", "VIOLATION")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)$", "[player] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)\\s+(.+?)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)(.+?)$", "[player] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[151]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "SUB\\:\\s+(.+?)\\s+FOR\\s+(.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "SUB\\:\\s+(.+?)\\s+FOR\\s+(.+?$)", "SUB: [player] FOR [player]"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[152]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Timeout\\:\\s+Regular", "TIMEOUT")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TIMEOUT\\s+\\(Full\\s+[0-9]+\\s+Short\\s+[0-9]+\\)"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+TIMEOUT\\s+\\(Full\\s+[0-9]+\\s+Short\\s+[0-9]+\\)", "[team] TIMEOUT (Full cnt Short cnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TIMEOUT\\s+\\(Reg\\.[0-9]+\\s+Short\\s+[0-9]+\\)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+TIMEOUT\\s+\\(Reg\\.[0-9]+\\s+Short\\s+[0-9]+\\)", "[team] TIMEOUT (Reg.cnt Short cnt)"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[153]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Timeout\\:\\s+Coach\\s+Challenge", "TIMEOUT")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TIMEOUT\\s+\\(Full\\s+[0-9]+\\s+Short\\s+[0-9]+\\)"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+TIMEOUT\\s+\\(Full\\s+[0-9]+\\s+Short\\s+[0-9]+\\)", "[team] TIMEOUT (Full cnt Short cnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TIMEOUT\\s+\\(Reg\\.[0-9]+\\s+Short\\s+[0-9]+\\)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+TIMEOUT\\s+\\(Reg\\.[0-9]+\\s+Short\\s+[0-9]+\\)", "[team] TIMEOUT (Reg.cnt Short cnt)"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  # 
  xdf[[153]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  }
{
  xdf[[1]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = stringr::str_replace(desc, cnt_pts, "cnt PTS")) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Running Alley Oop Dunk Shot|Alley Oop Dunk|Cutting Dunk Shot|Driving Dunk|Driving Reverse Dunk Shot|Driving Slam Dunk|Dunk Shot|Putback Dunk|Putback Slam Dunk|Reverse Dunk|Reverse Slam Dunk|Running Dunk|Running Reverse Dunk Shot|Running Slam Dunk|Slam Dunk|Tip Dunk Shot|Dunk"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[DUNK_SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[DUNK_SHOT\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[DUNK_SHOT\\])\\s+(\\(cnt PTS\\))", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[DUNK_SHOT\\])\\s+(\\(cnt PTS\\))"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[DUNK_SHOT\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[2]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = stringr::str_replace(desc, cnt_pts, "cnt PTS")) %>% 
    dplyr::mutate(cnt_ast = stringr::str_extract(desc, "[0-9]+\\s+AST"),
                  desc = stringr::str_replace(desc, cnt_ast, "cnt AST")) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Running Alley Oop Dunk Shot|Alley Oop Dunk|Cutting Dunk Shot|Driving Dunk|Driving Reverse Dunk Shot|Driving Slam Dunk|Dunk Shot|Putback Dunk|Putback Slam Dunk|Reverse Dunk|Reverse Slam Dunk|Running Dunk|Running Reverse Dunk Shot|Running Slam Dunk|Slam Dunk|Tip Dunk Shot|Dunk"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[DUNK_SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(desc = stringr::str_replace(desc, "\\(.+?\\s+cnt\\s+AST\\)", "([player] cnt AST)")) %>%  
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[DUNK_SHOT\\])\\s+(.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[DUNK_SHOT\\])\\s+(.+?$)", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[DUNK_SHOT\\])\\s+(.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[DUNK_SHOT\\])\\s+(.+?$)", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[3]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = stringr::str_replace(desc, cnt_pts, "cnt PTS")) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Driving Hook Shot|Driving Bank Hook Shot|Hook Bank Shot|Jump Hook Shot|Jump Bank Hook Shot|Running Bank Hook Shot|Running Hook Shot|Turnaround Hook Shot|Turnaround Bank Hook Shot|Hook Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[HOOK_SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[HOOK_SHOT\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[HOOK_SHOT\\])\\s+(\\(cnt PTS\\))", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[HOOK_SHOT\\])\\s+(\\(cnt PTS\\))"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[HOOK_SHOT\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)

  xdf[[4]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = stringr::str_replace(desc, cnt_pts, "cnt PTS")) %>% 
    dplyr::mutate(cnt_ast = stringr::str_extract(desc, "[0-9]+\\s+AST"),
                  desc = stringr::str_replace(desc, cnt_ast, "cnt AST")) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Driving Hook Shot|Driving Bank Hook Shot|Hook Bank Shot|Jump Hook Shot|Jump Bank Hook Shot|Running Bank Hook Shot|Running Hook Shot|Turnaround Hook Shot|Turnaround Bank Hook Shot|Hook Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[HOOK_SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(desc = stringr::str_replace(desc, "\\(.+?\\s+cnt\\s+AST\\)", "([player] cnt AST)")) %>%  
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[HOOK_SHOT\\])\\s+(.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[HOOK_SHOT\\])\\s+(.+?$)", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[HOOK_SHOT\\])\\s+(.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[HOOK_SHOT\\])\\s+(.+?$)", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)

  xdf[[5]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = stringr::str_replace(desc, cnt_pts, "cnt PTS")) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Driving Reverse Layup|Running Layup|Driving Finger Roll Layup|Driving Layup|Cutting Layup Shot|Cutting Finger Roll Layup Shot|Putback Layup|Running Alley Oop Layup Shot|Tip Layup Shot|Tip Shot|Layup"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[LAYUP_SHOT]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[LAYUP_SHOT\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[LAYUP_SHOT\\])\\s+(\\(cnt PTS\\))", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[LAYUP_SHOT\\])\\s+(\\(cnt PTS\\))"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[LAYUP_SHOT\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)

  xdf[[6]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = stringr::str_replace(desc, cnt_pts, "cnt PTS")) %>% 
    dplyr::mutate(cnt_ast = stringr::str_extract(desc, "[0-9]+\\s+AST"),
                  desc = stringr::str_replace(desc, cnt_ast, "cnt AST")) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Driving Reverse Layup|Running Layup|Driving Finger Roll Layup|Driving Layup|Cutting Layup Shot|Cutting Finger Roll Layup Shot|Putback Layup|Running Alley Oop Layup Shot|Tip Layup Shot|Tip Shot|Layup"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[LAYUP_SHOT]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(desc = stringr::str_replace(desc, "\\(.+?\\s+cnt\\s+AST\\)", "([player] cnt AST)")) %>%  
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[LAYUP_SHOT\\])\\s+(.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[LAYUP_SHOT\\])\\s+(.+?$)", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[LAYUP_SHOT\\])\\s+(.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[LAYUP_SHOT\\])\\s+(.+?$)", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  
  xdf[[7]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = stringr::str_replace(desc, cnt_pts, "cnt PTS")) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Turnaround Fadeaway Shot|Turnaround Fadeaway Bank Jump Shot|Driving Floating Bank Jump Shot|Driving Floating Jump Shot|Running Pull-Up Jump Shot|Step Back Bank Jump Shot|Turnaround Bank Shot|Turnaround Jump Shot|Step Back Jump Shot|Turnaround Fadeaway|Fadeaway Bank Shot|Floating Jump Shot|Driving Bank Shot|Driving Jump Shot|Running Bank Shot|Running Jump Shot|Pullup Bank Shot|Pullup Jump Shot|Fadeaway Jumper|Jump Bank Shot|Jump Shot|No Shot|Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[JUMPSHOT]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[JUMPSHOT\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[JUMPSHOT\\])\\s+(\\(cnt PTS\\))", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[JUMPSHOT\\])\\s+(\\(cnt PTS\\))"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[JUMPSHOT\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[8]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = stringr::str_replace(desc, cnt_pts, "cnt PTS")) %>% 
    dplyr::mutate(cnt_ast = stringr::str_extract(desc, "[0-9]+\\s+AST"),
                  desc = stringr::str_replace(desc, cnt_ast, "cnt AST")) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Turnaround Fadeaway Shot|Turnaround Fadeaway Bank Jump Shot|Driving Floating Bank Jump Shot|Driving Floating Jump Shot|Running Pull-Up Jump Shot|Step Back Bank Jump Shot|Turnaround Bank Shot|Turnaround Jump Shot|Step Back Jump Shot|Turnaround Fadeaway|Fadeaway Bank Shot|Floating Jump Shot|Driving Bank Shot|Driving Jump Shot|Running Bank Shot|Running Jump Shot|Pullup Bank Shot|Pullup Jump Shot|Fadeaway Jumper|Jump Bank Shot|Jump Shot|No Shot|Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[JUMPSHOT]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(desc = stringr::str_replace(desc, "\\(.+?\\s+cnt\\s+AST\\)", "([player] cnt AST)")) %>%  
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[JUMPSHOT\\])\\s+(.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[JUMPSHOT\\])\\s+(.+?$)", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[JUMPSHOT\\])\\s+(.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[JUMPSHOT\\])\\s+(.+?$)", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[9]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = stringr::str_replace(desc, cnt_pts, "cnt PTS")) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "3PT Hook Bank Shot|3PT Layup|3PT Driving Hook Shot|3PT Driving Bank Hook Shot|3PT Running Layup|3PT Turnaround Hook Shot|3PT Reverse Layup|3PT Tip Shot|3PT Running Hook Shot|3PT Dunk|3PT Jump Hook Shot|3PT Hook Shot|3PT Turnaround Fadeaway Shot|3PT Running Bank Hook Shot|3PT Turnaround Bank Hook Shot|3PT Turnaround Fadeaway Bank Jump Shot|3PT Driving Floating Bank Jump Shot|3PT Driving Floating Jump Shot|3PT Running Pull-Up Jump Shot|3PT Step Back Bank Jump Shot|3PT Turnaround Bank Shot|3PT Turnaround Jump Shot|3PT Step Back Jump Shot|3PT Turnaround Fadeaway|3PT Fadeaway Bank Shot|3PT Floating Jump Shot|3PT Driving Bank Shot|3PT Driving Jump Shot|3PT Running Bank Shot|3PT Running Jump Shot|3PT Pullup Bank Shot|3PT Pullup Jump Shot|3PT Fadeaway Jumper|3PT Jump Bank Shot|3PT Putback Layup|3PT Jump Shot|3PT No Shot|3PT Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[SHOT]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(\\(cnt PTS\\))", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[SHOT\\])\\s+(\\(cnt PTS\\))"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[SHOT\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[10]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = stringr::str_replace(desc, cnt_pts, "cnt PTS")) %>% 
    dplyr::mutate(cnt_ast = stringr::str_extract(desc, "[0-9]+\\s+AST"),
                  desc = stringr::str_replace(desc, cnt_ast, "cnt AST")) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "3PT Hook Bank Shot|3PT Layup|3PT Driving Hook Shot|3PT Driving Bank Hook Shot|3PT Running Layup|3PT Turnaround Hook Shot|3PT Reverse Layup|3PT Tip Shot|3PT Running Hook Shot|3PT Dunk|3PT Jump Hook Shot|3PT Hook Shot|3PT Turnaround Fadeaway Shot|3PT Running Bank Hook Shot|3PT Turnaround Bank Hook Shot|3PT Turnaround Fadeaway Bank Jump Shot|3PT Driving Floating Bank Jump Shot|3PT Driving Floating Jump Shot|3PT Running Pull-Up Jump Shot|3PT Step Back Bank Jump Shot|3PT Turnaround Bank Shot|3PT Turnaround Jump Shot|3PT Step Back Jump Shot|3PT Turnaround Fadeaway|3PT Fadeaway Bank Shot|3PT Floating Jump Shot|3PT Driving Bank Shot|3PT Driving Jump Shot|3PT Running Bank Shot|3PT Running Jump Shot|3PT Pullup Bank Shot|3PT Pullup Jump Shot|3PT Fadeaway Jumper|3PT Jump Bank Shot|3PT Putback Layup|3PT Jump Shot|3PT No Shot|3PT Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[SHOT]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(desc = stringr::str_replace(desc, "\\(.+?\\s+cnt\\s+AST\\)", "([player] cnt AST)")) %>%  
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(.+?$)", "[player] [dist] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[SHOT\\])\\s+(.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[SHOT\\])\\s+(.+?$)", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[11]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to (.+)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to (.+)", "Jump Ball [player] vs. [player]: Tip to [player]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "Jump Ball .+? vs\\. .+?\\: Tip\\s+to", "Jump Ball [player] vs. [player]: Tip to"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[12]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to (.+)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to (.+)", "Jump Ball [player] vs. [player]: Tip to [player]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "Jump Ball .+? vs\\. .+?\\: Tip\\s+to", "Jump Ball [player] vs. [player]: Tip to"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[13]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball \\(CC\\) (.+?) vs\\. (.+?)\\: Tip to (.+)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "Jump Ball (.+?) vs\\. (.+?)\\: Tip to (.+)", "Jump Ball (CC) [player] vs. [player]: Tip to [player]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "Jump Ball \\(CC\\) (.+?) vs\\. (.+?)\\: Tip to"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "Jump Ball .+? vs\\. .+?\\: Tip\\s+to", "Jump Ball (CC) [player] vs. [player]: Tip to"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[14]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>%
    dplyr::mutate(tp_ejection = stringr::str_extract(desc, "Ejection\\:Other|Ejection\\:Second Technical|Ejection\\:No Ejection|Ejection\\:Second Flagrant Type 1|Ejection:First Flagrant Type 2|Ejection\\:"),
                  desc = stringr::str_replace(desc, tp_ejection, "[EJECTION]")) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?) \\[EJECTION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?) (\\[EJECTION\\]$)", "[player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[15]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>%
    dplyr::mutate(tp_ejection = stringr::str_extract(desc, "Ejection\\:Other|Ejection\\:Second Technical|Ejection\\:No Ejection|Ejection\\:Second Flagrant Type 1|Ejection:First Flagrant Type 2|Ejection\\:"),
                  desc = stringr::str_replace(desc, tp_ejection, "[EJECTION]")) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?) \\[EJECTION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?) (\\[EJECTION\\]$)", "[coach] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[16]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[17]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[18]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[19]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[20]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[21]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[22]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[23]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[24]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[25]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[26]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[27]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[28]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[29]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[30]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[31]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[32]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[33]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = stringr::str_replace(desc, "\\([0-9]+\\:[0-9]+\\s+[A|P]M", "(HOUR")) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  

  xdf[[34]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Running Alley Oop Dunk Shot|Alley Oop Dunk|Cutting Dunk Shot|Driving Dunk|Driving Reverse Dunk Shot|Driving Slam Dunk|Dunk Shot|Putback Dunk|Putback Slam Dunk|Reverse Dunk|Reverse Slam Dunk|Running Dunk|Running Reverse Dunk Shot|Running Slam Dunk|Slam Dunk|Tip Dunk Shot|Dunk"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[DUNK_SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[DUNK_SHOT\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[DUNK_SHOT\\])", "MISS [player] [dist] \\2"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[DUNK_SHOT\\])"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[DUNK_SHOT\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[35]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(cnt_blk = stringr::str_extract(desc, "[0-9]+\\s+BLK"),
                  desc = stringr::str_replace(desc, cnt_blk, "cnt BLK")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Running Alley Oop Dunk Shot|Alley Oop Dunk|Cutting Dunk Shot|Driving Dunk|Driving Reverse Dunk Shot|Driving Slam Dunk|Dunk Shot|Putback Dunk|Putback Slam Dunk|Reverse Dunk|Reverse Slam Dunk|Running Dunk|Running Reverse Dunk Shot|Running Slam Dunk|Slam Dunk|Tip Dunk Shot|Dunk"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(player_blk = stringr::str_extract(desc, "\\[SHOT\\].+?\\s+BLOCK"),
                  player_blk = stringr::str_remove_all(player_blk, "\\[SHOT\\]\\s+|\\s+BLOCK")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])(.+?)\\s+(BLOCK.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK.+?$)", "MISS [player] [dist] \\2 [player] \\4"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)", "MISS [player] \\2 [player] \\4"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[36]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Driving Hook Shot|Driving Bank Hook Shot|Hook Bank Shot|Jump Hook Shot|Jump Bank Hook Shot|Running Bank Hook Shot|Running Hook Shot|Turnaround Hook Shot|Turnaround Bank Hook Shot|Hook Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])", "MISS [player] [dist] \\2"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[37]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(cnt_blk = stringr::str_extract(desc, "[0-9]+\\s+BLK"),
                  desc = stringr::str_replace(desc, cnt_blk, "cnt BLK")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Driving Hook Shot|Driving Bank Hook Shot|Hook Bank Shot|Jump Hook Shot|Jump Bank Hook Shot|Running Bank Hook Shot|Running Hook Shot|Turnaround Hook Shot|Turnaround Bank Hook Shot|Hook Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(player_blk = stringr::str_extract(desc, "\\[SHOT\\].+?\\s+BLOCK"),
                  player_blk = stringr::str_remove_all(player_blk, "\\[SHOT]\\s+|\\s+BLOCK")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])(.+?)\\s+(BLOCK.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK.+?$)", "MISS [player] [dist] \\2 [player] \\4"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)", "MISS [player] \\2 [player] \\4"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[38]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Driving Reverse Layup|Running Layup|Driving Finger Roll Layup|Driving Layup|Cutting Layup Shot|Cutting Finger Roll Layup Shot|Putback Layup|Running Alley Oop Layup Shot|Tip Layup Shot|Tip Shot|Layup"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])", "MISS [player] [dist] \\2"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[39]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(cnt_blk = stringr::str_extract(desc, "[0-9]+\\s+BLK"),
                  desc = stringr::str_replace(desc, cnt_blk, "cnt BLK")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Driving Reverse Layup|Running Layup|Driving Finger Roll Layup|Driving Layup|Cutting Layup Shot|Cutting Finger Roll Layup Shot|Putback Layup|Running Alley Oop Layup Shot|Tip Layup Shot|Tip Shot|Layup"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(player_blk = stringr::str_extract(desc, "\\[SHOT\\].+?\\s+BLOCK"),
                  player_blk = stringr::str_remove_all(player_blk, "\\[SHOT]\\s+|\\s+BLOCK")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])(.+?)\\s+(BLOCK.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK.+?$)", "MISS [player] [dist] \\2 [player] \\4"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)", "MISS [player] \\2 [player] \\4"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[40]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Turnaround Fadeaway Shot|Turnaround Fadeaway Bank Jump Shot|Driving Floating Bank Jump Shot|Driving Floating Jump Shot|Running Pull-Up Jump Shot|Step Back Bank Jump Shot|Turnaround Bank Shot|Turnaround Jump Shot|Step Back Jump Shot|Turnaround Fadeaway|Fadeaway Bank Shot|Floating Jump Shot|Driving Bank Shot|Driving Jump Shot|Running Bank Shot|Running Jump Shot|Pullup Bank Shot|Pullup Jump Shot|Fadeaway Jumper|Jump Bank Shot|Jump Shot|No Shot|Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])", "MISS [player] [dist] \\2"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[41]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(cnt_blk = stringr::str_extract(desc, "[0-9]+\\s+BLK"),
                  desc = stringr::str_replace(desc, cnt_blk, "cnt BLK")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Turnaround Fadeaway Shot|Turnaround Fadeaway Bank Jump Shot|Driving Floating Bank Jump Shot|Driving Floating Jump Shot|Running Pull-Up Jump Shot|Step Back Bank Jump Shot|Turnaround Bank Shot|Turnaround Jump Shot|Step Back Jump Shot|Turnaround Fadeaway|Fadeaway Bank Shot|Floating Jump Shot|Driving Bank Shot|Driving Jump Shot|Running Bank Shot|Running Jump Shot|Pullup Bank Shot|Pullup Jump Shot|Fadeaway Jumper|Jump Bank Shot|Jump Shot|No Shot|Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(player_blk = stringr::str_extract(desc, "\\[SHOT\\].+?\\s+BLOCK"),
                  player_blk = stringr::str_remove_all(player_blk, "\\[SHOT]\\s+|\\s+BLOCK")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])(.+?)\\s+(BLOCK.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK.+?$)", "MISS [player] [dist] \\2 [player] \\4"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)", "MISS [player] \\2 [player] \\4"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[42]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "3PT Driving Layup|3PT Hook Bank Shot|3PT Layup|3PT Driving Hook Shot|3PT Driving Bank Hook Shot|3PT Running Layup|3PT Turnaround Hook Shot|3PT Reverse Layup|3PT Tip Shot|3PT Running Hook Shot|3PT Dunk|3PT Jump Hook Shot|3PT Hook Shot|3PT Turnaround Fadeaway Shot|3PT Running Bank Hook Shot|3PT Turnaround Bank Hook Shot|3PT Turnaround Fadeaway Bank Jump Shot|3PT Driving Floating Bank Jump Shot|3PT Driving Floating Jump Shot|3PT Running Pull-Up Jump Shot|3PT Step Back Bank Jump Shot|3PT Turnaround Bank Shot|3PT Turnaround Jump Shot|3PT Step Back Jump Shot|3PT Turnaround Fadeaway|3PT Fadeaway Bank Shot|3PT Floating Jump Shot|3PT Driving Bank Shot|3PT Driving Jump Shot|3PT Running Bank Shot|3PT Running Jump Shot|3PT Pullup Bank Shot|3PT Pullup Jump Shot|3PT Fadeaway Jumper|3PT Jump Bank Shot|3PT Putback Layup|3PT Jump Shot|3PT No Shot|3PT Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])", "MISS [player] [dist] \\2"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[43]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(dist = stringr::str_extract(desc, "[0-9]+\\'"),
                  desc = stringr::str_replace(desc, "[0-9]+\\'", "[dist]")) %>% 
    dplyr::mutate(cnt_blk = stringr::str_extract(desc, "[0-9]+\\s+BLK"),
                  desc = stringr::str_replace(desc, cnt_blk, "cnt BLK")) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "3PT Driving Layup|3PT Hook Bank Shot|3PT Layup|3PT Driving Hook Shot|3PT Driving Bank Hook Shot|3PT Running Layup|3PT Turnaround Hook Shot|3PT Reverse Layup|3PT Tip Shot|3PT Running Hook Shot|3PT Dunk|3PT Jump Hook Shot|3PT Hook Shot|3PT Turnaround Fadeaway Shot|3PT Running Bank Hook Shot|3PT Turnaround Bank Hook Shot|3PT Turnaround Fadeaway Bank Jump Shot|3PT Driving Floating Bank Jump Shot|3PT Driving Floating Jump Shot|3PT Running Pull-Up Jump Shot|3PT Step Back Bank Jump Shot|3PT Turnaround Bank Shot|3PT Turnaround Jump Shot|3PT Step Back Jump Shot|3PT Turnaround Fadeaway|3PT Fadeaway Bank Shot|3PT Floating Jump Shot|3PT Driving Bank Shot|3PT Driving Jump Shot|3PT Running Bank Shot|3PT Running Jump Shot|3PT Pullup Bank Shot|3PT Pullup Jump Shot|3PT Fadeaway Jumper|3PT Jump Bank Shot|3PT Putback Layup|3PT Jump Shot|3PT No Shot|3PT Shot"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[SHOT]"))) %>%
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(player_blk = stringr::str_extract(desc, "\\[SHOT\\].+?\\s+BLOCK"),
                  player_blk = stringr::str_remove_all(player_blk, "\\[SHOT]\\s+|\\s+BLOCK")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])(.+?)\\s+(BLOCK.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+\\[dist\\]\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK.+?$)", "MISS [player] [dist] \\2 [player] \\4"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[SHOT\\])\\s+(.+?)\\s+(BLOCK\\s+.+?$)", "MISS [player] \\2 [player] \\4"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[46]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 1 of 1"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[47]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 1 of 1"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)

  xdf[[48]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 1 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)    
  
  xdf[[49]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 1 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[50]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 1 of 3"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)   
  
  xdf[[51]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 1 of 3"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[52]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 2 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[53]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 2 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[54]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 2 of 3"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[55]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 2 of 3"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[56]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 3 of 3"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[57]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw 3 of 3"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[58]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Technical 1 of 2|Free Throw Technical 2 of 2|Free Throw Technical"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[59]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Technical 1 of 2|Free Throw Technical 2 of 2|Free Throw Technical"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[60]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Flagrant 1 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[61]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Flagrant 1 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[62]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Flagrant 2 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[63]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Flagrant 2 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[64]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Flagrant 1 of 1"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[65]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Clear Path 1 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[66]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Clear Path 1 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[67]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Clear Path 2 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[68]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Clear Path 2 of 2"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[69]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Flagrant 1 of 3"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[70]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Flagrant 1 of 3"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[71]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Flagrant 2 of 3"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[72]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Flagrant 2 of 3"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[73]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_pts = stringr::str_extract(desc, "[0-9]+\\s+PTS"),
                  desc = ifelse(!is.na(cnt_pts), stringr::str_replace(desc, cnt_pts, "cnt PTS"), desc)) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Flagrant 3 of 3"),
                  desc = ifelse(is.na(tp_shot), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FREETHROW\\])\\s+(\\(cnt PTS\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[74]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_shot = stringr::str_extract(desc, "Free Throw Flagrant 3 of 3"),
                  desc = ifelse(is.na(tp_rnb), desc, stringr::str_replace(desc, tp_shot, "[FREETHROW]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "^MISS\\s+(.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "^MISS\\s+(.+?)\\s+(\\[FREETHROW\\])", "MISS [player] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[75]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_rnd = stringr::str_extract(desc, "Off\\:[0-9]+\\s+Def\\:[0-9]+"),
                  desc = ifelse(!is.na(cnt_rnd), stringr::str_replace(desc, cnt_rnd, "Off:cnt Def:cnt"), desc)) %>% 
    dplyr::mutate(tp_rnb = stringr::str_extract(desc, "REBOUND"),
                  desc = ifelse(is.na(tp_rnb), desc, stringr::str_replace(desc, tp_rnb, "[REBOUND]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[REBOUND\\])\\s+(\\(Off\\:cnt\\s+Def\\:cnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[REBOUND\\])\\s+(\\(Off\\:cnt\\s+Def\\:cnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[76]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_rnb = stringr::str_extract(desc, "Rebound"),
                  desc = ifelse(is.na(tp_rnb), desc, stringr::str_replace(desc, tp_rnb, "[REBOUND]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[REBOUND\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[REBOUND\\])", "[team] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[77]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[78]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_rnb = stringr::str_extract(desc, "Rebound"),
                  desc = ifelse(is.na(tp_rnb), desc, stringr::str_replace(desc, tp_rnb, "[REBOUND]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[REBOUND\\])"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[REBOUND\\])", "[team] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[79]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "No Turnover|Turnover Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[80]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(cnt_stl = stringr::str_extract(desc, "[0-9]+\\s+STL"),
                  desc = ifelse(!is.na(cnt_stl), stringr::str_replace(desc, cnt_stl, "cnt STL"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "No Turnover|Turnover Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(player_stl = stringr::str_extract(desc, "\\(Pcnt.Tcnt\\).+?\\s+STEAL"),
                  player_stl = stringr::str_remove_all(player_stl, "\\(Pcnt.Tcnt\\)\\s+|\\s+STEAL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(.+?)\\s+(STEAL.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(.+?)\\s+(STEAL.+?$)", "[player] \\2 \\3 [player] \\5"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[81]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(cnt_stl = stringr::str_extract(desc, "[0-9]+\\s+STL"),
                  desc = ifelse(!is.na(cnt_stl), stringr::str_replace(desc, cnt_stl, "cnt STL"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Bad Pass Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(player_stl = stringr::str_extract(desc, "\\(Pcnt.Tcnt\\).+?\\s+STEAL"),
                  player_stl = stringr::str_remove_all(player_stl, "\\(Pcnt.Tcnt\\)\\s+|\\s+STEAL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(.+?)\\s+(STEAL.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(.+?)\\s+(STEAL.+?$)", "[player] \\2 \\3 [player] \\5"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[82]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Traveling Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[83]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Foul Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[84]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Double Dribble Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[85]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Discontinue Dribble Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[86]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "3 Second Violation Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[87]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Inbound Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[88]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Backcourt Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[89]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Offensive Goaltending Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[90]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Lane Violation Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[91]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Jump Ball Violation Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[92]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Kicked Ball Violation Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[93]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Illegal Assist Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[94]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Palming Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[95]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "T\\#[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "T#cnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Turnover: Inbound"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))", "[team] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[96]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Double Personal Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[97]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Punched Ball Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[98]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Swinging Elbows Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[99]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Basket from Below Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[100]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Illegal Screen Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[101]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Step Out of Bounds Turnover|Out Of Bounds Turnover|Player Out of Bounds Violation Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[102]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Out of Bounds Lost Ball Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[103]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Poss Lost Ball Turnover|Lost Ball Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[104]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Out of Bounds - Bad Pass Turnover Turnover|Bad Pass Turnover|Bad Pass Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[105]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(cnt_stl = stringr::str_extract(desc, "[0-9]+\\s+STL"),
                  desc = ifelse(!is.na(cnt_stl), stringr::str_replace(desc, cnt_stl, "cnt STL"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Lost Ball Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(player_stl = stringr::str_extract(desc, "\\(Pcnt.Tcnt\\).+?\\s+STEAL"),
                  player_stl = stringr::str_remove_all(player_stl, "\\(Pcnt.Tcnt\\)\\s+|\\s+STEAL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(.+?)\\s+(STEAL.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(.+?)\\s+(STEAL.+?$)", "[player] \\2 \\3 [player] \\5"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[106]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "T\\#[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "T#cnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Turnover: 8 Second Violation"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))", "[team] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[107]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "T\\#[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "T#cnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Turnover: Shot Clock"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))", "[team] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[108]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "T\\#[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "T#cnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Turnover: 5 Second Inbound|Turnover: 5 Second Violation"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))", "[team] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[109]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "T\\#[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "T#cnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Turnover: "),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))", "[team] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[110]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "T\\#[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "T#cnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Turnover: Too Many Players"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(T#cnt\\))", "[team] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[111]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "5 Second Violation Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[112]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_tnv = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_tnv), stringr::str_replace(desc, cnt_tnv, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_tnv = stringr::str_extract(desc, "Opposite Basket Turnover"),
                  desc = ifelse(is.na(tp_tnv), desc, stringr::str_replace(desc, tp_tnv, "[TURNOVER]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[TURNOVER\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[113]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Foul\\:T\\.FOUL|Foul\\:Non\\-Unsportsmanlike"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(coach = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])$", "[coach] \\2"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)", "[coach] \\2 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[114]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+|P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "T\\.FOUL|HANGING\\.TECH\\.FOUL|Non-Unsportsmanlike Tech Foul - Flopping|Non-Unsportsmanlike"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[115]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Foul|T\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])$", "[team] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[117]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "T.Foul"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "\\(.+?\\)"),
                  player = stringr::str_remove_all(player, '\\(Def\\. 3 Sec\\s+|\\s+\\)')) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+\\(Def\\.\\s+3\\s+Sec\\s+.+?\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+\\(Def\\.\\s+3\\s+Sec\\s+.+?\\)$", "[team] \\2 (Def. 3 Sec [player])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[118]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Foul|Delay"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])$", "[team] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[120]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Excess Timeout Technical"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])$", "[team] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[122]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Too Many Players Tech Foul"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(team = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])$", "[team] \\2"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[123]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "P\\.FOUL|Personal Block Foul"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[124]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "P\\.FOUL|Personal Block Foul"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[125]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "S\\.FOUL|Shooting Block Foul"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[126]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "S\\.FOUL|Shooting Block Foul"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[127]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "L\\.B\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[128]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "L\\.B\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[129]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN|P[0-9]+\\.T[0-9]+|P[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "OFF\\.Foul|Offensive Charge Foul"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[130]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "IN\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[131]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "IN\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[132]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "AWAY\\.FROM\\.PLAY\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[133]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "AWAY\\.FROM\\.PLAY\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[134]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "PUNCH\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 

  xdf[[135]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "C\\.P\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 

  xdf[[136]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "C\\.P\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[137]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul1 = stringr::str_extract(desc, "[0-9]+\\s+PF"),
                  desc = ifelse(!is.na(cnt_foul1), stringr::str_replace(desc, cnt_foul1, "cnt PF"), desc),
                  cnt_foul2 = stringr::str_extract(desc, "[0-9]+\\s+PF"),
                  desc = ifelse(!is.na(cnt_foul2), stringr::str_replace(desc, cnt_foul2, "cnt PF"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, " Foul : Double Personal"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player1 = stringr::str_extract(desc, "\\-\\s+(.+?)\\s+\\("),
                  player1 = stringr::str_remove_all(player1, "\\-\\s+|\\s+\\(")) %>% 
    dplyr::mutate(player2 = stringr::str_extract(desc, "\\;\\s+(.+?)\\s+\\("),
                  player2 = stringr::str_remove_all(player2, "\\;\\s+|\\s+\\(")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "\\[FOUL\\]\\s+\\-\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\;\\s+(.+?)\\s+\\(cnt\\s+PF\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "\\[FOUL\\]\\s+\\-\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\;\\s+(.+?)\\s+\\(cnt\\s+PF\\)$", "[FOUL] - [player] (cnt PF); [player] (cnt PF)"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "\\[FOUL\\]\\s+\\-\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\;\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\s+\\(.+?\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "\\[FOUL\\]\\s+\\-\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\;\\s+(.+?)\\s+\\(cnt\\s+PF\\)\\s+\\(.+?\\)$", "[FOUL] - [player] (cnt PF); [player] (cnt PF) ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[138]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE1"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[139]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE1"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[140]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE2"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[141]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "FLAGRANT\\.FOUL\\.TYPE2"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[142]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Personal Take Foul"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[143]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Personal Take Foul"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[144]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Transition Take Foul"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[145]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Transition Take Foul"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[146]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.T[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.Tcnt"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Taunting"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.Tcnt\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[147]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "P[0-9]+\\.PN"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Pcnt.PN"), desc)) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Taunting"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))$", "[player] \\2 \\3"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(Pcnt\\.PN\\))\\s+(\\(.+?\\)$)", "[player] \\2 \\3 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[148]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Foul\\:DOUBLE\\.TECHNICAL\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(coach = stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)", "[player] \\2 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[149]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Foul\\:DOUBLE\\.TECHNICAL\\.FOUL"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(coach = ifelse(!is.na(tp_foul), stringr::str_replace(desc, "(^.+?)(\\s+\\[)(.+?$)", "\\1"), NA_character_)) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\]$)"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\]$)", "[player] \\2"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(.+?)\\s+(\\[FOUL\\])\\s+(\\(.+?\\)$)", "[player] \\2 ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[150]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_foul = stringr::str_extract(desc, "Double Technical \\-"),
                  desc = ifelse(is.na(tp_foul), desc, stringr::str_replace(desc, tp_foul, "[FOUL]"))) %>% 
    dplyr::mutate(player1 = stringr::str_extract(desc, "\\]\\s+(.+?)\\;"),
                  player1 = stringr::str_remove_all(player1, "\\]\\s+|\\;")) %>% 
    dplyr::mutate(player2 = stringr::str_extract(desc, "\\;\\s+(.+?$)"),
                  player2 = stringr::str_remove_all(player2, "\\;\\s+")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "\\[FOUL\\]\\s+(.+?)\\;\\s+(.+?)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "\\[FOUL\\]\\s+(.+?)\\;\\s+(.+?)$", "[FOUL] [player]; [player]"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[151]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:No Violation"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[152]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\: Delay of game Violation"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(team = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  team = stringr::str_remove_all(team, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[team] [VIOLATION]"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[153]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:Defensive Goaltending"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$", "[player] [VIOLATION] ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[154]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:Lane"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$", "[player] [VIOLATION] ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[155]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:Jump Ball"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$", "[player] [VIOLATION] ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[156]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:Kicked Ball"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$", "[player] [VIOLATION] ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[157]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_vio = stringr::str_extract(desc, "Violation\\:Double Lane"),
                  desc = ifelse(is.na(tp_vio), desc, stringr::str_replace(desc, tp_vio, "[VIOLATION]"))) %>% 
    dplyr::mutate(player = stringr::str_extract(desc, "(^.+?)\\s+\\["),
                  player = stringr::str_remove_all(player, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]$", "[player] [VIOLATION]"), desc)) %>% 
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 2, stringr::str_replace(desc, "(^.+?)\\s+\\[VIOLATION\\]\\s+\\(.+?\\)$", "[player] [VIOLATION] ([referee])"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[158]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(player1 = stringr::str_extract(desc, "SUB\\:\\s+(.+?)\\s+FOR"),
                  player1 = stringr::str_remove_all(player1, "SUB\\:\\s+|\\s+FOR")) %>% 
    dplyr::mutate(player2 = stringr::str_extract(desc, "FOR\\s+(.+?)$"),
                  player2 = stringr::str_remove_all(player2, "FOR\\s+")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "SUB\\:\\s+(.+?)\\s+FOR\\s+(.+?)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "SUB\\:\\s+(.+?)\\s+FOR\\s+(.+?)$", "SUB: [player] FOR [player]"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[159]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "Full\\s+[0-9]+\\s+Short\\s+[0-9]+|Reg\\.[0-9]+\\s+Short\\s+[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Full cnt Short cnt"), desc)) %>% 
    dplyr::mutate(tp_tout = stringr::str_extract(desc, "Timeout\\:\\s+Regular|Timeout\\:\\s+Short|Timeout\\:Short|Timeout\\:Regular"),
                  desc = ifelse(is.na(tp_tout), desc, stringr::str_replace(desc, tp_tout, "[TIMEOUT]"))) %>% 
    dplyr::mutate(team = stringr::str_extract(desc, "(^.+?)\\["),
                  team = stringr::str_remove_all(team, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$", "[team] [TIMEOUT] (Full cnt Short cnt)"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[160]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "Full\\s+[0-9]+\\s+Short\\s+[0-9]+|Reg\\.[0-9]+\\s+Short\\s+[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Full cnt Short cnt"), desc)) %>% 
    dplyr::mutate(tp_tout = stringr::str_extract(desc, "Timeout\\: Coach Challenge"),
                  desc = ifelse(is.na(tp_tout), desc, stringr::str_replace(desc, tp_tout, "[TIMEOUT]"))) %>% 
    dplyr::mutate(team = stringr::str_extract(desc, "(^.+?)\\["),
                  team = stringr::str_remove_all(team, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$", "[team] [TIMEOUT] (Full cnt Short cnt)"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[161]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(tp_tout = stringr::str_extract(desc, "Timeout\\: Official"),
                  desc = ifelse(is.na(tp_tout), desc, stringr::str_replace(desc, tp_tout, "[TIMEOUT]"))) %>% 
    dplyr::mutate(sintaxe = ifelse(desc == '[TIMEOUT]', 1, 0)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  
  xdf[[162]] %>% 
    dplyr::select(game_id, description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(cnt_foul = stringr::str_extract(desc, "Full\\s+[0-9]+\\s+Short\\s+[0-9]+|Reg\\.[0-9]+\\s+Short\\s+[0-9]+"),
                  desc = ifelse(!is.na(cnt_foul), stringr::str_replace(desc, cnt_foul, "Full cnt Short cnt"), desc)) %>% 
    dplyr::mutate(tp_tout = stringr::str_extract(desc, "Timeout: No Timeout|Timeout: Timeout"),
                  desc = ifelse(is.na(tp_tout), desc, stringr::str_replace(desc, tp_tout, "[TIMEOUT]"))) %>% 
    dplyr::mutate(team = stringr::str_extract(desc, "(^.+?)\\["),
                  team = stringr::str_remove_all(team, "\\s+\\[")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(sintaxe == 0 & stringr::str_detect(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(desc = ifelse(sintaxe == 1, stringr::str_replace(desc, "(^.+?)\\s+\\[TIMEOUT\\]\\s+\\(Full cnt Short cnt\\)$", "[team] [TIMEOUT] (Full cnt Short cnt)"), desc)) %>% 
    dplyr::group_by(sintaxe, desc) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100) 
  

  ##################
  
  xdf[[109]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Foul:Non-Unsportsmanlike", "TECH_FOUL"),
                  description = stringr::str_replace(description, "Foul:T.FOUL", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)$", "[coach] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+(.+?)$", "[coach] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[110]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "HANGING.TECH.FOUL", "TECH_FOUL"),
                  description = stringr::str_replace(description, "Non-Unsportsmanlike", "TECH_FOUL"),
                  description = stringr::str_replace(description, "T.FOUL", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+.+$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+.+", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 3, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 3, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.PN\\)", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+.+$"), 4, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 4, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+.+", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[111]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Foul", "TECH_FOUL"),
                  description = stringr::str_replace(description, "T.FOUL", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)$", "[team] \\2"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[113]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "T.FOUL", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.PN\\)", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+.+$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+.+", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[114]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Foul:DOUBLE.TECHNICAL.FOUL", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+(.+?)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+(.+?)$", "[coach] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[115]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Foul:DOUBLE.TECHNICAL.FOUL", "TECH_FOUL"),
                  description = stringr::str_replace(description, "^Double Technical -", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)$", "[coach] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL\\s+(.+?)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+(.+?)$", "[coach] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(TECH_FOUL)\\s+(.+?)\\;\\s+(.+?)\\s+(\\(.+?\\))$"), 3, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 3, stringr::str_replace(description, "(TECH_FOUL)\\s+(.+?)\\;\\s+(.+?)\\s+(\\(.+?\\)$)", "\\1 [coach], [player] ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[116]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "^Double Technical -", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(TECH_FOUL)\\s+(.+?)\\;\\s+(.+?)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(TECH_FOUL)\\s+(.+?)\\;\\s+(.+?)$", "\\1 [player], [player]"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(TECH_FOUL)\\s+(.+?)\\;\\s+(.+?)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(TECH_FOUL)\\s+(.+?)\\,\\s+(.+?)\\s+(\\(.+?\\))$", "\\1 [player], [player] ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[117]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "T.Foul", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(Def\\. 3 Sec .+? \\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(Def\\. 3 Sec .+? \\)", "[team] \\2 (Def. 3 Sec [player] )"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(Def\\. 3 Sec .+? \\) \\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)\\s+\\(Def\\. 3 Sec .+? \\) \\(.+?\\)", "[team] \\2 (Def. 3 Sec [player] ) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[118]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Delay", "TECH_FOUL"),
                  description = stringr::str_replace(description, "Foul$", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)", "[team] \\2"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[120]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Excess Timeout Technical", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)", "[team] \\2"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[122]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Too Many Players Tech Foul", "TECH_FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TECH_FOUL"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(TECH_FOUL)", "[team] \\2"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[123]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "P.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[124]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "P.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[125]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "S.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[126]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "S.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[127]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "L.B.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[128]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "L.B.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[129]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "OFF.Foul", "FOUL"),
                  description = stringr::str_replace(description, "Offensive Charge Foul", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\)$", "[player] \\2 (Pcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 3, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 3, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\)\\s+\\(.+?\\)$"), 4, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 4, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt) ([referee])"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 5, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 5, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 6, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 6, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[130]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "AWAY.FROM.PLAY.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[131]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "AWAY.FROM.PLAY.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[132]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "PUNCH.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[133]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "C.P.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[134]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "C.P.FOUL", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[135]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, " Foul : Double Personal -", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(FOUL)\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\;\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\s+\\(.+?\\)"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(FOUL)\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\;\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\s+\\(.+?\\)", "\\1 [player] (cnt PF); [player] (cnt PF) ([referee])"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(FOUL)\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\;\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(FOUL)\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)\\;\\s+(.+?)\\s+\\([0-9]+\\s+PF\\)$", "\\1 [player] (cnt PF); [player] (cnt PF)"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[136]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "FLAGRANT.FOUL.TYPE1", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[137]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "FLAGRANT.FOUL.TYPE1", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[138]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "FLAGRANT.FOUL.TYPE2", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[139]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "FLAGRANT.FOUL.TYPE2", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[140]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Personal Take Foul", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)$", "[player] \\2 (Pcnt.Tcnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[141]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Personal Take Foul", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)$", "[player] \\2 (Pcnt.PN)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[142]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Transition Take Foul", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.T[0-9]+\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.Tcnt) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[143]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Transition Take Foul", "FOUL")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+FOUL\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(FOUL)\\s+\\(P[0-9]+\\.PN\\)\\s+\\(.+?\\)$", "[player] \\2 (Pcnt.PN) ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[144]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Violation: Delay of game Violation", "VIOLATION")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)", "[team] \\2"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[146]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Violation:Defensive Goaltending", "VIOLATION")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)$", "[player] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)\\s+(.+?)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)(.+?)$", "[player] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[147]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Violation:Lane", "VIOLATION")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)$", "[player] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)\\s+(.+?)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)(.+?)$", "[player] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[148]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Violation:Jump Ball", "VIOLATION")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)$", "[player] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)\\s+(.+?)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)(.+?)$", "[player] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[149]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Violation:Kicked Ball", "VIOLATION")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)$"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)$", "[player] \\2"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+(VIOLATION)\\s+(.+?)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+(VIOLATION)(.+?)$", "[player] \\2 ([referee])"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[151]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "SUB\\:\\s+(.+?)\\s+FOR\\s+(.+?$)"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "SUB\\:\\s+(.+?)\\s+FOR\\s+(.+?$)", "SUB: [player] FOR [player]"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[152]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Timeout\\:\\s+Regular", "TIMEOUT")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TIMEOUT\\s+\\(Full\\s+[0-9]+\\s+Short\\s+[0-9]+\\)"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+TIMEOUT\\s+\\(Full\\s+[0-9]+\\s+Short\\s+[0-9]+\\)", "[team] TIMEOUT (Full cnt Short cnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TIMEOUT\\s+\\(Reg\\.[0-9]+\\s+Short\\s+[0-9]+\\)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+TIMEOUT\\s+\\(Reg\\.[0-9]+\\s+Short\\s+[0-9]+\\)", "[team] TIMEOUT (Reg.cnt Short cnt)"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  xdf[[153]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(desc = description) %>% 
    dplyr::mutate(description = stringr::str_replace(description, "Timeout\\:\\s+Coach\\s+Challenge", "TIMEOUT")) %>% 
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TIMEOUT\\s+\\(Full\\s+[0-9]+\\s+Short\\s+[0-9]+\\)"), 1, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 1, stringr::str_replace(description, "(.+?)\\s+TIMEOUT\\s+\\(Full\\s+[0-9]+\\s+Short\\s+[0-9]+\\)", "[team] TIMEOUT (Full cnt Short cnt)"), description)) %>% 
    dplyr::mutate(sintaxe = ifelse(stringr::str_detect(description, "(.+?)\\s+TIMEOUT\\s+\\(Reg\\.[0-9]+\\s+Short\\s+[0-9]+\\)"), 2, sintaxe)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 2, stringr::str_replace(description, "(.+?)\\s+TIMEOUT\\s+\\(Reg\\.[0-9]+\\s+Short\\s+[0-9]+\\)", "[team] TIMEOUT (Reg.cnt Short cnt)"), description)) %>% 
    dplyr::mutate(description = ifelse(sintaxe == 0, desc, description)) %>% 
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  
  # 
  xdf[[153]] %>% 
    dplyr::select(description) %>%
    dplyr::mutate(sintaxe = 0) %>%
    dplyr::group_by(sintaxe, description) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
    dplyr::arrange(desc(n)) %>% 
    print(., n = 100)
  }
