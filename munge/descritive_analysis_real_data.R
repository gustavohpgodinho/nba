
require(ggridges)
require(tidyverse)

load("D:/Mestrado/NBA/nba/data/processed/pbp_moments.RData")

pbp_moments %>% 
  dplyr::group_by(season, game_id, period) %>% 
  dplyr::summarise(hpts = sum(hpts), 
                   vpts = sum(vpts), 
                   .groups = 'drop') %>% 
  dplyr::filter(period <= 4) %>% 
  ggplot2::ggplot(ggplot2::aes(season, hpts)) +
  ggplot2::geom_boxplot(fill = 'tomato') +
  ggplot2::facet_grid(~period) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 60, 5)) +
  ggplot2::ylab("Home Points") +
  ggplot2::xlab("Season") +
  ggplot2::ggtitle("Home Points by Season and Period")

pbp_moments %>% 
  dplyr::group_by(season, game_id, period) %>% 
  dplyr::summarise(hpts = sum(hpts), 
                   vpts = sum(vpts), 
                   .groups = 'drop') %>% 
  dplyr::filter(period <= 4) %>% 
  ggplot2::ggplot(ggplot2::aes(season, vpts)) +
  ggplot2::geom_boxplot(fill = 'royalblue1') +
  ggplot2::facet_grid(~period) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 60, 5)) +
  ggplot2::ylab("Away Points") +
  ggplot2::xlab("Season") +
  ggplot2::ggtitle("Away Points by Season and Period")


tbl_aux_time <- pbp_moments %>% 
  dplyr::distinct(period, clock) %>% 
  dplyr::arrange(desc(clock)) %>% 
  tidyr::separate(clock, into = c('min', 'sec', 'dec'), sep = ":", remove = FALSE) %>% 
  dplyr::mutate_at(.vars = c('min', 'sec', 'dec'), .funs = as.numeric) %>% 
  dplyr::mutate(decsecs = (600 * min) + (10 * sec) + (dec/10)) %>% 
  dplyr::mutate(time = ifelse(period <= 4, 7200 - decsecs, 3000 - decsecs)) %>% 
  dplyr::select(period, clock, time) %>% 
  dplyr::mutate(time = (period - 1) * 7200 + time) %>% 
  dplyr::arrange(time)

cumulative_points_by_time <- function(t, df) {
  
  df %>% 
    dplyr::group_by(game_id) %>% 
    dplyr::filter(time <= t) %>% 
    dplyr::slice_tail(n = 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(time = t)
  
}

calculate_metrics <- function(df){
  
  df %>%
    dplyr::group_by(season, time) %>% 
    dplyr::summarise(
      min_hpts = min(hpts),
      p10_hpts = quantile(hpts, 0.10),
      p25_hpts = quantile(hpts, 0.25),
      med_hpts = median(hpts),
      avg_hpts = mean(hpts),
      p75_hpts = quantile(hpts, 0.75),
      p90_hpts = quantile(hpts, 0.90),
      max_hpts = max(hpts),
      sd_hpts = sd(hpts),
      min_vpts = min(vpts),
      p10_vpts = quantile(vpts, 0.10),
      p25_vpts = quantile(vpts, 0.25),
      med_vpts = median(vpts),
      avg_vpts = mean(vpts),
      p75_vpts = quantile(vpts, 0.75),
      p90_vpts = quantile(vpts, 0.90),
      max_vpts = max(vpts),
      sd_vpts = sd(vpts),
      min_dif = min(dif),
      p10_dif = quantile(dif, 0.10),
      p25_dif = quantile(dif, 0.25),
      med_dif = median(dif),
      avg_dif = mean(dif),
      p75_dif = quantile(dif, 0.75),
      p90_dif = quantile(dif, 0.90),
      sd_dif = sd(dif),
      max_dif = max(dif), .groups = 'drop')
  
}


df0 <- pbp_moments %>% 
  dplyr::filter(period <= 4) %>% 
  dplyr::left_join(tbl_aux_time, by = c('period', 'clock')) %>%
  dplyr::group_by(season, game_id) %>%
  dplyr::mutate(hpts = cumsum(hpts),
                vpts = cumsum(vpts)) %>% 
  dplyr::mutate(dif = hpts - vpts) %>%
  dplyr::ungroup() %>% 
  plyr::llply(.data = seq(0, 28800, 300), .fun = cumulative_points_by_time, df = .,  .progress = 'time') %>%   
  dplyr::bind_rows()
  


df0 %>% 
  calculate_metrics() %>% 
  dplyr::mutate(period = 4,
                period = ifelse(time < 21600, 3, period),
                period = ifelse(time < 14400, 2, period),
                period = ifelse(time < 7200, 1, period)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = time, y = avg_hpts, colour = season)) +
  ggplot2::geom_line(linewidth = .1) + 
  ggplot2::geom_point() + 
  ggplot2::theme_minimal() +
  ggplot2::geom_vline(xintercept = c(0,7200, 14400, 21600,28800), linetype = 'dashed') +
  ggplot2::geom_hline(yintercept = 100, linetype = 'dashed') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::scale_fill_discrete(name = "Season") +
  ggplot2::scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 28800, 1800), 
                              labels = c('1> 12:00', '1> 09:00', '1> 06:00', '1> 03:00', 
                                         '2> 12:00', '2> 09:00', '2> 06:00', '2> 03:00',
                                         '3> 12:00', '3> 09:00', '3> 06:00', '3> 03:00',
                                         '4> 12:00', '4> 09:00', '4> 06:00', '4> 03:00', 
                                         '4> 00:00')) +
  ggplot2::labs(title = "Cumulative Mean Home Points over Time by Season", x = "Time", 
                y = "Cumulative Mean Home Points", color = "Season")
  

df0 %>% 
  calculate_metrics() %>% 
  dplyr::mutate(period = 4,
                period = ifelse(time < 21600, 3, period),
                period = ifelse(time < 14400, 2, period),
                period = ifelse(time < 7200, 1, period)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = time, y = avg_hpts)) +
  ggplot2::geom_line(linewidth = 1, colour = 'tomato') + 
  ggplot2::theme_minimal() +
  ggplot2::geom_vline(xintercept = c(0, 7200, 14400, 21600,28800), linetype = 'dashed') +
  ggplot2::geom_hline(yintercept = c(0, 25, 50, 75, 100), linetype = 'dashed') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::scale_fill_discrete(name = "Season") +
  ggplot2::scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 28800, 1800), 
                              labels = c('1> 12:00', '1> 09:00', '1> 06:00', '1> 03:00', 
                                         '2> 12:00', '2> 09:00', '2> 06:00', '2> 03:00',
                                         '3> 12:00', '3> 09:00', '3> 06:00', '3> 03:00',
                                         '4> 12:00', '4> 09:00', '4> 06:00', '4> 03:00', 
                                         '4> 00:00')) +
  ggplot2::labs(title = "Cumulative Mean Home Points over Time by Season", x = "Time", 
                y = "Cumulative Mean Home Points", color = "Season") +
  ggplot2::facet_wrap(~season, nrow = 4)
  
df0 %>% 
  calculate_metrics() %>% 
  dplyr::mutate(period = 4,
                period = ifelse(time < 21600, 3, period),
                period = ifelse(time < 14400, 2, period),
                period = ifelse(time < 7200, 1, period)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = time)) +
  ggplot2::geom_line(ggplot2::aes(y = avg_hpts), linewidth = 1, colour = 'royalblue1') + 
  ggplot2::geom_line(ggplot2::aes(y = avg_vpts), linewidth = 1, colour = 'tomato') + 
  ggplot2:::geom_ribbon(ggplot2::aes(ymin = p10_hpts, ymax = p90_hpts), fill = 'royalblue1', alpha = 0.3) +
  ggplot2:::geom_ribbon(ggplot2::aes(ymin = p10_vpts, ymax = p90_vpts), fill = 'tomato', alpha = 0.3) +
  ggplot2::theme_minimal() +
  ggplot2::geom_vline(xintercept = c(0, 7200, 14400, 21600,28800), linetype = 'dashed') +
  ggplot2::geom_hline(yintercept = c(0, 25, 50, 75, 100), linetype = 'dashed') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::scale_fill_discrete(name = "Season") +
  ggplot2::scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = 10)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 28800, 1800), 
                              labels = c('1> 12:00', '1> 09:00', '1> 06:00', '1> 03:00', 
                                         '2> 12:00', '2> 09:00', '2> 06:00', '2> 03:00',
                                         '3> 12:00', '3> 09:00', '3> 06:00', '3> 03:00',
                                         '4> 12:00', '4> 09:00', '4> 06:00', '4> 03:00', 
                                         '4> 00:00')) +
  ggplot2::labs(title = "Cumulative Mean Points over Time by Season", x = "Time", 
                subtitle = "Blue: Home Team, Red: Away Team",
                y = "Cumulative Mean Points", color = "Season") +
  ggplot2::facet_wrap(~season, nrow = 4)

df0 %>% 
  calculate_metrics() %>% 
  dplyr::mutate(period = 4,
                period = ifelse(time < 21600, 3, period),
                period = ifelse(time < 14400, 2, period),
                period = ifelse(time < 7200, 1, period)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = time, colour = season)) +
  ggplot2::geom_hline(yintercept = seq(-20, 20, 10), linewidth = 0.5, colour = 'gray60') +
  ggplot2::geom_line(ggplot2::aes(y = min_dif), linewidth = 1) + 
  ggplot2::geom_line(ggplot2::aes(y = max_dif), linewidth = 1) + 
  ggplot2::geom_line(ggplot2::aes(y = p10_dif), linewidth = 1) + 
  ggplot2::geom_line(ggplot2::aes(y = p90_dif), linewidth = 1) + 
  ggplot2::geom_line(ggplot2::aes(y = avg_dif), linewidth = 1) + 
  ggplot2::theme_minimal() +
  ggplot2::geom_vline(xintercept = c(0, 7200, 14400, 21600,28800), linetype = 'dashed') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::scale_fill_discrete(name = "Season") +
  ggplot2::scale_y_continuous(breaks = seq(-20, 20, 10)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 28800, 1800), 
                              labels = c('1> 12:00', '1> 09:00', '1> 06:00', '1> 03:00', 
                                         '2> 12:00', '2> 09:00', '2> 06:00', '2> 03:00',
                                         '3> 12:00', '3> 09:00', '3> 06:00', '3> 03:00',
                                         '4> 12:00', '4> 09:00', '4> 06:00', '4> 03:00', 
                                         '4> 00:00')) +
  ggplot2::labs(title = "Cumulative Mean Difference Points over Time by Season", x = "Time", 
                subtitle = "lines order: max, percentile 90, mean, percentile 10, min",
                y = "Cumulative Mean Difference Points", color = "Season") +
  ggplot2::facet_wrap(~season, nrow = 4)

df0 %>% 
  calculate_metrics() %>% 
  dplyr::mutate(period = 4,
                period = ifelse(time < 21600, 3, period),
                period = ifelse(time < 14400, 2, period),
                period = ifelse(time < 7200, 1, period)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = time)) +
  ggplot2::geom_line(ggplot2::aes(y = sd_hpts), colour = 'tomato', linewidth = 1) + 
  ggplot2::geom_line(ggplot2::aes(y = sd_vpts), colour = 'royalblue1', linewidth = 1) + 
  ggplot2::geom_line(ggplot2::aes(y = sd_dif), colour = 'gold1', linewidth = 1) + 
  ggplot2::theme_minimal() +
  ggplot2::geom_vline(xintercept = c(0, 7200, 14400, 21600,28800), linetype = 'dashed') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::scale_fill_discrete(name = "Season") +
  ggplot2::scale_x_continuous(breaks = seq(0, 28800, 1800), 
                              labels = c('1> 12:00', '1> 09:00', '1> 06:00', '1> 03:00', 
                                         '2> 12:00', '2> 09:00', '2> 06:00', '2> 03:00',
                                         '3> 12:00', '3> 09:00', '3> 06:00', '3> 03:00',
                                         '4> 12:00', '4> 09:00', '4> 06:00', '4> 03:00', 
                                         '4> 00:00')) +
  ggplot2::labs(title = "Cumulative Mean Difference Points over Time by Season", x = "Time", 
                y = "Cumulative Mean Difference Points", color = "Season") +
  ggplot2::facet_wrap(~season, nrow = 4)
  

df0 %>% 
  dplyr::filter(time %in% seq(0, 28800, 3600)) %>%
  dplyr::mutate(time = as.factor(time)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = dif, y = time, fill = ..x..)) +
  ggridges::geom_density_ridges_gradient(scale = 10, rel_min_height = 0.005) +
  ggplot2::scale_fill_viridis_c(name = "Home points", alpha = 0.5)
  

df0 %>% 
  dplyr::group_by(season, time) %>%
  dplyr::summarise(n = dplyr::n_distinct(game_id),
                   n0 = sum(dif == 0), .groups = 'drop') %>%
  dplyr::mutate(prop = n0/n) %>% 
  dplyr::mutate(period = 4,
                period = ifelse(time < 21600, 3, period),
                period = ifelse(time < 14400, 2, period),
                period = ifelse(time < 7200, 1, period)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = time)) +
  ggplot2::geom_line(ggplot2::aes(y = prop), colour = 'tomato', linewidth = 1) + 
  ggplot2::geom_point(ggplot2::aes(y = prop), colour = 'tomato') + 
  ggplot2::theme_minimal() +
  ggplot2::geom_vline(xintercept = c(0, 7200, 14400, 21600,28800), linetype = 'dashed') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::scale_fill_discrete(name = "Season") +
  ggplot2::scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = .01)) +
  ggplot2::ylim(0, 0.1) +
  ggplot2::scale_x_continuous(breaks = seq(0, 28800, 1800), 
                              labels = c('1> 12:00', '1> 09:00', '1> 06:00', '1> 03:00', 
                                         '2> 12:00', '2> 09:00', '2> 06:00', '2> 03:00',
                                         '3> 12:00', '3> 09:00', '3> 06:00', '3> 03:00',
                                         '4> 12:00', '4> 09:00', '4> 06:00', '4> 03:00', 
                                         '4> 00:00')) +
  ggplot2::labs(title =  "Draw Proportion Across Game Time in NBA Matches by Season", x = "Time", 
                y = "Proportion", color = "Season") +
  ggplot2::facet_wrap(~season, nrow = 4)

df0 %>% 
  dplyr::group_by(season, time) %>%
  dplyr::summarise(n = dplyr::n_distinct(game_id),
                   n0 = sum(abs(dif) <= 3), .groups = 'drop') %>% 
  dplyr::mutate(prop = n0/n) %>% 
  dplyr::filter(time > 3600) %>% 
  dplyr::mutate(period = 4,
                period = ifelse(time < 21600, 3, period),
                period = ifelse(time < 14400, 2, period),
                period = ifelse(time < 7200, 1, period)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = time)) +
  ggplot2::geom_line(ggplot2::aes(y = prop), colour = 'tomato', linewidth = 1) + 
  ggplot2::theme_minimal() +
  ggplot2::geom_hline(yintercept = 0.2, colour = 'gray60') +
  ggplot2::geom_vline(xintercept = c(0, 7200, 14400, 21600,28800), linetype = 'dashed') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::scale_fill_discrete(name = "Season") +
  ggplot2::scale_x_continuous(breaks = seq(0, 28800, 1800), 
                              labels = c('1> 12:00', '1> 09:00', '1> 06:00', '1> 03:00', 
                                         '2> 12:00', '2> 09:00', '2> 06:00', '2> 03:00',
                                         '3> 12:00', '3> 09:00', '3> 06:00', '3> 03:00',
                                         '4> 12:00', '4> 09:00', '4> 06:00', '4> 03:00', 
                                         '4> 00:00')) +
  ggplot2::labs(title = "Proportion of matches with Points difference equal or smaller 3 over Time by Season", x = "Time", 
                y = "Proportion of matches", color = "Season") +
  ggplot2::facet_wrap(~season, nrow = 4)


load('D:/Mestrado/NBA/data/simulacoes/df_simulacoes_tipoa.RData')
load('D:/Mestrado/NBA/data/simulacoes/df_simulacoes_tipob.RData')


dfa <- df_simulacoes_tipoa %>% 
  dplyr::filter(period <= 4) %>% 
  dplyr::mutate(hpts = 0,
                hpts = (1 * stringr::str_count(cod1, "_p4.{2}a3\\(.{2}c")) + hpts,
                hpts = (2 * stringr::str_count(cod1, "_p4.{2}a1\\((1|2|3|4)")) + hpts,
                hpts = (3 * stringr::str_count(cod1, "_p4.{2}a1\\(5")) + hpts) %>% 
  dplyr::mutate(vpts = 0,
                vpts = (1 * stringr::str_count(cod1, "_p5.{2}a3\\(.{2}c")) + vpts,
                vpts = (2 * stringr::str_count(cod1, "_p5.{2}a1\\((1|2|3|4)")) + vpts,
                vpts = (3 * stringr::str_count(cod1, "_p5.{2}a1\\(5")) + vpts) %>% 
  dplyr::group_by(simulation_code) %>% 
  dplyr::mutate(hpts = cumsum(hpts),
                vpts = cumsum(vpts)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dif = hpts - vpts) %>% 
  dplyr::mutate(time = ifelse(period == 2, 7200 + time, time),
                time = ifelse(period == 3, 2*(7200) + time, time),
                time = ifelse(period == 4, 3*(7200) + time, time)) %>% 
  dplyr::rename(game_id = simulation_code) %>% 
  plyr::llply(.data = seq(0, 28800, 300), .fun = cumulative_points_by_time, df = .,  .progress = 'time') %>%   
  dplyr::bind_rows()

dfb <- df_simulacoes_tipob %>% 
  dplyr::filter(period <= 4) %>% 
  dplyr::mutate(hpts = 0,
                hpts = (1 * stringr::str_count(cod1, "_p4.{2}a3\\(.{2}c")) + hpts,
                hpts = (2 * stringr::str_count(cod1, "_p4.{2}a1\\((1|2|3|4)")) + hpts,
                hpts = (3 * stringr::str_count(cod1, "_p4.{2}a1\\(5")) + hpts) %>% 
  dplyr::mutate(vpts = 0,
                vpts = (1 * stringr::str_count(cod1, "_p5.{2}a3\\(.{2}c")) + vpts,
                vpts = (2 * stringr::str_count(cod1, "_p5.{2}a1\\((1|2|3|4)")) + vpts,
                vpts = (3 * stringr::str_count(cod1, "_p5.{2}a1\\(5")) + vpts) %>% 
  dplyr::group_by(simulation_code) %>% 
  dplyr::mutate(hpts = cumsum(hpts),
                vpts = cumsum(vpts)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dif = hpts - vpts) %>% 
  dplyr::mutate(time = ifelse(period == 2, 7200 + time, time),
                time = ifelse(period == 3, 2*(7200) + time, time),
                time = ifelse(period == 4, 3*(7200) + time, time)) %>% 
  dplyr::rename(game_id = simulation_code) %>% 
  plyr::llply(.data = seq(0, 28800, 300), .fun = cumulative_points_by_time, df = .,  .progress = 'time') %>%   
  dplyr::bind_rows()

df_simA <- dfa %>% 
  dplyr::filter(game_id > 5000) %>% 
  dplyr::group_by(time) %>%
  dplyr::summarise(n = dplyr::n_distinct(game_id),
                   n0 = sum(abs(dif) <= 3), .groups = 'drop') %>% 
  dplyr::mutate(prop = n0/n) %>% 
  dplyr::mutate(period = 4,
                period = ifelse(time < 21600, 3, period),
                period = ifelse(time < 14400, 2, period),
                period = ifelse(time < 7200, 1, period)) 

df_simB <- dfb %>% 
  dplyr::filter(game_id > 5000) %>% 
  dplyr::group_by(time) %>%
  dplyr::summarise(n = dplyr::n_distinct(game_id),
                   n0 = sum(abs(dif) <= 3), .groups = 'drop') %>% 
  dplyr::mutate(prop = n0/n) %>% 
  dplyr::mutate(period = 4,
                period = ifelse(time < 21600, 3, period),
                period = ifelse(time < 14400, 2, period),
                period = ifelse(time < 7200, 1, period)) 

df_real <- df0 %>% 
  dplyr::filter(season %in% c('2018-19', '2019-20', '2020-21', '2021-22')) %>% 
  dplyr::group_by(time) %>%
  dplyr::summarise(n = dplyr::n_distinct(game_id),
                   n0 = sum(abs(dif) <= 3), .groups = 'drop') %>% 
  dplyr::mutate(prop = n0/n) %>% 
  dplyr::mutate(period = 4,
                period = ifelse(time < 21600, 3, period),
                period = ifelse(time < 14400, 2, period),
                period = ifelse(time < 7200, 1, period))

df_real <- df0 %>% 
  dplyr::filter(season %in% c('2022-23')) %>% 
  dplyr::group_by(time) %>%
  dplyr::summarise(n = dplyr::n_distinct(game_id),
                   n0 = sum(abs(dif) <= 3), .groups = 'drop') %>% 
  dplyr::mutate(prop = n0/n) %>% 
  dplyr::mutate(period = 4,
                period = ifelse(time < 21600, 3, period),
                period = ifelse(time < 14400, 2, period),
                period = ifelse(time < 7200, 1, period))


ggplot2::ggplot() +
  ggplot2::geom_line(data = df_simA, ggplot2::aes(x = time, y = prop), colour = 'tomato', linewidth = 1) + 
  ggplot2::geom_line(data = df_simB, ggplot2::aes(x = time, y = prop), colour = 'limegreen', linewidth = 1) + 
  ggplot2::geom_line(data = df_real, ggplot2::aes(x = time, y = prop), colour = 'royalblue1', linewidth = 1) + 
  ggplot2::geom_point(data = df_simA, ggplot2::aes(x = time, y = prop), colour = 'tomato') + 
  ggplot2::geom_point(data = df_simB, ggplot2::aes(x = time, y = prop), colour = 'limegreen') + 
  ggplot2::geom_point(data = df_real, ggplot2::aes(x = time, y = prop), colour = 'royalblue1') + 
  ggplot2::theme_minimal() +
  ggplot2::geom_vline(xintercept = c(0, 7200, 14400, 21600,28800), linetype = 'dashed') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::scale_fill_discrete(name = "Season") +
  ggplot2::scale_y_continuous(breaks = function(z) seq(0, range(z)[2], by = .01)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 28800, 1800), 
                              labels = c('1> 12:00', '1> 09:00', '1> 06:00', '1> 03:00', 
                                         '2> 12:00', '2> 09:00', '2> 06:00', '2> 03:00',
                                         '3> 12:00', '3> 09:00', '3> 06:00', '3> 03:00',
                                         '4> 12:00', '4> 09:00', '4> 06:00', '4> 03:00', 
                                         '4> 00:00')) +
  ggplot2::ylim(0, 0.5) +
  ggplot2::labs(title =  "Comparison of Matches proportion of Points difference equal or smaller than 3 over time",
                subtitle = "Blue: Real data of season 2022-23\nRed: Simulations without considering played time\nGreen: Simulations considering played time",
                x = "Time", 
                y = "Proportion")
