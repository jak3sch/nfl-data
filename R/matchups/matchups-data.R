#dvoa_data <- readr::read_csv(glue::glue("https://github.com/jak3sch/nfl-insights/releases/download/dvoa-data/nfl_dvoa_{var.season}.csv"), col_types = "cdddcd")

# all matchups ----

matchups <- nflreadr::load_schedules(var.season) %>%
  dplyr::select(week, gameday, weekday, gametime, ends_with("_team"), spread_line, total_line, roof, surface, temp, wind, stadium) %>%
  dplyr::filter(week == var.currentWeek) %>%
  mutate(
    matchup = paste(away_team, "@", home_team),
    date_time = lubridate::force_tz(lubridate::as_datetime(lubridate::ymd_hm(paste(gameday, gametime))), "EST"),
    date_time_cet = lubridate::with_tz(date_time, "CET"),
    home_opponent = away_team,
    away_opponent = home_team
  ) %>%
  separate(date_time_cet, c("date_cet", "time_cet"), sep = " ") %>%

  tidyr::gather(key, team, dplyr::ends_with("_opponent")) %>%
  dplyr::mutate(
    opponent = ifelse(key == "away_opponent", away_team, home_team),
    spread_line = ifelse(key == "away_opponent", spread_line, spread_line * -1),
    score_pred = (total_line / 2) + (spread_line / 2),
    spread_line_size = spread_line / 10,
    spread_line_size = (0.08 - 0.03) * (spread_line_size - min(spread_line_size)) / (max(spread_line_size) - min(spread_line_size)) + 0.03
  ) %>%

  ## add external data ----
  #dplyr::left_join(dvoa_teams %>% dplyr::select(team, dplyr::starts_with("offense")), by = c("team" = "team")) %>%
  #dplyr::left_join(dvoa_teams %>% dplyr::select(team, dplyr::starts_with("defense")), by = c("opponent" = "team")) %>%
  #dplyr::left_join(epa_teams %>% dplyr::select(team, dplyr::starts_with("adjusted_off")), by = c("team" = "team")) %>%
  #dplyr::left_join(epa_teams %>% dplyr::select(team, dplyr::starts_with("adjusted_def")), by = c("opponent" = "team")) %>%
  #dplyr::left_join(pace_teams, by = "team") %>%

  # ranks
  f_create_ranks(arrange_by = desc(total_line), rank_col_name = "total_line_rank") %>%
  f_create_ranks(arrange_by = desc(score_pred), rank_col_name = "pred_score_rank") %>%
  f_create_ranks(arrange_by = desc(spread_line), rank_col_name = "spread_line_rank") %>%

  #f_create_ranks(arrange_by = desc(offense_weighted_dvoa), rank_col_name = "off_dvoa_rank") %>%
  #f_create_ranks(arrange_by = desc(offense_pass_dvoa), rank_col_name = "off_pass_dvoa_rank") %>%
  #f_create_ranks(arrange_by = desc(offense_rush_dvoa), rank_col_name = "off_rush_dvoa_rank") %>%

  #f_create_ranks(arrange_by = desc(adjusted_off_epa), rank_col_name = "off_epa_rank") %>%
  #f_create_ranks(arrange_by = desc(adjusted_off_pass_epa), rank_col_name = "off_pass_epa_rank") %>%
  #f_create_ranks(arrange_by = desc(adjusted_off_run_epa), rank_col_name = "off_rush_epa_rank") %>%

  #f_create_ranks(arrange_by = defense_weighted_dvoa, rank_col_name = "def_dvoa_rank") %>%
  #f_create_ranks(arrange_by = defense_pass_dvoa, rank_col_name = "def_pass_dvoa_rank") %>%
  #f_create_ranks(arrange_by = defense_rush_dvoa, rank_col_name = "def_rush_dvoa_rank") %>%

  #f_create_ranks(arrange_by = adjusted_def_epa, rank_col_name = "def_epa_rank") %>%
  #f_create_ranks(arrange_by = adjusted_def_pass_epa, rank_col_name = "def_pass_epa_rank") %>%
  #f_create_ranks(arrange_by = adjusted_def_run_epa, rank_col_name = "def_rush_epa_rank") %>%

  #dplyr::mutate(
    #across(offense_weighted_dvoa:adjusted_def_run_epa, ~ -2 + ((. - min(.)) * (2 - -2)) / (max(.) - min(.))),

    #offense_total_ranks_sum = (off_dvoa_rank + off_epa_rank) / 2,
    #defense_total_ranks_sum = (def_dvoa_rank + def_epa_rank) / 2,
    #matchup_total = defense_total_ranks_sum - offense_total_ranks_sum,

    #offense_pass_ranks_sum = (off_pass_dvoa_rank + off_pass_epa_rank) / 2,
    #defense_pass_ranks_sum = (def_pass_dvoa_rank + def_pass_epa_rank) / 2,
    #matchup_pass = defense_pass_ranks_sum - offense_pass_ranks_sum,

    #offense_rush_ranks_sum = (off_rush_dvoa_rank + off_rush_epa_rank) / 2,
    #defense_rush_ranks_sum = (def_rush_dvoa_rank + def_rush_epa_rank) / 2,
    #matchup_rush = defense_rush_ranks_sum - offense_rush_ranks_sum,

    #offense_ranks_sum = (offense_total_ranks_sum + offense_pass_ranks_sum + offense_rush_ranks_sum) / 3,
    #defense_ranks_sum = (defense_total_ranks_sum + defense_pass_ranks_sum + defense_rush_ranks_sum) / 3,

    #pred_gamescript_factor = dplyr::case_when(
    #  spread_line >= 10 ~ (offense_ranks_sum + offense_ranks_sum + defense_ranks_sum + defense_ranks_sum + offense_rush_ranks_sum + defense_rush_ranks_sum + sec_per_play_rank + sec_per_play_leading_rank + sec_per_play_leading_rank) / 9,
    #  spread_line >= 4.5 ~ (offense_ranks_sum + defense_ranks_sum + offense_rush_ranks_sum + defense_rush_ranks_sum + sec_per_play_rank + sec_per_play_leading_rank) / 6,
    #  spread_line <= -4.5 ~ (offense_ranks_sum + defense_ranks_sum + offense_pass_ranks_sum + defense_pass_ranks_sum + sec_per_play_rank + sec_per_play_trailing_rank) / 6,
    #  spread_line <= -10 ~ (offense_ranks_sum + offense_ranks_sum + defense_ranks_sum + defense_ranks_sum + offense_pass_ranks_sum + defense_pass_ranks_sum + sec_per_play_rank + sec_per_play_trailing_rank + sec_per_play_trailing_rank) / 9,
    #  T ~ (offense_ranks_sum + defense_ranks_sum + sec_per_play_rank) / 3
    #)

    #adjusted_off_epa = round(adjusted_off_epa, 3),
    #adjusted_off_pass_epa = round(adjusted_off_pass_epa, 3),
    #adjusted_off_run_epa = round(adjusted_off_run_epa, 3),
    #adjusted_def_epa = round(adjusted_def_epa, 3),
    #adjusted_def_pass_epa = round(adjusted_def_pass_epa, 3),
    #adjusted_def_run_epa = round(adjusted_def_run_epa, 3),

  #) %>%

  #f_create_ranks(arrange_by = pred_gamescript_factor, rank_col_name = "game_script_rank") %>%
  #f_create_ranks(arrange_by = desc(matchup_total), rank_col_name = "matchup_total_rank") %>%
  #f_create_ranks(arrange_by = desc(matchup_pass), rank_col_name = "matchup_pass_rank") %>%
  #f_create_ranks(arrange_by = desc(matchup_rush), rank_col_name = "matchup_rush_rank") %>%

  dplyr::left_join(
    nflTeams %>%
      dplyr::select(team_abbr, team_color, team_color2, team_logo_espn),
    by = c("team" = "team_abbr")
  )

  #dplyr::rowwise() %>%
  #dplyr::mutate(
  #  qb_score = round(sum(game_script_rank, matchup_total_rank, matchup_pass_rank, total_line_rank, pred_score_rank, spread_line_rank)),
  #  wr_score = round(sum(game_script_rank, matchup_total_rank, matchup_pass_rank, total_line_rank, pred_score_rank) + (33 - spread_line_rank)),
  #  rb_score = round(sum(game_script_rank, matchup_total_rank, matchup_rush_rank, total_line_rank, pred_score_rank, spread_line_rank)),
  #) %>%
  #dplyr::ungroup() %>%
  #dplyr::mutate(
  #  qb_pct = round(1 - (qb_score / max(qb_score)), 2),
  #  qb_pct_week = round(qb_pct / max(qb_pct), 2),
  #  wr_pct = round(1 - (wr_score / max(wr_score)), 2),
  #  wr_pct_week = round(wr_pct / max(wr_pct), 2),
  #  rb_pct = round(1 - (rb_score / max(rb_score)), 2),
  #  rb_pct_week = round(rb_pct / max(rb_pct), 2)
  #)



