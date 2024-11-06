player_stats <- nflreadr::load_player_stats(var.season, "offense")

qb_stats <- player_stats %>%
  dplyr::filter(position == "QB") %>%
  dplyr::select(
    player_id, player_name, position, recent_team, opponent_team, week,
    completions, attempts, passing_yards, passing_air_yards, pacr, passing_tds, interceptions, sacks, dakota,
    carries, rushing_yards, rushing_tds,
    fantasy_points_ppr
  ) %>%
  dplyr::mutate(
    passing_yards_per_attempt = round(passing_yards / attempts, 1),
    pacr = round(pacr, 2),
    dakota = round(dakota, 2),
    rushing_yards_per_carry = round(rushing_yards / carries, 1),
  ) %>%
  dplyr::group_by(player_id) %>%
  dplyr::mutate(
    ppg = round(sum(fantasy_points_ppr) / n(), 2)
  ) %>%
  dplyr::arrange(dplyr::desc(week)) %>%
  dplyr::ungroup()

skill_stats <- player_stats %>%
  dplyr::filter(position != "QB") %>%
  dplyr::select(
    player_id, player_name, position, recent_team, opponent_team, week,
    carries, rushing_yards, rushing_tds,
    targets, receptions, target_share, receiving_yards, receiving_air_yards, air_yards_share, racr, wopr, receiving_yards_after_catch, receiving_tds,
    fantasy_points_ppr
  ) %>%
  dplyr::mutate(
    rushing_yards_per_carry = round(rushing_yards / carries, 1),
    target_share = round(target_share, 2) * 100,
    air_yards_share = round(air_yards_share, 2) * 100,
    racr = round(racr, 2),
    wopr = round(wopr, 2)
  ) %>%
  dplyr::group_by(player_id) %>%
  dplyr::mutate(
    ppg = round(sum(fantasy_points_ppr) / n(), 2)
  ) %>%
  dplyr::arrange(dplyr::desc(week)) %>%
  dplyr::ungroup()

default_stats_gt <- function(df) {
  df %>%
    dplyr::select(-position) %>%
    gt::gt() %>%
    nflplotR::gt_nfl_headshots(player_id) %>%
    gtExtras::gt_merge_stack(col1 = player_name, col2 = recent_team) %>%
    gt::tab_spanner(
      label = "Fantasy",
      columns = c(fantasy_points_ppr, ppg)
    ) %>%
    gt::cols_label(
      player_name = "Name",
      opponent_team = "OPP",
      week = "WK",
      fantasy_points_ppr = "FPTS",
      ppg = "FPTS/G"
    ) %>%
    gt::data_color(
      method = "numeric",
      palette = c("red", "green")
    )
}

passing_stats_gt <- function(df) {
  df %>%
    gt::tab_spanner(
      label = "Passing",
      columns = c(completions, attempts, passing_yards, passing_yards_per_attempt, passing_air_yards, pacr, passing_tds, interceptions, sacks, dakota)
    ) %>%
    gt::cols_label(
      completions = "CMP",
      attempts = "ATT",
      passing_yards = "YDS",
      passing_yards_per_attempt = "Y/A",
      pacr = "PACR",
      passing_air_yards = "AY",
      passing_tds = "TD",
      interceptions = "INT",
      sacks = "SCK",
      dakota = "DAKOTA"
    )
}

rushing_stats_gt <- function(df) {
  df %>%
    gt::tab_spanner(
      label = "Rushing",
      columns = c(carries, rushing_yards, rushing_yards_per_carry, rushing_tds)
    ) %>%
    gt::cols_label(
      carries = "ATT",
      rushing_yards = "YDS",
      rushing_yards_per_carry = "Y/A",
      rushing_tds = "TD"
    )
}

receiving_stats_gt <- function(df) {
  df %>%
    gt::tab_spanner(
      label = "Receiving",
      columns = c(targets, receptions, target_share, receiving_yards, receiving_air_yards, air_yards_share, racr, wopr, receiving_yards_after_catch, receiving_tds)
    ) %>%
    gt::cols_label(
      targets = "TGT",
      receptions = "REC",
      target_share = "TGT%",
      receiving_yards = "YDS",
      receiving_air_yards = "AY",
      air_yards_share = "AY%",
      racr = "RACR",
      wopr = "WOPR",
      receiving_yards_after_catch = "YAC",
      receiving_tds = "TD"
    )
}
