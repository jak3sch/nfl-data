flow_chart_data <- player_stats %>%
  dplyr::filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  dplyr::group_by(season, week, position) %>%
  dplyr::arrange(dplyr::desc(fantasy_points_ppr)) %>%
  dplyr::mutate(rank = row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(player_id, player_name, position, recent_team, season, week, fantasy_points_ppr, rank, opponent_team)

flow_chart_data_filtered <- shiny::reactive({
  flow_chart_data %>%
    dplyr::filter(rank <= input$selectTopFinish) %>%
    dplyr::group_by(season, position, opponent_team) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    dplyr::left_join(
      matchups %>%
        dplyr::select(team, opponent),
      by = c("opponent_team" = "team")
    ) %>%
    dplyr::left_join(
      player_stats %>%
        dplyr::group_by(player_id) %>%
        dplyr::arrange(dplyr::desc(week)) %>%
        dplyr::summarise(
          fpts = sum(fantasy_points_ppr),
          opponent_player = first(player_name),
          recent_team = first(recent_team),
          position = first(position),
          .groups = "drop"
        ) %>%
        dplyr::group_by(recent_team, position) %>%
        dplyr::arrange(dplyr::desc(fpts)) %>%
        dplyr::summarise(
          opponent_player = paste(opponent_player, collapse = ", "),
          .groups = "drop"
        ),
      by = c("opponent" = "recent_team", "position"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(!is.na(opponent)) %>%
    dplyr::select(-season)
})

output$flow_chart <- gt::render_gt({
  flow_chart_data_filtered() %>%
    dplyr::group_by(position) %>%
    gt::gt() %>%
    gt::tab_spanner(
      label = paste("Week", var.currentWeek, "Opponent"),
      columns = c("opponent", "opponent_player")
    ) %>%
    gt::data_color(
      columns = count,
      palette = c("red", "green")
    ) %>%
    gt::cols_label(
      "opponent_team" = "Team",
      "count" = paste("Top", input$selectTopFinish, "Games Allowed"),
      "opponent" = "Team",
      "opponent_player" = "Players"
    )
})
