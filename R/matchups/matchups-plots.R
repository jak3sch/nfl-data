matchup_plot_defaults <- function() {
  list(
    ggplot2::geom_vline(xintercept = 0, color = var.lightgrey),
    ggplot2::geom_hline(yintercept = 0, color = var.lightgrey),
    
    ggplot2::scale_x_continuous(breaks = seq(-2.5, 2.5, by = 0.5)),
    ggplot2::scale_y_continuous(breaks = seq(-2.5, 2.5, by = 0.5)),
    
    ggplot2::theme_minimal(),
    ggplot2::theme(
      plot.title = element_text(size = 18, color = var.darkgrey, hjust = 0.5, margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_text(size = 12, color = var.darkgrey, hjust = 0.5, margin = margin(0, 0, 30, 0)),
      panel.grid = element_line(color = "#dff9fb"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 0, 10, 20),
      axis.title = element_text(size = 12, color = var.darkgrey),
      axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
      axis.text = element_text( size = 8, color = var.darkgrey)
    )
  )
}

matchup_overview_plot_defaults <- function() {
  list(
    matchup_plot_defaults(),
    nflplotR::geom_nfl_logos(aes(team_abbr = team, width = spread_line_size)),
    ggrepel::geom_label_repel(aes(label = paste(team, ":", opponent, "\n", spread_line, total_line)), size = 2.5, fill = "#dff9fb", color = var.darkgrey)
  )
}

plot_single_matchup <- function() {
  list(
    matchup_plot_defaults(),
    facet_wrap(vars(group), ncol = 1),
    ggplot2::geom_point(aes(color = team, fill = team), shape = 21, size = 4, stroke = 1.2),
    nflplotR::scale_color_nfl(alpha = 0.4),
    nflplotR::scale_fill_nfl(alpha = 0.1),
    ggplot2::theme(
      plot.title = element_text(size = 12, color = var.darkgrey, hjust = 0.5, margin = margin(0, 0, 10, 0)),
      axis.title = element_text(size = 10, color = var.darkgrey),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(color = var.darkgrey, margin = margin(30, 0, 25, 0), face = "bold", size = 10)
    ),
    ggplot2::labs(
      #title = paste(matchup, " Matchups"),
      x = "Offense",
      y = "Opponent Defense"
    )
  )
}

plot_matchup_dvoa_total <- function() {
  list(
    nflplotR::geom_nfl_logos(data = subset(single_matchup_charts, matchup == input$selectMatchup & category %in% c("total_dvoa", "total_epa")), aes(team_abbr = team, width = 0.12))
  )
}

plot_matchup_dvoa_pass <- function() {
  list(
    nflplotR::geom_nfl_logos(data = subset(single_matchup_charts, matchup == input$selectMatchup & category %in% c("pass_dvoa", "pass_epa")), aes(team_abbr = team, width = 0.12))
  )
}

plot_matchup_dvoa_rush <- function() {
  list(
    nflplotR::geom_nfl_logos(data = subset(single_matchup_charts, matchup == input$selectMatchup & category %in% c("rush_dvoa", "rush_epa")), aes(team_abbr = team, width = 0.12))
  )
}

matchup_annotations <- function(offense, defense) {
  list(
    geom_vline(xintercept = mean(offense), color = var.red, linetype = "dashed"),
    geom_hline(yintercept = mean(defense), color = var.red, linetype = "dashed"),
    ggplot2::annotate(
      "text",
      x = min(offense),
      y = max(defense),
      label = "Bad Offense vs\nBad Defense",
      hjust = -0.1,
      color = var.darkgrey,
      size = 3
    ),
    ggplot2::annotate(
      "text",
      x = max(offense),
      y = max(defense),
      label = "Good Offense vs\nBad Defense",
      hjust = 0.75,
      color = var.darkgrey,
      size = 3
    ),
    ggplot2::annotate(
      "text",
      x = min(offense),
      y = min(defense),
      label = "Bad Offense vs\nGood Defense",
      hjust = -0.1,
      color = var.darkgrey,
      size = 3
    ),
    ggplot2::annotate(
      "text",
      x = max(offense),
      y = min(defense),
      label = "Good Offense vs\nGood Defense",
      hjust = 0.75,
      color = var.darkgrey,
      size = 3
    )
  )
}