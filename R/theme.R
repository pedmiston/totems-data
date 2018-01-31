
#' Return a list containing theme elements to be used across visualizations.
#' @import ggplot2
#' @export
load_totems_theme <- function() {
  strategy_levels = levels(recode_strategy()$StrategyLabel)
  strategy_colors = RColorBrewer::brewer.pal(4, "Set2")
  strategy_named_colors <- strategy_colors
  names(strategy_named_colors) <- c("green", "orange", "blue", "pink")

  breaks_in_minutes <- seq(0, 50, by = 25)

  totems_theme <- list(
    # Theme
    base_theme = theme_minimal(),
    # Colors
    synchronic_color = strategy_colors[1],
    isolated_color = strategy_colors[2],
    diachronic_color = strategy_colors[3],
    color_picker = function(x) unname(strategy_named_colors[x]),

    # Strategy scales
    scale_x_strategy = scale_x_discrete("Strategy", labels = strategy_levels),
    scale_y_num_tools = scale_y_continuous("Tools created", breaks = seq(0, 30, by = 2)),
    scale_y_num_innovations = scale_y_continuous(
      "Number of innovations",
      breaks = seq(0, 30, by = 2)
    ),
    scale_color_strategy = scale_color_manual("Strategy", values = strategy_colors),
    scale_fill_strategy = scale_fill_manual("Strategy", values = strategy_colors),

    scale_x_calendar_time = scale_x_continuous("Calendar time (min)", breaks = breaks_in_minutes),
    scale_x_team_time = scale_x_continuous("Labor time (min)", breaks = breaks_in_minutes),
    scale_x_player_time = scale_x_continuous("Learning time (min)", breaks = breaks_in_minutes)
  )

  totems_theme
}
