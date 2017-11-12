
#' Return a list containing theme elements to be used across visualizations.
#' @export
load_totems_theme <- function() {
  strategy_levels = levels(recode_strategy()$StrategyLabel)
  strategy_colors = RColorBrewer::brewer.pal(length(strategy_levels), "Set2")
  strategy_named_colors <- strategy_colors
  names(strategy_named_colors) <- c("green", "orange", "blue")

  totems_theme <- list(
    # Theme
    base_theme = ggplot2::theme_minimal(),
    # Colors
    synchronic_color = strategy_colors[1],
    isolated_color = strategy_colors[2],
    diachronic_color = strategy_colors[3],
    color_picker = function(x) unname(strategy_named_colors[x]),

    # Strategy scales
    scale_x_strategy = ggplot2::scale_x_discrete("Strategy", labels = strategy_levels),
    scale_y_num_innovations = ggplot2::scale_y_continuous(
      "Number of innovations",
      breaks = seq(0, 30, by = 2)
    ),
    scale_color_strategy = ggplot2::scale_color_manual("Strategy", values = strategy_colors),
    scale_fill_strategy = ggplot2::scale_fill_manual("Strategy", values = strategy_colors)
  )

  totems_theme
}
