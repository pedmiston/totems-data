#' Summarize the team's final performance.
#'
#' @import dplyr
#' @export
summarize_team_performance <- function(frame) {
  frame %>%
    group_by(Strategy, ID_Group) %>%
    summarize(Score = max(Score),
              InventorySize = max(InventorySize),
              DifficultyScore = max(DifficultyScore)) %>%
    recode_strategy()
}
