#' Label results as unique and accumulate them.
label_result_uniqueness <- function(Guesses) {
  Guesses %>%
    label_unique_session_results() %>%
    label_unique_player_results() %>%
    label_unique_team_results()
}

#' Assess the uniqueness of results created in this session.
label_unique_session_results <- function(frame) {
  frame %>%
    group_by(PrevSessionInventoryID) %>%
    mutate(
      UniqueSessionResult = Result != 0 & !(Result %in% PrevSessionInventory[[1]]),
      SessionInventorySize = UniqueSessionResult + length(PrevSessionInventory[[1]])
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this player.
label_unique_player_results <- function(frame) {
  frame %>%
    group_by(PrevPlayerInventoryID) %>%
    mutate(
      UniquePlayerResult = Result != 0 & !(Result %in% PrevPlayerInventory[[1]]),
      PlayerInventorySize = UniquePlayerResult + length(PrevPlayerInventory[[1]])
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this team.
label_unique_team_results <- function(frame) {
  frame %>%
    group_by(PrevTeamInventoryID) %>%
    mutate(
      UniqueTeamResult = Result != 0 & !(Result %in% PrevTeamInventory[[1]]),
      TeamInventorySize = UniqueTeamResult + length(PrevTeamInventory[[1]])
    ) %>%
    ungroup()
}
