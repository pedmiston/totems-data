#' Label results as unique and accumulate them.
label_result_uniqueness <- function(frame) {
  frame %>%
    label_unique_session_results() %>%
    label_unique_player_results() %>%
    label_unique_team_results()
}

#' Assess the uniqueness of results created in this session.
label_unique_session_results <- function(frame) {
  frame %>%
    rowwise() %>%
    mutate(
      UniqueSessionResult = !(Result %in% PrevSessionInventory),
      SessionInventorySize = length(PrevSessionInventory),
      SessionInventoryID = inventory_to_id(PrevSessionInventory)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this player.
label_unique_player_results <- function(frame) {
  frame %>%
    rowwise() %>%
    mutate(
      UniquePlayerResult = !(Result %in% PrevPlayerInventory),
      PlayerInventorySize = length(PrevPlayerInventory),
      PlayerInventoryID = inventory_to_id(PrevPlayerInventory)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this team.
label_unique_team_results <- function(frame) {
  frame %>%
    rowwise() %>%
    mutate(
      UniqueTeamResult = !(Result %in% PrevTeamInventory),
      TeamResultsSize = length(PrevTeamInventory),
      TeamResultsID = inventory_to_id(PrevTeamInventory)
    ) %>%
    ungroup()
}

