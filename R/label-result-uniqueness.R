#' Label results as unique and accumulate them.
label_result_uniqueness <- function(frame) {
  frame %>%
    label_unique_session_results() %>%
    label_unique_player_results() %>%
    label_unique_team_results()
}

#' Assess the uniqueness of
label_unique_session_results <- function(frame) {
  Guesses %>%
    rowwise() %>%
    mutate(
      UniqueSessionItem = !(Result %in% PrevSessionInventory),
      SessionInventorySize = length(PrevSessionInventory),
      SessionInventoryID = inventory_to_id(PrevSessionInventory)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this player.
label_unique_player_results <- function(Guesses) {
  Guesses %>%
    rowwise() %>%
    mutate(
      UniquePlayerItem = !(Result %in% PrevPlayerInventory),
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
      UniqueTeamItem = !(Result %in% PrevTeamInventory),
      TeamInventorySize = length(PrevTeamInventory),
      TeamInventoryID = inventory_to_id(PrevTeamInventory)
    ) %>%
    ungroup()
}

inventory_to_id <- function(inventory) {
    paste(inventory, collapse = "-")
}
