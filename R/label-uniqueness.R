#' Label guesses as unique and count cumulate unique guesses
label_guess_uniqueness <- function(frame) {
  frame %>%
    label_unique_session_guesses() %>%
    label_unique_player_guesses() %>%
    label_unique_team_guesses()
}

label_unique_session_guesses <- function(frame) {
  frame %>%
    accumulate_session_guesses() %>%
    rowwise() %>%
    mutate(
      UniqueSessionGuess = !(Guess %in% PrevSessionGuesses),
      NumUniqueSessionGuesses = length(PrevSessionGuesses)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each Guess for this player.
label_unique_player_guesses <- function(frame) {
  frame %>%
    accumulate_player_guesses() %>%
    rowwise() %>%
    mutate(
      UniquePlayerGuess = !(Guess %in% PrevPlayerGuesses),
      NumUniquePlayerGuesses = length(PrevPlayerGuesses)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each Guess for this team.
label_unique_team_guesses <- function(frame) {
  frame %>%
    accumulate_team_guesses() %>%
    rowwise() %>%
    mutate(
      UniqueTeamGuess = !(Guess %in% PrevTeamGuesses),
      NumUniqueTeamGuesses = length(PrevTeamGuesses)
    ) %>%
    ungroup()
}

#' Label items as unique and count cumulate unique items.
label_item_uniqueness <- function(frame) {
  frame %>%
    label_unique_session_items() %>%
    label_unique_player_items() %>%
    label_unique_team_items()
}

label_unique_session_items <- function(frame) {
  Guesses %>%
    accumulate_session_inventory() %>%
    rowwise() %>%
    mutate(
      UniqueSessionItem = !(Result %in% PrevSessionInventory),
      SessionInventorySize = length(PrevSessionInventory),
      SessionInventoryID = inventory_to_id(PrevSessionInventory)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this player.
label_unique_player_items <- function(Guesses) {
  Guesses %>%
    accumulate_player_inventory() %>%
    rowwise() %>%
    mutate(
      UniquePlayerItem = !(Result %in% PrevPlayerInventory),
      PlayerInventorySize = length(PrevPlayerInventory),
      PlayerInventoryID = inventory_to_id(PrevPlayerInventory)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this team.
label_unique_team_items <- function(frame) {
  frame %>%
    accumulate_team_inventory() %>%
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
