#' Keep track of accumulating variables.
#'
#' @examples
#' items <- c("a", "b", "b", "c")
#' rolling(items)
#'
#' @import magrittr
#' @export
rolling <- function(items, default = NA) {
  results <- list()
  if (length(default) > 1) default <- sort(default)
  for(i in seq_along(items)) {
    if (i == 1) results[[i]] <- default
    else {
      results[[i]] <- c(results[[i-1]], items[i-1]) %>%
        .[!is.na(.)] %>%
        unique() %>%
        sort()
    }
  }
  results
}

#' Create PrevGuesses list column containing
#' unique guesses made by this player.
rolling_player_guesses <- function(Guesses) {
  Guesses %>%
    group_by(PlayerID) %>%
    arrange(GuessNum) %>%
    mutate(PrevGuesses = rolling(Guess)) %>%
    ungroup() %>%
    arrange(PlayerID, GuessNum)
}

#' Create PrevTeamGuesses list column containing
#' unique guesses made by this team.
rolling_team_guesses <- function(Guesses) {
  Guesses %>%
    group_by(TeamID) %>%
    arrange(TeamGuessNum) %>%
    mutate(PrevTeamGuesses = rolling(Guess)) %>%
    ungroup() %>%
    arrange(TeamID, TeamGuessNum)
}

#' Create PrevInventory list column containing
#' unique inventories held by this player.
rolling_player_inventory <- function(Guesses) {
  Guesses %>%
    group_by(PlayerID) %>%
    arrange(GuessNum) %>%
    mutate(PrevInventory = rolling(Result, default = 1:6)) %>%
    ungroup() %>%
    arrange(PlayerID, GuessNum)
}

#' Create PrevTeamInventory list column containing
#' unique inventories held by this team.
rolling_team_inventory <- function(Guesses) {
  Guesses %>%
    group_by(TeamID) %>%
    arrange(TeamGuessNum) %>%
    mutate(PrevTeamInventory = rolling(Result, default = 1:6)) %>%
    ungroup() %>%
    arrange(TeamID, TeamGuessNum)
}

#' Assess the uniqueness of each Guess for this player.
label_unique_guesses <- function(frame) {
  frame %>%
    rolling_player_guesses() %>%
    rowwise() %>%
    mutate(
      UniqueGuess = !(Guess %in% PrevGuesses),
      NumUniqueGuesses = length(PrevGuesses)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each Guess for this team.
label_unique_team_guesses <- function(frame) {
  frame %>%
    rolling_team_guesses() %>%
    rowwise() %>%
    mutate(
      UniqueTeamGuess = !(Guess %in% PrevTeamGuesses),
      NumUniqueTeamGuesses = length(PrevTeamGuesses)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this player.
label_unique_items <- function(Guesses) {
  Guesses %>%
    rolling_player_inventory() %>%
    rowwise() %>%
    mutate(
      UniqueItem = !(Result %in% PrevInventory),
      InventorySize = length(PrevInventory)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this team.
label_unique_team_items <- function(frame) {
  frame %>%
    rolling_team_inventory() %>%
    rowwise() %>%
    mutate(
      UniqueTeamItem = !(Result %in% PrevTeamInventory),
      TeamInventorySize = length(PrevTeamInventory)
    ) %>%
    ungroup()
}
