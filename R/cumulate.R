#' Cumulate unique items and guesses by player and by team
cumulate_uniques <- function(frame) {
  frame %>%
    cumulate_player_uniques() %>%
    cumulate_team_uniques()
}

cumulate_player_uniques <- function(frame) {
  frame %>%
    group_by(PlayerID) %>%
    arrange(PlayerTime) %>%
    mutate(
      NumUniqueItems = cumsum(UniqueItem),
      NumUniqueGuesses = cumsum(UniqueGuess)
    ) %>%
    ungroup()
}

cumulate_team_uniques <- function(frame) {
  frame %>%
    group_by(TeamID) %>%
    arrange(TeamTime) %>%
    mutate(
      NumUniqueTeamItems = cumsum(UniqueTeamItem),
      NumUniqueTeamGuesses = cumsum(UniqueTeamGuess)
    ) %>%
    ungroup()
}
