data_inventories <- function() {
  Guesses <- data_guesses()
  Inventories <- Guesses %>%
    group_by(TeamID, TeamInventorySize) %>%
    summarize(
      TeamGuesses = n(),
      UniqueTeamGuesses = sum(UniqueTeamGuess),
      TotalUniqueGuesses = sum(UniqueGuess),
      UniqueGuessesPerPlayer = sum(UniqueGuess)/unique(NumCurrentPlayers)[[1]],
      Duration = max(TeamTime) - min(TeamTime),
      NewItem = ifelse(sum(UniqueTeamItem) == 0, 0, Result[UniqueTeamItem == 1]),
      DiscoveredBy = ifelse(sum(UniqueTeamItem) == 0, "", PlayerID[UniqueTeamItem == 1])
    ) %>%
    ungroup() %>%
    arrange(TeamID, TeamInventorySize) %>%
    join_teams()
}
