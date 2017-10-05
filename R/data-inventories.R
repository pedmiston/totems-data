data_inventories <- function() {
  Guesses <- data_guesses()
  Inventories <- Guesses %>%
    group_by(TeamID, NumUniqueTeamItems) %>%
    summarize(
      TeamGuesses = n(),
      UniqueTeamGuesses = sum(UniqueTeamGuess),
      TotalUniqueGuesses = sum(UniqueGuess),
      UniqueGuessesPerPlayer = sum(UniqueGuess)/unique(NumCurrentPlayers)[[1]],
      Duration = max(TeamTime) - min(TeamTime)
    ) %>%
    ungroup() %>%
    arrange(TeamID, NumUniqueTeamItems) %>%
    join_teams()
}
