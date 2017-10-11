data_team_performance <- function() {
  Guesses <- data_guesses()
  TeamPerformance <- Guesses %>%
    group_by(Exp, TeamID) %>%
    summarize(
      TeamScore = sum(TeamScore),
      TeamGuesses = max(TeamGuessNum),
      UniqueTeamGuesses = max(NumUniqueTeamGuesses),
      TeamInventorySize = max(TeamInventorySize)
    ) %>%
    join_teams()
  TeamPerformance
}

data_player_performance <- function() {
  Guesses <- data_guesses()
  PlayerPerformance <- Guesses %>%
    group_by(Exp, PlayerID) %>%
    summarize(
      Score = sum(Score),
      NumGuesses = max(GuessNum),
      UniqueGuesses = max(NumUniqueGuesses),
      UniquePlayerItems = max(NumUniquePlayerItems)
    ) %>%
    join_players() %>%
    join_teams()
}

filter_valid_players <- function(frame) {
  frame
}
