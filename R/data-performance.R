data_team_performance <- function() {
  Guesses <- data_guesses()
  TeamPerformance <- Guesses %>%
    label_experiment() %>%
    filter_valid_players() %>%
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
    label_experiment() %>%
    filter_valid_players() %>%
    group_by(Exp, PlayerID) %>%
    summarize(
      Score = sum(Score),
      NumGuesses = max(GuessNum),
      UniqueGuesses = max(NumUniqueGuesses),
      UniqueItems = max(NumUniqueItems)
    ) %>%
    join_players() %>%
    join_teams()
}

filter_valid_players <- function(frame) {
  frame
}
