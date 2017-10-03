data_performance <- function(con) {
  TeamPerformance <- Guesses %>%
    left_join(PlayerInfo) %>%
    # !!! Exclude diachronic players with generations > 2
    filter(Generation <= 2) %>%
    # !!! Exclude synchronic teams with team size == 4
    # ...
    group_by(TeamID) %>%
    summarize(
      NumInnovations = sum(TeamUniqueItem),
      NumGuesses = max(TeamGuessNum),
      NumUniqueGuesses = sum(TeamUniqueGuess)
    ) %>%
    # Add in Score from Player table
    left_join(
      Player %>%
        left_join(PlayerInfo) %>%
        filter(Generation <= 2) %>%
        group_by(TeamID) %>%
        summarize(Score = max(Score))
    ) %>%
    left_join(TeamInfo)

  PlayerPerformance <- Guesses %>%
    group_by(PlayerID) %>%
    summarize(
      NumInnovations = sum(UniqueItem),
      NumGuesses = max(GuessNum),
      NumUniqueGuesses = sum(UniqueGuess),
      NumTeamUniqueGuesses = sum(TeamUniqueGuess)
    ) %>%
    # Add in Score from Player table
    left_join(select(Player, PlayerID, Score))
}
