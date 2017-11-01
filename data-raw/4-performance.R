# load("data/Guesses.rda")

TeamPerformance <- Guesses %>%
  group_by(Exp, TeamID) %>%
  summarize(
    NumInnovations = sum(UniqueTeamResult),
    NumGuesses = max(NumTeamGuess),
    NumUniqueGuesses = sum(UniqueTeamGuess)
  ) %>%
  ungroup()
