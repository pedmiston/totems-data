# load("data/Guesses.rda")
# load("data/Teams.rda")

TeamPerformance <- Guesses %>%
  group_by(Exp, TeamID) %>%
  summarize(
    NumInnovations = sum(UniqueTeamResult),
    NumGuesses = max(NumTeamGuess),
    NumUniqueGuesses = sum(UniqueTeamGuess)
  ) %>%
  ungroup() %>%
  left_join(Teams)

devtools::use_data(TeamPerformance, overwrite = TRUE)
