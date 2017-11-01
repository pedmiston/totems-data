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

PlayerPerformance <- Guesses %>%
  group_by(Exp, SessionID) %>%
  summarize(
    NumInnovations = sum(UniqueSessionResult),
    NumGuesses = max(NumSessionGuess),
    NumUniqueGuesses = sum(UniqueSessionGuess)
  ) %>%
  ungroup() %>%
  left_join(Players)

devtools::use_data(TeamPerformance, PlayerPerformance, overwrite = TRUE)
