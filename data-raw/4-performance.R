# load("data/Guesses.rda")
# load("data/Teams.rda")

TeamPerformance <- Guesses %>%
  label_session_duration() %>%
  group_by(Exp, Strategy, SessionDuration, TeamID) %>%
  summarize(
    NumInnovations = sum(UniqueTeamResult),
    NumRepeatedInnovations = sum(Result != 0) - sum(UniqueTeamResult),
    NumGuesses = max(NumTeamGuess),
    NumUniqueGuesses = sum(UniqueTeamGuess),
    NumRedundantGuesses = sum(Result == 0) - sum(UniqueTeamGuess)
  ) %>%
  ungroup() %>%
  left_join(Teams) %>%
  label_team_status()

PlayerPerformance <- Guesses %>%
  group_by(Exp, SessionID) %>%
  summarize(
    NumInnovations = sum(UniqueSessionResult),
    NumRepeatedInnovations = sum(Result != 0) - sum(UniqueTeamResult),
    NumGuesses = max(NumSessionGuess),
    NumUniqueGuesses = sum(UniqueSessionGuess),
    NumRedundantGuesses = sum(Result == 0) - sum(UniqueTeamGuess)
  ) %>%
  ungroup() %>%
  left_join(Players) %>%
  label_team_status() %>%
  label_session_status()

devtools::use_data(TeamPerformance, PlayerPerformance, overwrite = TRUE)
