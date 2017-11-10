# load("data/Guesses.rda")
# load("data/Teams.rda")

TeamPerformance <- Guesses %>%
  recode_guess_type("UniqueTeamGuess", "UniqueTeamResult") %>%
  group_by(Exp, Strategy, SessionDuration, TeamID) %>%
  summarize(
    NumGuesses = max(NumTeamGuess),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumInnovations = sum(UniqueTeamResult)
  ) %>%
  ungroup() %>%
  left_join(Teams) %>%
  label_team_status()

PlayerPerformance <- Guesses %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(Exp, SessionID) %>%
  summarize(
    NumGuesses = max(NumSessionGuess),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumInnovations = sum(UniqueSessionResult)
  ) %>%
  ungroup() %>%
  left_join(Players) %>%
  label_team_status() %>%
  label_session_status()

devtools::use_data(TeamPerformance, PlayerPerformance, overwrite = TRUE)
