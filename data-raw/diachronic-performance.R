summarize_performance <- . %>%
  summarize(
    NumGuesses = n(),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumInnovations = sum(GuessType == "unique_guess")
  )

DiachronicPerformance_RelPlayer <- Guesses %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize_performance()

DiachronicPerformance_RelTeam <- Guesses %>%
  recode_guess_type("UniqueTeamGuess", "UniqueTeamResult") %>%
  group_by(SessionID) %>%
  summarize_performance()

DiachronicPerformance <- bind_rows(
    team_guesses = DiachronicPerformance_RelPlayer,
    player_guesses = DiachronicPerformance_RelTeam,
    .id = "GuessType"
  ) %>%
  label_session_player() %>%
  label_team_id() %>%
  label_strategy() %>%
  label_generation()

use_data(DiachronicPerformance, overwrite = TRUE)
