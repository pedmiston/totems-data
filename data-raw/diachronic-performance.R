library(tidyr)

filter_valid_diachronic <- . %>%
  dplyr::filter(Strategy == "Diachronic", SessionStatus == "valid")

summarize_performance <- . %>%
  count(GuessType) %>%
  mutate(PropGuesses = n/sum(n)) %>%
  ungroup() %>%
  rename(NumGuesses = n)

fill_missing_guess_types <- function(frame) {
  session_ids <- unique(frame$SessionID)
  guess_types_per_session_id <- expand.grid(
    SessionID = session_ids,
    GuessType = recode_guess_type()$GuessType,
    stringsAsFactors = FALSE
  )
  left_join(guess_types_per_session_id, frame) %>%
    replace_na(list(NumGuesses = 0, PropGuesses = 0))
}

fill_missing_guess_types_by_stage <- function(frame) {
  session_ids <- unique(frame$SessionID)
  guess_types_per_session_id <- expand.grid(
    SessionID = session_ids,
    Stage = c("learning", "playing"),
    GuessType = recode_guess_type()$GuessType,
    stringsAsFactors = FALSE
  )
  left_join(guess_types_per_session_id, frame) %>%
    replace_na(list(NumGuesses = 0, PropGuesses = 0))
}

# DiachronicPerformance ----
DiachronicPerformance_RelPlayer <- Guesses %>%
  filter_valid_diachronic() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize_performance() %>%
  fill_missing_guess_types()

DiachronicPerformance_RelTeam <- Guesses %>%
  filter_valid_diachronic() %>%
  recode_guess_type("UniqueTeamGuess", "UniqueTeamResult") %>%
  group_by(SessionID) %>%
  summarize_performance() %>%
  fill_missing_guess_types()

DiachronicPerformance <- bind_rows(
    player_guesses = DiachronicPerformance_RelPlayer,
    team_guesses = DiachronicPerformance_RelTeam,
    .id = "GuessesRel"
  ) %>%
  label_session_player() %>%
  label_team_id() %>%
  label_strategy() %>%
  label_generation() %>%
  arrange(TeamID, Generation, SessionID, GuessType, GuessesRel)

use_data(DiachronicPerformance, overwrite = TRUE)

# DiachronicPerformanceByStage ----
DiachronicPerformanceByStage_RelPlayer <- Guesses %>%
  filter_valid_diachronic() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID, Stage) %>%
  summarize_performance() %>%
  fill_missing_guess_types_by_stage()

DiachronicPerformanceByStage_RelTeam <- Guesses %>%
  filter_valid_diachronic() %>%
  recode_guess_type("UniqueTeamGuess", "UniqueTeamResult") %>%
  group_by(SessionID, Stage) %>%
  summarize_performance() %>%
  fill_missing_guess_types_by_stage()

DiachronicPerformanceByStage <- bind_rows(
  player_guesses = DiachronicPerformanceByStage_RelPlayer,
  team_guesses = DiachronicPerformanceByStage_RelTeam,
  .id = "GuessesRel"
) %>%
  label_session_player() %>%
  label_team_id() %>%
  label_strategy() %>%
  label_generation() %>%
  dplyr::filter(!(Generation == 1 & Stage == "learning")) %>%
  arrange(TeamID, Generation, SessionID, GuessType, GuessesRel)

use_data(DiachronicPerformanceByStage, overwrite = TRUE)
