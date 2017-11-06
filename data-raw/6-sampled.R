# load("data/Guesses.rda")

Sampled <- Guesses %>%
  label_session_duration() %>%
  group_by(Exp, SessionID) %>%
  do({ sample_session(.data) }) %>%
  ungroup() %>%
  mutate(SessionTime = SampledSessionTime) %>%
  label_players_per_session() %>%
  label_time()

devtools::use_data(Sampled, overwrite = TRUE)
