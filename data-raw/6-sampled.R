# load("data/Guesses.rda")

Sampled <- Guesses %>%
  label_session_duration() %>%
  group_by(Exp, SessionID) %>%
  do({ sample_session(.data) }) %>%
  ungroup()

devtools::use_data(Sampled, overwrite = TRUE)
