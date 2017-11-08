# load("data/Guesses.rda")

starting_inventory <- data_frame(
  NumGuesses = 0,
  InventoryID = "1-2-3-4-5-6",
  InventorySize = 6,
  GuessesHash = digest::digest(""),
  NumUniqueGuesses = 0
)

Sampled <- Guesses %>%
  arrange(SessionTime) %>%
  group_by(Exp, SessionID) %>%
  do({ sample_session(., default = starting_inventory) }) %>%
  ungroup() %>%
  label_session_player() %>%
  label_team_id() %>%
  label_strategy() %>%
  label_session_duration() %>%
  label_players_per_session() %>%
  label_generation() %>%
  label_player_conditions() %>%
  label_time() %>%
  label_session_status() %>%
  label_team_status()

devtools::use_data(Sampled, overwrite = TRUE)
