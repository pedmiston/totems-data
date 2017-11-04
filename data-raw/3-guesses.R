# devtools::load_all()

Guesses <- read_table("Table_Workshop") %>%
  rename(Guess = WorkShopString, Result = WorkShopResult) %>%
  replace_id_player() %>%
  replace_trial_time() %>%
  fix_bug_in_session_time() %>%
  label_team_id() %>%
  label_strategy() %>%
  label_generation() %>%
  label_players_per_session() %>%
  label_player_conditions() %>%
  label_time() %>%
  accumulate_session() %>%
  accumulate_player() %>%
  accumulate_team() %>%
  label_guess_uniqueness() %>%
  label_result_uniqueness() %>%
  label_score() %>%
  label_session_status() %>%
  label_team_status()

# DEBUG: Determine which SessionIDs don't have any Guesses associated.
filter_valid_sessions <- . %>% dplyr::filter(SessionStatus == "V") %>% .$SessionID %>% unique()

session_ids <- filter_valid_sessions(Players)
session_ids_in_guesses <- filter_valid_sessions(Guesses)
session_ids_with_no_guesses <- setdiff(session_ids, session_ids_in_guesses)

players_with_sessions_with_no_guesses <- Players %>%
  dplyr::filter(SessionID %in% session_ids_with_no_guesses) %>%
  select(PlayerID, SessionID, TeamID) %>%
  unique()
# dput(players_with_sessions_with_no_guesses$SessionID)

# Drop players without guesses I already knew about
teams_with_no_guesses <- c("G138", "G153", "G75")
players_with_sessions_with_no_guesses <- players_with_sessions_with_no_guesses %>%
  dplyr::filter(!(TeamID %in% teams_with_no_guesses))
# None of these players are recorded in the SubjInfo sheet,
# indicating they were false starts or other RA errors.

session_ids_with_unknown_guesses <- setdiff(session_ids_in_guesses, session_ids)
# dput(session_ids_with_unknown_guesses)

GuessesMap <- create_guesses_map(Guesses)
InventoryMap <- create_inventory_map(Guesses)

# Select final Guess columns excluding list columns for Guesses and Results
Guesses <- Guesses %>%
  select(
    Guess, Result,
    PlayerID, PlayerIX, SessionID, SessionIX, TeamID,
    Strategy, NumPlayers, Generation, Exp,
    SessionTime, PlayerTime, TeamTime, CalendarTime,
    NumSessionGuess, NumPlayerGuess, NumTeamGuess,
    UniqueSessionGuess, UniquePlayerGuess, UniqueTeamGuess,
    NumUniqueSessionGuesses, NumUniquePlayerGuesses, NumUniqueTeamGuesses,
    UniqueSessionResult, UniquePlayerResult, UniqueTeamResult,
    SessionScore, PlayerScore, TeamScore,
    PrevSessionGuessesHash, NumUniqueSessionGuesses, PrevSessionInventoryID, SessionInventorySize,
    PrevPlayerGuessesHash, NumUniquePlayerGuesses, PrevPlayerInventoryID, PlayerInventorySize,
    PrevTeamGuessesHash, NumUniqueTeamGuesses, PrevTeamInventoryID, TeamInventorySize,
    TeamStatus, SessionStatus
  )

devtools::use_data(Guesses, GuessesMap, InventoryMap, overwrite = TRUE)
