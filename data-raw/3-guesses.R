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
  label_score()

GuessesMap <- create_guesses_map(Guesses)
InventoryMap <- create_inventory_map(Guesses)

# Select final Guess columns *without* list columns
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
    PrevSessionGuessesHash, PrevSessionInventoryID,
    PrevPlayerGuessesHash, PrevPlayerInventoryID,
    PrevTeamGuessesHash, PrevTeamInventoryID
  )

devtools::use_data(Guesses, GuessesMap, InventoryMap, overwrite = TRUE)
