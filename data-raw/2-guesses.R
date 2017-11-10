# devtools::load_all()

Guesses <- read_table("Table_Workshop") %>%
  rename(Guess = WorkShopString, Result = WorkShopResult) %>%
  mutate(CreatedItem = Result != 0) %>%
  replace_id_player() %>%
  replace_trial_time() %>%
  fix_bug_in_session_time() %>%
  label_team_id() %>%
  label_strategy() %>%
  label_generation() %>%
  label_players_per_session() %>%
  label_session_duration() %>%
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

GuessesMap <- create_guesses_map(Guesses)
InventoryMap <- create_inventory_map(Guesses)

# Select final Guess columns excluding list columns for Guesses and Results
Guesses <- Guesses %>%
  select(
    Guess, Result, CreatedItem,
    PlayerID, PlayerIX, SessionID, SessionIX, TeamID,
    Strategy, NumPlayers, SessionDuration, Generation, Exp,
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

# Run python script to identify innovations adjacent to each inventory.
write.csv(InventoryMap[, "ID"], "data-raw/adjacent/inventory-ids.csv", row.names = FALSE)
system("bin/adjacent.py data-raw/adjacent/inventory-ids.csv")

NumAdjacent <- readr::read_csv("data-raw/adjacent/num-adjacent.csv")
InventoryMap <- left_join(InventoryMap, NumAdjacent)

AdjacentItems <- readr::read_csv("data-raw/adjacent/adjacent-items.csv")

devtools::use_data(Guesses, GuessesMap, InventoryMap, Sampled, AdjacentItems, overwrite = TRUE)
