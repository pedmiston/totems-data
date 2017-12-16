# devtools::load_all()

# Guesses, GuessesMap, and InventoryMap ----
Guesses <- read_table("Table_Workshop") %>%
  rename(Guess = WorkShopString, Result = WorkShopResult) %>%
  mutate(CreatedItem = (Result != 0)) %>%
  replace_id_player() %>%
  replace_trial_time() %>%
  label_team_id() %>%
  label_strategy() %>%
  label_generation() %>%
  label_players_per_session() %>%
  label_session_duration() %>%
  label_time() %>%
  accumulate_session() %>%
  accumulate_team() %>%
  label_guess_uniqueness() %>%
  label_result_uniqueness() %>%
  label_score() %>%
  label_stage()

GuessesMap <- create_guesses_map(Guesses)
InventoryMap <- create_inventory_map(Guesses)

# Select final Guess columns excluding list columns for Guesses and Results
Guesses <- Guesses %>%
  select(
    Guess, Result, CreatedItem,
    PlayerID, PlayerIX, SessionID, SessionIX, TeamID,
    Strategy, PlayersPerSession, SessionDuration, Generation,
    SessionTime, PlayerTime, TeamTime, CalendarTime, GuessTime,
    NumSessionGuess, NumTeamGuess,
    UniqueSessionGuess, UniqueTeamGuess,
    UniqueSessionResult, UniqueTeamResult,
    SessionScore, TeamScore,
    PrevSessionGuessesHash, NumUniqueSessionGuesses, PrevSessionInventoryID, SessionInventorySize,
    PrevTeamGuessesHash, NumUniqueTeamGuesses, PrevTeamInventoryID, TeamInventorySize,
    Stage
  )

devtools::use_data(
  Guesses,
  # GuessesMap,  # Hasn't been needed yet, and it's big.
  InventoryMap,
  overwrite = TRUE
)

# Sampled ----
starting_inventory <- data_frame(
  NumGuesses = 0,
  InventoryID = "1-2-3-4-5-6",
  InventorySize = 6,
  GuessesHash = digest::digest(""),
  NumUniqueGuesses = 0
)

Sampled <- Guesses %>%
  arrange(SessionTime) %>%
  group_by(SessionID) %>%
  do({ sample_session(., default = starting_inventory, interval = 1) }) %>%
  ungroup() %>%
  label_session_player() %>%
  label_team_id() %>%
  label_strategy() %>%
  label_session_duration() %>%
  label_players_per_session() %>%
  label_generation() %>%
  label_time()

devtools::use_data(Sampled, overwrite = TRUE)

# InventoryInfo, AdjacentItems, InventoriesPerItem, Combinatorics ----

# Run python script to identify innovations adjacent to each inventory.
write.csv(InventoryMap[, "ID"], "data-raw/adjacent/inventory-ids.csv", row.names = FALSE)
system("bin/adjacent.py data-raw/adjacent/inventory-ids.csv")
# creates num-adjacent.csv and adjacent-items.csv
# requires totems venv to be active!

NumAdjacent <- readr::read_csv("data-raw/adjacent/num-adjacent.csv")
AdjacentItems <- readr::read_csv("data-raw/adjacent/adjacent-items.csv")

InventoryInfo <- left_join(InventoryMap, NumAdjacent) %>%
  mutate(InventorySize = purrr::map_int(Inventory, length)) %>%
  # Drop the list column
  select(-Inventory)

Combinatorics <- data_frame(InventorySize = unique(InventoryInfo$InventorySize)) %>%
  mutate(
    UniqueGuesses = purrr::map_dbl(InventorySize, count_unique_guesses),
    UniqueCombinations = purrr::map_dbl(InventorySize, count_unique_combinations)
  )

z_score <- function(x) (x - mean(x))/sd(x)

InventoryInfo <- left_join(InventoryInfo, Combinatorics) %>%
  mutate(
    GuessDifficulty = UniqueGuesses/NumAdjacent,
    CombinationDifficulty = UniqueCombinations/NumAdjacent
  )

InventoriesPerItem <- AdjacentItems %>%
  group_by(Adjacent) %>%
  summarize(NumInventories = n())

devtools::use_data(InventoryInfo, AdjacentItems, InventoriesPerItem, Combinatorics, overwrite = TRUE)
