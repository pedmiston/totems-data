library(devtools)
load_all()
collect_tables()

Teams <- read_table("Table_Group") %>%
  rename(Strategy = Treatment, SessionDuration = BuildingTime) %>%
  replace_id_group() %>%
  label_players_per_session() %>%
  label_team_conditions() %>%
  select(
    Exp,
    TeamID,
    Strategy,
    NumPlayers,
    SessionsPerPlayer,
    PlayersPerSession,
    SessionDuration
  ) %>%
  arrange(TeamID, desc(Exp))

Players <- read_table("Table_Player") %>%
  replace_id_group() %>%
  label_strategy() %>%
  label_players_per_session() %>%
  replace_id_player() %>%
  replace_ancestor() %>%
  label_player_conditions() %>%
  select(
    Exp,
    PlayerID,
    PlayerIX,
    SessionID,
    SessionIX,
    TeamID,
    Strategy,
    NumPlayers,
    Generation,
    SessionDuration
  ) %>%
  arrange(TeamID, desc(Exp), PlayerIX, SessionIX, Strategy)

Guesses <- read_table("Table_Workshop") %>%
  rename(Guess = WorkShopString, Result = WorkShopResult) %>%
  replace_id_player() %>%
  replace_trial_time() %>%
  label_team_id() %>%
  label_strategy() %>%
  label_generation() %>%
  label_team_id() %>%
  label_players_per_session() %>%
  label_player_conditions() %>%
  label_time() %>%
  accumulate_session() %>%
  accumulate_player() %>%
  accumulate_team() %>%
  label_guess_uniqueness() %>%
  label_result_uniqueness() %>%
  label_score() %>%
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
    PrevSessionGuesses, PrevSessionGuessesHash, PrevSessionInventory, PrevSessionInventoryID,
    PrevPlayerGuesses, PrevPlayerGuessesHash, PrevPlayerInventory, PrevPlayerInventoryID,
    PrevTeamGuesses, PrevTeamGuessesHash, PrevTeamInventory, PrevTeamInventoryID
  )

GuessesMap <- Guesses %>%
  select(contains("Guesses")) %>%
  create_guesses_map()

InventoryMap <- Guesses %>%
  select(contains("Inventory")) %>%
  create_inventory_map()

Inventories <- Guesses %>%
  group_by(SessionID, SessionInventoryID) %>%
  summarize(
    NumGuesses = n(),
    UniqueGuesses = sum(UniqueSessionGuess),
    RepeatGuesses = NumGuesses - UniqueGuesses,
    RepeatResults = sum(Result != 0),
    StartTime = min(SessionTime),
    EndTime = max(SessionTime),
    SessionDuration = EndTime - StartTime,
    Result = ifelse(sum(UniqueSessionResult) == 0, 0,
                    Result[UniqueSessionResult == 1])
  ) %>%
  rename(InventoryID = SessionInventoryID) %>%
  join_players()

devtools::use_data(Teams, Players, overwrite = TRUE)
