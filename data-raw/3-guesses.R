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

nin <- function(x, y) x[!(x %in% y)]

team_ids <- unique(Teams$TeamID)
team_ids_in_guesses <- unique(Guesses$TeamID)
setequal(team_ids, team_ids_in_guesses)
# FALSE. Should be TRUE
# Explanation: TeamIDs can be created and the experiment is never run,
#   e.g., the participant never showed up.
teams_with_no_guesses <- nin(team_ids, team_ids_in_guesses)
# Verify that teams without guesses are not included
# in the experiment conditions.
teams_expected_to_have_guesses <- Teams %>%
  dplyr::filter(TeamStatus == "V") %>%
  .$TeamID %>%
  unique()
teams_that_should_have_guesses <- intersect(teams_with_no_guesses, teams_expected_to_have_guesses)
# [1] "G138" "G153" "G75"
# Doh!
# See who these teams are.
teams_with_errors <- dplyr::filter(Teams, TeamID %in% teams_that_should_have_guesses)
players_on_teams_with_errors <- dplyr::filter(Players, TeamID %in% teams_that_should_have_guesses)

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
