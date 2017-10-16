#' Get Teams data.
data_teams <- function() {
  Teams <- read_table("Table_Group") %>%
    rename(Strategy = Treatment, Duration = BuildingTime) %>%
    replace_id_group() %>%
    label_team_experiment() %>%
    label_valid_teams() %>%
    select(
      Exp,
      TeamID,
      Strategy,
      TeamSize,
      SessionSize,
      Duration,
      IsTeamValid
    )
  Teams
}

#' Teams data.
"Teams"

#' Get Player data.
data_players <- function() {
  teams <- read_table("Table_Group") %>%
    replace_id_group() %>%
    select(TeamID, Strategy = Treatment, Duration = BuildingTime)

  players <- read_table("Table_Player") %>%
    replace_id_group() %>%
    left_join(teams) %>%
    replace_id_player() %>%
    replace_ancestor() %>%
    label_player_experiment() %>%
    label_valid_players() %>%
    select(
      Exp,
      PlayerID,
      TeamID,
      TeamSize,
      Strategy,
      SessionID,
      SessionIX,
      Generation,
      Duration
    )
   players %>% arrange(desc(Exp), TeamID, Generation, Session)
}




#' Get Guesses data.
#'
#' Guesses are the primary unit of observation.
#' A Guess is a single trial of the experiment:
#' a combination of items selected from the
#' current Inventory which may or may not yield
#' a resulting Item.
#'
#' The guess and the result can both be unique
#' or repeated, relative to the current session,
#' player, or team.
data_guesses <- function() {
  local_guesses <- "data/Guesses.rda"
  if (file.exists(local_guesses)) {
    load(local_guesses)
    return(Guesses)
  }

  Guesses <- read_table("Table_Workshop") %>%
    rename(Guess = WorkShopString, Result = WorkShopResult) %>%
    replace_id_player() %>%
    replace_trial_time() %>%
    label_team_id() %>%
    label_strategy() %>%
    label_generation() %>%
    label_team_id() %>%
    label_player_experiment() %>%
    label_current_players() %>%
    label_time() %>%
    # accumulate_by("Session") %>%
    # accumulate_by("Player") %>%
    # accumulate_by("Team") %>%
    accumulate_session() %>%
    accumulate_player() %>%
    accumulate_team() %>%
    label_guess_uniqueness() %>%
    label_result_uniqueness() %>%
    label_score() %>%
    select(
      Guess, Result,
      PlayerID, SessionID, SessionIX, TeamID,
      Strategy, TeamSize, Generation, Exp,
      SessionTime, PlayerTime, TeamTime, CalendarTime,
      NumSessionGuess, NumPlayerGuess, NumTeamGuess,
      UniqueSessionGuess, UniquePlayerGuess, UniqueTeamGuess,
      NumUniqueSessionGuesses, NumUniquePlayerGuesses, NumUniqueTeamGuesses,
      UniqueSessionResult, UniquePlayerResult, UniqueTeamResult,
      SessionInventoryID, PlayerInventoryID, TeamInventoryID,
      SessionScore, PlayerScore, TeamScore
    )
  Guesses
}

#' Get Inventories data.
#'
#' Each observation is a description of the guesses
#' made between discoveries (an inventory).
#'
#' Better performance is fewer incorrect guesses made
#' prior to a new discovery. Of the incorrect guesses,
#' unique ones are better than repeats.
data_inventories <- function() {
  Guesses <- data_guesses()
  Inventories <- Guesses %>%
    group_by(SessionID, SessionInventoryID) %>%
    summarize(
      NumGuesses = n(),
      UniqueGuesses = sum(UniqueSessionGuess),
      RepeatGuesses = NumGuesses - UniqueGuesses,
      RepeatResults = sum(Result != 0),
      StartTime = min(SessionTime),
      EndTime = max(SessionTime),
      Duration = EndTime - StartTime,
      Result = ifelse(sum(UniqueSessionResult) == 0, 0,
                      Result[UniqueSessionResult == 1])
    ) %>%
    rename(InventoryID = SessionInventoryID) %>%
    join_players()
}

data_team_inventories <- function() {
  Inventories <- data_inventories()
  TeamInventories <- Inventories %>%
    label_current_players() %>%
    group_by(TeamID, InventoryID) %>%
    summarize(
      TeamGuesses = sum(NumGuesses),
      TotalUniqueGuesses = sum(UniqueGuesses),
      UniqueGuessesPerPlayer = sum(UniqueGuesses)/NumCurrentPlayers,
      TotalRepeatResults = sum(RepeatResults),
      TotalTime = sum(Duration)
    ) %>%
    ungroup() %>%
    join_teams()
}

data_team_performance <- function() {
  Guesses <- data_guesses()
  TeamPerformance <- Guesses %>%
    group_by(Exp, TeamID) %>%
    summarize(
      TeamScore = sum(TeamScore),
      TeamGuesses = max(NumTeamGuesses),
      UniqueTeamGuesses = max(NumUniqueTeamGuesses),
      TeamInventorySize = max(TeamInventorySize)
    ) %>%
    join_teams()
  TeamPerformance
}

data_player_performance <- function() {
  Guesses <- data_guesses()
  PlayerPerformance <- Guesses %>%
    group_by(Exp, PlayerID) %>%
    summarize(
      Score = sum(Score),
      NumGuesses = max(GuessNum),
      UniqueGuesses = max(NumUniqueGuesses),
      UniquePlayerResults = max(NumUniquePlayerResults)
    ) %>%
    join_players() %>%
    join_teams()
}

data_sampled <- function() {
  Guesses <- data_guesses()
  SampledPerformance <- Guesses %>%
    sample_every(60)
}
