#' Create Teams data.
pkg_data_teams <- function() {
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
  devtools::use_data(Teams, overwrite = TRUE)
}

#' Teams data.
"Teams"

#' Get Player data.
pkg_data_players <- function() {
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
  devtools::use_data(Players, overwrite = TRUE)
}

#' Guesses data.
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
pkg_data_guesses <- function() {
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
  devtools::use_data(Guesses, overwrite = TRUE)
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
  load("data/Guesses.rda")
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
      TotalTime = sum(SessionDuration)
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
