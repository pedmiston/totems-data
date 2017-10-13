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

#' Label the number of current players
label_current_players <- function(frame) {
  frame %>% mutate(
    NumCurrentPlayers = ifelse(Strategy == "Diachronic", 1, TeamSize)
  )
}
