#' Get Team Inventories data.
#'
#' Each row in the resulting data contains
#' a summary of the guesses made between discoveries.
#'
#' Better performance is fewer incorrect guesses made
#' prior to a new discovery.
#'
#' Performance can be measured further by the number
#' of unique guesses made, the idea being that an optimal
#' strategy would at minimum reduce the number of
#' guesses made.
data_team_inventories <- function() {
  Guesses <- data_guesses() %>%
    label_current_players()

  Inventories <- Guesses %>%
    group_by(TeamID, TeamInventoryID) %>%
    summarize(
      TeamGuesses = n(),
      UniqueTeamGuesses = sum(UniqueTeamGuess),
      TotalUniquePlayerGuesses = sum(UniquePlayerGuess),
      UniqueGuessesPerPlayer = sum(UniquePlayerGuess)/unique(NumCurrentPlayers)[[1]],
      StartTime = min(TeamTime),
      EndTime = max(TeamTime),
      Duration = max(TeamTime) - min(TeamTime),
      NewItem = ifelse(sum(UniqueTeamItem) == 0, 0, Result[UniqueTeamItem == 1]),
      DiscoveredBy = ifelse(sum(UniqueTeamItem) == 0, "", PlayerID[UniqueTeamItem == 1])
    ) %>%
    ungroup() %>%
    arrange(TeamID, TeamInventorySize) %>%
    join_teams()
}

#' Label the number of current players
label_current_players <- function(frame) {
  frame %>% mutate(
    NumCurrentPlayers = ifelse(Strategy == "Diachronic", 1, TeamSize)
  )
}
