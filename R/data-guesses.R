#' Get Guesses data.
data_guesses <- function() {
  Guesses <- collect_tbl("Table_Workshop") %>%
    rename(Guess = WorkShopString, Result = WorkShopResult) %>%

    # Prereqs:
    replace_id_player() %>%  # +PlayerID,SessionIX
    label_generation() %>%   # +Generation
    label_team_id() %>%      # +TeamID
    label_experiment() %>%   # +Exp
    label_team_size() %>%    # +TeamSize

    # Calculate kinds of time
    replace_trial_time() %>%
    calculate_team_time() %>%

    # Enumerate guesses by player and team
    count_player_guesses() %>%
    count_team_guesses() %>%

    # Label guesses as unique and count cumulate unique guesses
    label_unique_player_guesses() %>%
    label_unique_team_guesses() %>%

    # Label items as unique and count cumulate unique items
    label_unique_player_items() %>%
    label_unique_team_items() %>%

    # Merge score
    merge_player_score() %>%
    merge_team_score() %>%

    # Determine the number of current players
    label_current_players() %>%

    select(
      Exp, PlayerID, SessionIX, TeamID, Strategy, Generation,
      PlayerTime, TeamTime,
      GuessNum, TeamGuessNum,
      Guess, Result,
      PlayerScore, TeamScore,
      UniquePlayerGuess, NumUniqueGuesses,
      UniquePlayerItem, PlayerInventorySize, PlayerInventoryID,
      UniqueTeamGuess, NumUniqueTeamGuesses,
      UniqueTeamItem, TeamInventorySize, TeamInventoryID,
      NumCurrentPlayers, TeamSize
    )
  Guesses
}

#' Replace TrialTime (msec) with PlayerTime (sec)
replace_trial_time <- function(frame) {
  frame %>%
    mutate(PlayerTime = TrialTime/1000) %>%
    select(-TrialTime)
}

#' Calculate TeamTime.
calculate_team_time <- function(frame) {
  session_duration_sec <- 25 * 60
  frame %>%
    mutate(TeamTime = ifelse(Strategy == "Synchronic", PlayerTime * TeamSize,
                             (Generation-1) * session_duration_sec + PlayerTime))
}

#' Enumerate each player's guesses.
count_player_guesses <- function(frame) {
  count_guesses(frame, "PlayerID", "GuessNum")
}

#' Enumerate each team's guesses.
count_team_guesses <- function(frame) {
  count_guesses(frame, "TeamID", "TeamGuessNum")
}

#' Enumerate guesses by a grouping variable.
count_guesses <- function(frame, grouping_var, guess_col_name) {
  mutate_call <- lazyeval::interp(~ 1:n())
  frame %>%
    group_by_(.dots = grouping_var) %>%
    arrange(TeamTime) %>%
    mutate_(.dots = setNames(list(mutate_call), guess_col_name)) %>%
    ungroup()
}

#' Score guesses that resulted in unique items
merge_player_score <- function(frame) {
  scores <- read_scores() %>%
    select(Guess, Result, PlayerScore = Score) %>%
    mutate(UniqueItem = TRUE)
  left_join(frame, scores) %>%
    tidyr::replace_na(list(PlayerScore = 0))
}

#' Score guesses that resulted in unique team items
merge_team_score <- function(frame) {
  scores <- read_scores() %>%
    select(Guess, Result, TeamScore = Score) %>%
    mutate(UniqueTeamItem = TRUE)
  left_join(frame, scores) %>%
    tidyr::replace_na(list(TeamScore = 0))
}

#' Read the scores data
read_scores <- function() readr::read_csv("data-raw/scores.csv")

#' Label the number of current players
label_current_players <- function(frame) {
  frame %>% mutate(
    NumCurrentPlayers = ifelse(Strategy == "Diachronic", 1, TeamSize)
  )
}

#' Keep track of accumulating variables.
#'
#' @examples
#' items <- c("a", "b", "b", "c")
#' rolling(items)
#'
#' @import magrittr
#' @export
rolling <- function(items, default = NA) {
  results <- list()
  if (length(default) > 1) default <- sort(default)
  for(i in seq_along(items)) {
    if (i == 1) results[[i]] <- default
    else {
      results[[i]] <- c(results[[i-1]], items[i-1]) %>%
        .[!is.na(.)] %>%
        unique() %>%
        sort()
    }
  }
  results
}

#' Create PrevGuesses list column containing
#' unique guesses made by this player.
rolling_player_guesses <- function(Guesses) {
  Guesses %>%
    group_by(PlayerID) %>%
    arrange(GuessNum) %>%
    mutate(PrevGuesses = rolling(Guess)) %>%
    ungroup() %>%
    arrange(PlayerID, GuessNum)
}

#' Create PrevTeamGuesses list column containing
#' unique guesses made by this team.
rolling_team_guesses <- function(Guesses) {
  Guesses %>%
    group_by(TeamID) %>%
    arrange(TeamGuessNum) %>%
    mutate(PrevTeamGuesses = rolling(Guess)) %>%
    ungroup() %>%
    arrange(TeamID, TeamGuessNum)
}

#' Create PrevInventory list column containing
#' unique inventories held by this player.
rolling_player_inventory <- function(Guesses) {
  Guesses %>%
    group_by(PlayerID) %>%
    arrange(GuessNum) %>%
    mutate(PrevInventory = rolling(Result, default = 1:6)) %>%
    ungroup() %>%
    arrange(PlayerID, GuessNum)
}

#' Create PrevTeamInventory list column containing
#' unique inventories held by this team.
rolling_team_inventory <- function(Guesses) {
  Guesses %>%
    group_by(TeamID) %>%
    arrange(TeamGuessNum) %>%
    mutate(PrevTeamInventory = rolling(Result, default = 1:6)) %>%
    ungroup() %>%
    arrange(TeamID, TeamGuessNum)
}

#' Assess the uniqueness of each Guess for this player.
label_unique_guesses <- function(frame) {
  frame %>%
    rolling_player_guesses() %>%
    rowwise() %>%
    mutate(
      UniqueGuess = !(Guess %in% PrevGuesses),
      NumUniqueGuesses = length(PrevGuesses)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each Guess for this team.
label_unique_team_guesses <- function(frame) {
  frame %>%
    rolling_team_guesses() %>%
    rowwise() %>%
    mutate(
      UniqueTeamGuess = !(Guess %in% PrevTeamGuesses),
      NumUniqueTeamGuesses = length(PrevTeamGuesses)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this player.
label_unique_items <- function(Guesses) {
  Guesses %>%
    rolling_player_inventory() %>%
    rowwise() %>%
    mutate(
      UniqueItem = !(Result %in% PrevInventory),
      InventorySize = length(PrevInventory),
      PlayerInventoryID = inventory_to_id(PrevInventory)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this team.
label_unique_team_items <- function(frame) {
  frame %>%
    rolling_team_inventory() %>%
    rowwise() %>%
    mutate(
      UniqueTeamItem = !(Result %in% PrevTeamInventory),
      TeamInventorySize = length(PrevTeamInventory),
      TeamInventoryID = inventory_to_id(PrevTeamInventory)
    ) %>%
    ungroup()
}

inventory_to_id <- function(inventory) {
    paste(inventory, collapse = "-")
}
