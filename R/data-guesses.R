#' Get Guesses data.
#'
#' Guesses are the primary unit of observation.
#' A Guess is a single trial of the experiment:
#' a combination of items selected from the
#' current Inventory which may or may not yield
#' a result.
#'
#' The guess and the result can both be unique
#' or repeated, relative to the current session,
#' player, and team.
data_guesses <- function() {
  Guesses <- collect_tbl("Table_Workshop") %>%
    rename(Guess = WorkShopString, Result = WorkShopResult) %>%
    replace_id_player() %>%
    replace_trial_time() %>%
    # order matters!
    label_generation() %>%
    label_team_id() %>%
    label_experiment() %>%
    label_team_size() %>%
    label_current_players() %>%
    label_time() %>%
    label_guess_num() %>%
    label_guess_uniqueness() %>%
    label_item_uniqueness() %>%
    label_score() %>%
    select(
      Guess, Result,
      PlayerID, SessionIX, TeamID,
      Strategy, TeamSize, Duration, Generation, Exp,
      SessionTime, PlayerTime, TeamTime, CalendarTime,
      NumSessionGuesses, NumPlayerGuesses, NumTeamGuesses,
      UniqueSessionGuess, UniquePlayerGuess, UniqueTeamGuess,
      NumUniqueSessionGuesses, NumUniquePlayerGuesses, NumUniqueTeamGuesses,
      UniqueSessionItem, UniquePlayerItem, UniqueTeamItem,
      SessionInventoryID, PlayerInventoryID, TeamInventoryID,
      SessionScore, PlayerScore, TeamScore
    )
  Guesses
}
