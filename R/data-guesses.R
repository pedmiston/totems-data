#' Get data for each guess made by players.
#' @param con DBI Connection class.
#' @import dplyr
#' @export
data_guesses <- function() {
  con <- connect_db()
  Guesses <- collect(tbl(con, "Table_Workshop")) %>%
    replace_id_player() %>%
    join_players() %>%
    join_teams() %>%
    replace_trial_time() %>%
    calculate_team_time() %>%
    count_player_guesses() %>%
    count_team_guesses() %>%
    calculate_score() %>%
    # rolling_player_guesses() %>%
    determine_unique_guess() %>%
    label_unique_guesses() %>%
    label_team_unique_guesses() %>%
    label_unique_item() %>%
    label_team_unique_item() %>%
    select(
      PlayerID, TeamID, Strategy, Generation,
      PlayerTime, TeamTime,
      PlayerGuessNum, TeamGuessNum,
      Guess = WorkShopString, Result = WorkShopResult,
      Score, TeamScore,
      UniqueGuess, TeamUniqueGuess,
      UniqueItem, TeamUniqueItem
    )
  RMySQL::dbDisconnect(con)
  Guesses
}

#' Replace TrialTime (msec) with PlayerTime (sec)
replace_trial_time <- function(frame) {
  frame %>%
    mutate(PlayerTime = TrialTime/1000) %>%
    select(-TrialTime)
}

#' Calculate TeamTime for Diachronic teams.
calculate_team_time <- function(frame) {
  session_duration_sec <- 25 * 60
  frame %>%
    mutate(TeamTime = ifelse(Strategy == "Diachronic",
                             (Generation-1) * session_duration_sec + PlayerTime,
                             PlayerTime))
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

calculate_score <- function(frame) {
  frame %>%
    mutate(
      Score = NA,
      TeamScore = NA
    )
}

label_team_unique_guesses <- function(frame) {
  frame
}

label_unique_items <- function(frame) {
  frame %>%
    mutate(
      UniqueItem = NA,
      TeamUniqueItem = NA
    )
}

label_team_unique_items <- function(frame) {
  frame
}
