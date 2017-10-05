#' Get data for each guess made by players.
#' @param con DBI Connection class.
#' @import dplyr
#' @export
data_guesses <- function() {
  Guesses <- collect_tbl("Table_Workshop") %>%
    rename(Guess = WorkShopString, Result = WorkShopResult) %>%

    # Merge player and team info
    replace_id_player() %>%
    join_players() %>%
    join_teams() %>%

    # Calculate kinds of time
    replace_trial_time() %>%
    calculate_team_time() %>%

    # Enumerate guesses by player and team
    count_player_guesses() %>%
    count_team_guesses() %>%

    # Label guesses as unique
    label_unique_guesses() %>%
    label_unique_team_guesses() %>%

    # Label items as unique
    label_unique_items() %>%
    label_unique_team_items() %>%

    # Merge score
    merge_player_score() %>%
    merge_team_score() %>%

    select(
      PlayerID, TeamID, Strategy, Generation,
      PlayerTime, TeamTime,
      GuessNum, TeamGuessNum,
      Guess, Result,
      PlayerScore, TeamScore,
      UniqueGuess, UniqueTeamGuess,
      UniqueItem, UniqueTeamItem
    )
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
