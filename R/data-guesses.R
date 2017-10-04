#' Get data for each guess made by players.
#' @param con DBI Connection class.
#' @import dplyr
#' @export
data_guesses <- function(con) {
  players <- data_players(con)
  teams <- data_teams(con)
  Workshop <- tbl(con, "Table_Workshop") %>%
    collect() %>%
    rename_player_id(con) %>%
    left_join(players) %>%
    left_join(teams) %>%
    calculate_time() %>%
    count_guesses("PlayerID", "GuessNum") %>%
    count_guesses("TeamID", "TeamGuessNum") %>%
    calculate_score() %>%
    determine_unique_guess() %>%
    determine_unique_item() %>%
    select(
      PlayerID,
      PlayerTime, TeamTime,
      GuessNum, TeamGuessNum,
      Guess = WorkShopString, Result = WorkShopResult,
      Score, TeamScore,
      UniqueGuess, TeamUniqueGuess,
      UniqueItem, TeamUniqueItem
    )
}

calculate_time <- function(frame) {
  session_duration_sec <- 25 * 60
  # Convert milliseconds to seconds
  frame %>%
    mutate(
      PlayerTime = TrialTime/1000,
      TeamTime = ifelse(Strategy == "Diachronic",
                        (Generation-1) * session_duration_sec + PlayerTime,
                        PlayerTime)
    )
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

determine_unique_guess <- function(frame) {
  frame %>%
    mutate(
      UniqueGuess = NA,
      TeamUniqueGuess = NA
    )
}

determine_unique_item <- function(frame) {
  frame %>%
    mutate(
      UniqueItem = NA,
      TeamUniqueItem = NA
    )
}
