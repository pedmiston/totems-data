#' Count guesses for each player and each team.
label_guess_num <- function(frame) {
  frame %>%
    count_session_guesses() %>%
    count_player_guesses() %>%
    count_team_guesses()
}

count_session_guesses <- function(frame) {
  frame %>%
    group_by(SessionID) %>%
    arrange(SessionTime) %>%
    mutate(NumSessionGuesses = 1:n())
}

#' Enumerate each player's guesses.
count_player_guesses <- function(frame) {
  frame %>%
    group_by(PlayerID) %>%
    arrange(PlayerTime) %>%
    mutate(NumPlayerGuesses = 1:n())
}

#' Enumerate each team's guesses.
count_team_guesses <- function(frame) {
  frame %>%
    group_by(TeamID) %>%
    arrange(TeamTime) %>%
    mutate(NumTeamGuesses = 1:n())
}
