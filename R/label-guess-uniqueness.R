#' Label guesses as unique and count cumulate unique guesses
label_guess_uniqueness <- function(frame) {
  frame %>%
    label_unique_session_guesses() %>%
    label_unique_player_guesses() %>%
    label_unique_team_guesses()
}

#' Assess the uniqueness of each guess for this session.
label_unique_session_guesses <- function(frame) {
  frame %>%
    rowwise() %>%
    mutate(
      UniqueSessionGuess = !(Guess %in% PrevSessionGuesses),
      NumUniqueSessionGuesses = length(PrevSessionGuesses)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each guess for this player.
label_unique_player_guesses <- function(frame) {
  frame %>%
    rowwise() %>%
    mutate(
      UniquePlayerGuess = !(Guess %in% PrevPlayerGuesses),
      NumUniquePlayerGuesses = length(PrevPlayerGuesses)
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each guess for this team.
label_unique_team_guesses <- function(frame) {
  frame %>%
    rowwise() %>%
    mutate(
      UniqueTeamGuess = !(Guess %in% PrevTeamGuesses),
      NumUniqueTeamGuesses = length(PrevTeamGuesses)
    ) %>%
    ungroup()
}
