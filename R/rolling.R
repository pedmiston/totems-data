
rolling <- function(items, default = NA) {
  results <- list()
  for(i in seq_along(items)) {
    if (i == 1) results[[i]] <- default
    else if (i == 2) results[[i]] <- items[i-1]
    else results[[i]] <- sort(unique(c(results[[i-1]], items[i-1])))
  }
  results
}

rolling_player_guesses <- function(Guesses) {
  Guesses %>%
    arrange(PlayerID, GuessNum) %>%
    group_by(PlayerID) %>%
    mutate(PrevGuesses = rolling(Guess)) %>%
    ungroup()
}

label_unique_guesses <- function(frame) {
  isin <- function(guess, all_guesses) guess %in% all_guesses
  frame %>%
    rolling_player_guesses() %>%
    rowwise() %>%
    mutate(UniqueGuess = !(Guess %in% PrevGuesses))
}

rolling_team_guesses <- function(Guesses) {
  Guesses %>%
    arrange(TeamID, TeamGuessNum) %>%
    group_by(TeamID) %>%
    mutate(AllTeamGuesses = rolling(Guess)) %>%
    ungroup()
}
