#' Keep track of accumulating variables.
#'
#' @examples
#' items <- c("a", "b", "b", "c")
#' rolling(items)
#' # list(NA, c("a"), c("a", "b"), c("a", "b"))
#'
#' @import magrittr
#' @export
accumulate <- function(items, default = NA) {
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

accumulate_session <- function(Guesses) {
  Guesses %>%
    group_by(SessionID) %>%
    arrange(SessionTime) %>%
    mutate(
      NumSessionGuess = 1:n(),
      PrevSessionGuesses = accumulate(Guess),
      PrevSessionResults = accumulate(Result, default = 1:6)
    ) %>%
    ungroup()
}

accumulate_player <- function(Guesses) {
  Guesses %>%
    group_by(PlayerID) %>%
    arrange(PlayerTime) %>%
    mutate(
      NumPlayerGuessses = 1:n(),
      PrevPlayerGuesses = accumulate(Guess),
      PrevPlayerResults = accumulate(Result, default = 1:6)
    ) %>%
    ungroup()
}

accumulate_team <- function(Guesses) {
  Guesses %>%
    group_by(TeamID) %>%
    arrange(TeamTime) %>%
    mutate(
      NumTeamGuesses = 1:n(),
      PrevTeamGuesses = accumulate(Guess),
      PrevTeamResults = accumulate(Result, default = 1:6)
    ) %>%
    ungroup()
}
