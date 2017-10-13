#' Keep track of accumulating variables.
#'
#' @examples
#' items <- c("a", "b", "b", "c")
#' rolling(items)
#' # list(NA, c("a"), c("a", "b"), c("a", "b"))
#'
#' @import magrittr
#' @export
accumulator <- function(items, default = NA) {
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

accumulate <- function(Guesses) {
  Guesses %>%
    accumulate_session() %>%
    accumulate_player() %>%
    accumulate_team()
}

accumulate_session <- function(Guesses) {
  Guesses %>%
    group_by(SessionID) %>%
    arrange(SessionGuessNum) %>%
    mutate(
      PrevSessionGuesses = accumulator(Guess),
      PrevSessionResults = accumulator(Result, default = 1:6)
    ) %>%
    ungroup()
}

accumulate_player <- function(Guesses) {
  Guesses %>%
    group_by(PlayerID) %>%
    arrange(PlayerGuessNum) %>%
    mutate(
      PrevPlayerGuesses = accumulator(Guess),
      PrevPlayerResults = accumulator(Result, default = 1:6)
    ) %>%
    ungroup()
}

accumulate_team <- function(Guesses) {
  Guesses %>%
    group_by(TeamID) %>%
    arrange(TeamGuessNum) %>%
    mutate(
      PrevTeamGuesses = accumulator(Guess),
      PrevTeamResults = accumulator(Result, default = 1:6)
    ) %>%
    ungroup()
}
