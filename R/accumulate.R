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

#' Create PrevGuesses list column containing
#' unique guesses made by this player.
accumulate_player_guesses <- function(Guesses) {
  Guesses %>%
    group_by(PlayerID) %>%
    arrange(PlayerGuessNum) %>%
    mutate(PrevGuesses = accumulate(Guess)) %>%
    ungroup() %>%
    arrange(PlayerID, PlayerGuessNum)
}

#' Create PrevTeamGuesses list column containing
#' unique guesses made by this team.
accumulate_team_guesses <- function(Guesses) {
  Guesses %>%
    group_by(TeamID) %>%
    arrange(TeamGuessNum) %>%
    mutate(PrevTeamGuesses = accumulate(Guess)) %>%
    ungroup() %>%
    arrange(TeamID, TeamGuessNum)
}

#' Create PrevInventory list column containing
#' unique inventories held by this player.
accumulate_player_inventory <- function(Guesses) {
  Guesses %>%
    group_by(PlayerID) %>%
    arrange(PlayerGuessNum) %>%
    mutate(PrevInventory = accumulate(Result, default = 1:6)) %>%
    ungroup() %>%
    arrange(PlayerID, PlayerGuessNum)
}

#' Create PrevTeamInventory list column containing
#' unique inventories held by this team.
accumulate_team_inventory <- function(Guesses) {
  Guesses %>%
    group_by(TeamID) %>%
    arrange(TeamGuessNum) %>%
    mutate(PrevTeamInventory = accumulate(Result, default = 1:6)) %>%
    ungroup() %>%
    arrange(TeamID, TeamGuessNum)
}
