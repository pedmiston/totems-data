#' Accumulate items over trials, i.e. Guesses and Results.
#'
#' Accumulated items are lagged by one trial so that the resulting
#' item list represents the items that had been discovered up to
#' that trial.
#'
#' @param items The vector of items to accumulate in order.
#' @param default The default items. Optional. Defaults to NA.
#' @param ignore Item to ignore during accumulation. Optional.
#'
#' @examples
#' items <- c("a", "b", "b", "c")
#'
#' accumulate(items)
#' # list(NA, c("a"), c("a", "b"), c("a", "b"))
#'
#' accumulate(items, default = "a")
#' # list(c("a"), c("a"), c("a", "b"), c("a", "b"))
#'
#' accumulate(items, ignore = "b")
#' # list(c("a"), c("a"), c("a"), c("a"))
#'
#' @import magrittr
#' @export
accumulate <- function(items, default = NA, ignore = NA) {
  results <- list()
  if (length(default) > 1) default <- sort(default)
  for(i in seq_along(items)) {
    prev_ix <- i - 1
    if (i == 1) results[[i]] <- default
    else if (!is.na(ignore) & items[prev_ix] == ignore) {
      results[[i]] <- results[[prev_ix]]
    } else {
      results[[i]] <- append(results[[prev_ix]], items[prev_ix])
    }
  }
  results
}

#' Append a new item to the previous items.
append <- function(prev, new) {
  if(is.na(new)) return(prev)
  unique(c(prev, new)) %>% sort()
}

#' Assign a string id representing the vector of accumulated variables.
assign_ids <- function(accumulated) {
  purrr::map(accumulated, function(x) paste(sort(x), collapse = "-")) %>%
    unlist()
}

#' Assign a hash representing the vector of accumulated variables.
assign_hashes <- function(accumulated) {
  accumulated %>%
    assign_ids() %>%
    purrr::map(function(id) digest::digest(id)) %>%
    unlist()
}

#' Accumulate guesses and inventory items by session.
accumulate_session <- function(Guesses) {
  Guesses %>%
    arrange(SessionTime) %>%
    group_by(Exp, SessionID) %>%
    mutate(
      NumSessionGuess = 1:n(),
      PrevSessionGuesses = accumulate(Guess),
      PrevSessionGuessesHash = assign_hashes(PrevSessionGuesses),
      PrevSessionInventory = accumulate(Result, default = 1:6, ignore = 0),
      PrevSessionInventoryID = assign_ids(PrevSessionInventory)
    ) %>%
    ungroup()
}

#' Accumulate guesses and inventory items by player.
accumulate_player <- function(Guesses) {
  Guesses %>%
    arrange(PlayerTime) %>%
    group_by(Exp, PlayerID) %>%
    mutate(
      NumPlayerGuess = 1:n(),
      PrevPlayerGuesses = accumulate(Guess),
      PrevPlayerGuessesHash = assign_hashes(PrevPlayerGuesses),
      PrevPlayerInventory = accumulate(Result, default = 1:6, ignore = 0),
      PrevPlayerInventoryID = assign_ids(PrevPlayerInventory)
    ) %>%
    ungroup()
}

#' Accumulate guesses and inventory items by team.
accumulate_team <- function(Guesses) {
  Guesses %>%
    arrange(TeamTime) %>%
    group_by(Exp, TeamID) %>%
    mutate(
      NumTeamGuess = 1:n(),
      PrevTeamGuesses = accumulate(Guess),
      PrevTeamGuessesHash = assign_hashes(PrevTeamGuesses),
      PrevTeamInventory = accumulate(Result, default = 1:6, ignore = 0),
      PrevTeamInventoryID = assign_ids(PrevTeamInventory)
    ) %>%
    ungroup()
}
