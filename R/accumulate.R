#' Accumulate items over trials, i.e. Guesses and Results.
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
      prev_ix <- i - 1
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

accumulate_session <- function(Guesses) {
  Guesses %>%
    arrange(SessionTime) %>%
    group_by(SessionID) %>%
    mutate(
      NumSessionGuess = 1:n(),
      PrevSessionGuesses = accumulate(Guess),
      PrevSessionGuessesHash = assign_hashes(PrevSessionGuesses),
      PrevSessionInventory = accumulate(Result, default = 1:6),
      PrevSessionInventoryID = assign_ids(PrevSessionInventory)
    ) %>%
    ungroup()
}

accumulate_player <- function(Guesses) {
  Guesses %>%
    group_by(PlayerID) %>%
    arrange(PlayerTime) %>%
    mutate(
      NumPlayerGuess = 1:n(),
      PrevPlayerGuesses = accumulate(Guess),
      PrevPlayerGuessesHash = assign_hashes(PrevPlayerGuesses),
      PrevPlayerInventory = accumulate(Result, default = 1:6),
      PrevPlayerInventoryID = assign_ids(PrevPlayerInventory)
    ) %>%
    ungroup()
}

accumulate_team <- function(Guesses) {
  Guesses %>%
    group_by(TeamID) %>%
    arrange(TeamTime) %>%
    mutate(
      NumTeamGuess = 1:n(),
      PrevTeamGuesses = accumulate(Guess),
      PrevTeamGuessesHash = assign_hashes(PrevTeamGuesses),
      PrevTeamInventory = accumulate(Result, default = 1:6),
      PrevTeamInventoryID = assign_ids(PrevTeamInventory)
    ) %>%
    ungroup()
}
