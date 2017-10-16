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

#' Accumulate Guesses and Results by some grouping variable.
accumulate_by <- function(guesses, group) {
  time_var <- paste0(group, "Time")
  id_var <- paste0(group, "ID")
  num_var <- paste0("Num", group, "Guess")
  guesses_var <- paste0("Prev", group, "Guesses")
  guesses_hash_var <- paste0("Prev", group, "GuessesHash")
  inventory_var <- paste0("Prev", group, "Inventory")
  inventory_id_var <- paste0("Prev", group, "InventoryID")

  guesses %>%
    arrange(!!time_var) %>%
    group_by(!!id_var) %>%
    mutate(
      !!num_var := 1:n(),
      !!guesses_var := accumulate(Guess),
      !!guesses_hash_var := assign_hashes(!!guesses_var),
      !!inventory_var := accumulate(Result, default = 1:6),
      !!inventory_id_var := assign_ids(!!inventory_var)
    ) %>%
    ungroup()
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
      NumPlayerGuesss = 1:n(),
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
