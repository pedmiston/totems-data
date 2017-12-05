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
    group_by(SessionID) %>%
    mutate(
      NumSessionGuess = 1:n(),
      PrevSessionGuesses = accumulate(Guess),
      PrevSessionGuessesHash = assign_hashes(PrevSessionGuesses),
      PrevSessionInventory = accumulate(Result, default = 1:6, ignore = 0),
      PrevSessionInventoryID = assign_ids(PrevSessionInventory)
    ) %>%
    ungroup()
}

#' Accumulate guesses and inventory items by team.
accumulate_team <- function(Guesses) {
  Guesses %>%
    arrange(TeamTime) %>%
    group_by(TeamID) %>%
    mutate(
      NumTeamGuess = 1:n(),
      PrevTeamGuesses = accumulate(Guess),
      PrevTeamGuessesHash = assign_hashes(PrevTeamGuesses),
      PrevTeamInventory = accumulate(Result, default = 1:6, ignore = 0),
      PrevTeamInventoryID = assign_ids(PrevTeamInventory)
    ) %>%
    ungroup()
}

#' Label guesses as unique.
label_guess_uniqueness <- function(frame) {
  frame %>%
    label_unique_session_guesses() %>%
    label_unique_team_guesses()
}

#' Assess the uniqueness of each guess for this session.
label_unique_session_guesses <- function(frame) {
  frame %>%
    group_by(PrevSessionGuessesHash) %>%
    mutate(
      UniqueSessionGuess = !(Guess %in% PrevSessionGuesses[[1]]),
      NumUniqueSessionGuesses = length(PrevSessionGuesses[[1]])
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each guess for this team.
label_unique_team_guesses <- function(frame) {
  frame %>%
    group_by(PrevTeamGuessesHash) %>%
    mutate(
      UniqueTeamGuess = !(Guess %in% PrevTeamGuesses[[1]]),
      NumUniqueTeamGuesses = length(PrevTeamGuesses[[1]])
    ) %>%
    ungroup()
}

#' Label results as unique and accumulate them.
label_result_uniqueness <- function(Guesses) {
  Guesses %>%
    label_unique_session_results() %>%
    label_unique_team_results()
}

#' Assess the uniqueness of results created in this session.
label_unique_session_results <- function(frame) {
  frame %>%
    group_by(PrevSessionInventoryID) %>%
    mutate(
      UniqueSessionResult = Result != 0 & !(Result %in% PrevSessionInventory[[1]]),
      SessionInventorySize = UniqueSessionResult + length(PrevSessionInventory[[1]])
    ) %>%
    ungroup()
}

#' Assess the uniqueness of each created Item for this team.
label_unique_team_results <- function(frame) {
  frame %>%
    group_by(PrevTeamInventoryID) %>%
    mutate(
      UniqueTeamResult = Result != 0 & !(Result %in% PrevTeamInventory[[1]]),
      TeamInventorySize = UniqueTeamResult + length(PrevTeamInventory[[1]])
    ) %>%
    ungroup()
}

#' Create a map of hashes to vectors of guesses.
create_guesses_map <- function(Guesses) {
  unique_guesses <- function(hash_col, guesses_col) {
    hash_col <- enquo(hash_col)
    guesses_col <- enquo(guesses_col)
    Guesses %>%
      select(Hash = !!hash_col, Guesses = !!guesses_col) %>%
      distinct(Hash, .keep_all = TRUE)
  }

  bind_rows(
    unique_guesses(PrevSessionGuessesHash, PrevSessionGuesses),
    unique_guesses(PrevTeamGuessesHash, PrevTeamGuesses)
  ) %>%
    distinct(Hash, .keep_all = TRUE)
}

#' Create a map of inventory ids to items.
create_inventory_map <- function(Guesses) {
  unique_inventories <- function(id_col, inventory_col) {
    id_col <- enquo(id_col)
    inventory_col <- enquo(inventory_col)
    Guesses %>%
      select(ID = !!id_col, Inventory = !!inventory_col) %>%
      distinct(ID, .keep_all = TRUE)
  }

  bind_rows(
    unique_inventories(PrevSessionInventoryID, PrevSessionInventory),
    unique_inventories(PrevTeamInventoryID, PrevTeamInventory)
  ) %>%
    distinct(ID, .keep_all = TRUE)
}

#' Sample guesses at regular intervals in a session.
#'
#' Requires SessionID, SessionDuration, SessionTime, NumSessionGuesses,
#' PrevSessionInventoryID, SessionInventorySize,
#' PrevSessionGuessesHash, NumUniqueSessionGuesses
sample_session <- function(session_guesses, default, interval = 1) {
  # Create samples as a map of sampled session times to indices in session guesses
  samples <- data_frame(
    SampledSessionTime = seq(0, session_guesses$SessionDuration[[1]], by = interval),
    SessionGuessesIndex = findInterval(SampledSessionTime, session_guesses$SessionTime)
  )

  # Subset only the valid samples with index > 0
  valid_samples <- dplyr::filter(samples, SessionGuessesIndex > 0)

  # Reindex session guesses based on sampled session guesses indices
  sampled_guesses <- session_guesses[valid_samples$SessionGuessesIndex, ]

  # Replace session time (for the guess) with sampled time
  sampled_guesses$SessionTime <- valid_samples$SampledSessionTime

  # Select only the columns relevant to this session
  sampled_guesses <- sampled_guesses %>%
    select(
      SessionID, SessionTime,
      NumGuesses = NumSessionGuess,
      InventoryID = PrevSessionInventoryID,       # INVALID in case where sampled guess result != 0
      InventorySize = SessionInventorySize,
      GuessesHash = PrevSessionGuessesHash,       # INVALID in case where sampled guess uniqueness != FALSE
      NumUniqueGuesses = NumUniqueSessionGuesses  # INVALID in case where sampled guess uniqueness != FALSE
    )

  # Identify missing samples
  missing_samples <- dplyr::filter(samples, SessionGuessesIndex == 0) %>%
    select(SessionTime = SampledSessionTime)

  if(nrow(missing_samples) > 0) {
    default$SessionID <- session_guesses$SessionID[[1]]
    missing <- merge(missing_samples, default)
    sampled_guesses <- bind_rows(missing, sampled_guesses)
  }

  sampled_guesses
}


read_scores <- function() {
  readr::read_csv("data-raw/game/scores.csv") %>% select(Guess, Result, Score)
}

label_score <- function(frame) {
  scores <- read_scores()
  frame %>%
    label_session_score(scores) %>%
    label_team_score(scores)
}

label_session_score <- function(frame, scores) {
  if(missing(scores)) scores <- read_scores()
  scores <- scores %>%
    rename(SessionScore = Score) %>%
    mutate(UniqueSessionResult = TRUE)
  left_join(frame, scores) %>%
    tidyr::replace_na(list(SessionScore = 0))
}

#' Score guesses that resulted in unique team items
label_team_score <- function(frame, scores) {
  if(missing(scores)) scores <- read_scores()
  scores <- scores %>%
    rename(TeamScore = Score) %>%
    mutate(UniqueTeamResult = TRUE)
  left_join(frame, scores) %>%
    tidyr::replace_na(list(TeamScore = 0))
}

#' Label the stage of the guesser's inventory relative to the team inventory.
label_stage <- function(Guesses) {
  Guesses %>%
    arrange(SessionTime) %>%
    group_by(SessionID) %>%
    mutate(Stage = ifelse(nchar(PrevTeamInventoryID) > nchar(PrevSessionInventoryID),
                          "learning", "playing")) %>%
    ungroup()
}
