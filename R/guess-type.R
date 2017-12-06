#' Recode guess type based on the uniqueness of the guess, whether the guess
#' created an item, and the uniqueness of the created item.
#'
#' The reason for the params `unique_guess` and `unique_result` is that
#' uniqueness can be determined relative to either the session or to the team.
#'
#' @param unique_guess str Name of column in frame containing uniqueness of
#'          the guess, e.g., "UniqueSessionGuess"
#' @param unique_result str Name of column in frame containing uniqueness of
#'          result, e.g., "UniqueTeamResult"
#'
#' @return Same as input with new columns GuessType and GuessTypeLabel.
#'
#' @import dplyr
#' @export
recode_guess_type <- function(frame, unique_guess, unique_result, suffix) {
  guess_type_levels <- c("redundant", "repeat_item", "unique_guess", "unique_item")
  guess_type_labels <- c("Redundant", "Repeat item", "Unique guess", "Unique item")
  guess_type_map <- data_frame(
    UniqueGuess = c(0, 0, 1, 1),
    CreatedItem = c(0, 1, 0, 1),
    UniqueResult = c(0, 0, 0, 1),
    GuessType = guess_type_levels,
    GuessTypeLabel = factor(guess_type_levels, labels = guess_type_labels)
  )
  if(!missing(suffix)) {
    # Renaming in a function when column names are created from arguments
    guess_type_map[, paste0("GuessType", suffix)] <- guess_type_map$GuessType
    guess_type_map[, paste0("GuessTypeLabel", suffix)] <- guess_type_map$GuessTypeLabel
    guess_type_map <- guess_type_map %>%
      select(-GuessType, -GuessTypeLabel)
  }
  if(missing(frame)) return(guess_type_map)

  # Renaming in a function when column names are given as arguments
  guess_type_map[, unique_guess] <- guess_type_map$UniqueGuess
  guess_type_map[, unique_result] <- guess_type_map$UniqueResult
  guess_type_map <- guess_type_map %>%
    select(-UniqueGuess, UniqueResult)

  left_join(frame, guess_type_map)
}

#' @export
recode_guess_type_total <- function(frame) {
  guess_type_total_map <- data_frame(
    GuessTypeTotal = c("NumRedundantGuesses", "NumRepeatedItems", "NumUniqueGuesses", "NumInnovations"),
    GuessType = c("redundant", "repeat_item", "unique_guess", "unique_result")
  )
  if(missing(frame)) return(guess_type_total_map)
  left_join(frame, guess_type_total_map)
}

#' @export
recode_prop_guess_type_total <- function(frame) {
  prop_guess_type_total_map <- data_frame(
    PropGuessType = c("PropRedundantGuesses", "PropRepeatedItems", "PropUniqueGuesses", "PropUniqueItems"),
    PropGuessTypeLabel = c("Redundant", "Repeat item", "Unique guess", "Unique item"),
    GuessType = c("redundant", "repeat_item", "unique_guess", "unique_result")
  )
  if(missing(frame)) return(prop_guess_type_total_map)
  left_join(frame, prop_guess_type_total_map)
}

#' @export
recode_strategy_by_session_duration <- function(frame) {
  strategy_by_session_duration_map <- data_frame(
    Strategy = c("Diachronic", "Synchronic", "Isolated", "Isolated"),
    SessionDuration = c(25, 25, 25, 50),
    StrategyBySessionDuration = c("Diachronic-25", "Synchronic-25", "Isolated-25", "Isolated-50")
  )
  if(missing(frame)) return(strategy_by_session_duration_map)
  left_join(frame, strategy_by_session_duration_map)
}
