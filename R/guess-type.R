#' Recode guess type based on the uniqueness of the guess, whether the guess
#' created an item, and the uniqueness of the created item.
#'
#' @param unique_guess str Name of column in frame containing uniqueness of the guess.
#' @param unique_result str Name of column in frame containing uniqueness of result.
#'
#' @import dplyr
#' @export
recode_guess_type <- function(frame, unique_guess, unique_result) {
  guess_type_levels <- c("redundant", "repeat_item", "unique_guess", "unique_item")
  guess_type_labels <- c("Redundant", "Repeat item", "Unique guess", "Unique item")
  guess_type_map <- data_frame(
    UniqueGuess = c(0, 0, 1, 1),
    CreatedItem = c(0, 1, 0, 1),
    UniqueResult = c(0, 0, 0, 1),
    GuessType = guess_type_levels,
    GuessTypeLabel = factor(guess_type_levels, labels = guess_type_labels)
  )
  if(missing(frame)) return(guess_type_map)

  guess_type_map[, unique_guess] <- guess_type_map$UniqueGuess
  guess_type_map[, unique_result] <- guess_type_map$UniqueResult

  guess_type_map <- guess_type_map %>%
    select(-UniqueGuess, UniqueResult)

  left_join(frame, guess_type_map)
}
