#' Sample guesses at regular intervals in a session.
#'
#' Requires SessionID, SessionDuration, SessionTime, NumSessionGuesses,
#' PrevSessionInventoryID, SessionInventorySize,
#' PrevSessionGuessesHash, NumUniqueSessionGuesses
sample_session <- function(session_guesses, default) {
  # Create samples as a map of sampled session times to indices in session guesses
  samples <- data_frame(
    SampledSessionTime = seq(60, session_guesses$SessionDuration[[1]] * 60, by = 60),
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
