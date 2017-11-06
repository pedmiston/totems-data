
#' Sample guesses at regular intervals in a session.
#'
#' Requires SessionID, SessionDuration, SessionTime
sample_session <- function(session_guesses) {
  session_duration <- session_guesses$SessionDuration[[1]]
  sampled_times <- seq(1, session_duration, by = 1)
  indices <- findInterval(sampled_times, session_guesses$SessionTime)
  sampled_indices <- cbind(sampled_times, indices) %>% as.data.frame()
  valid_sample_times <- dplyr::filter(sampled_indices, indices != 0)
  sampled_guesses <- session_guesses[valid_sample_times$indices, ]
  sampled_guesses$SampledSessionTime <- valid_sample_times$sampled_times

  missing_sample_times <- dplyr::filter(sampled_indices, indices == 0) %>%
    select(SampledSessionTime = sampled_times)
  if(nrow(missing_sample_times) > 0) {
    default <- head(session_guesses, n = 1) %>%
      mutate(
        Guess = NA_character_,
        Result = NA_integer_,
        PrevSessionInventoryID = "1-2-3-4-5-6",
        SessionInventorySize = 6,
        PrevSessionGuessesHash = digest::digest(""),
        NumSessionGuesses = 0
      )
    missing <- merge(missing_sample_times, default)
    sampled_guesses <- bind_rows(missing, sampled_guesses)
  }

  sampled_guesses
}
