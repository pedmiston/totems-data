
#' Sample guesses at regular intervals in a session.
sample_session <- function(session_guesses) {
  session_duration <- session_guesses$SessionDuration[[1]]
  sampled_times <- seq(1, session_duration, by = 1)
  indices <- findInterval(sampled_times, session_guesses$SessionTime)
  sampled_indices <- cbind(sampled_times, indices) %>% as.data.frame()
  valid_sample_times <- dplyr::filter(sampled_indices, indices != 0)
  sampled_guesses <- session_guesses[valid_sample_times$indices, ]
  sampled_guesses$SampledSessionTime <- valid_sample_times$sampled_times
  sampled_guesses
}
