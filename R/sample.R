
#' Sample guesses at regular intervals in a session.
sample_session <- function(session_guesses, default) {
  session_duration <- session_guesses$SessionDuration[[1]]
  sampled_times <- seq(1, session_duration, by = 1)
  indices <- findInterval(sampled_times, session_guesses$SessionTime)
  sampled_indices <- cbind(sampled_times, indices) %>% as.data.frame()
  valid_sample_times <- dplyr::filter(sampled_indices, indices != 0)
  sampled_guesses <- session_guesses[valid_sample_times$indices, ]
  sampled_guesses$SampledSessionTime <- valid_sample_times$sampled_times

  if(!missing(default)) {
    missing_sample_times <- dplyr::filter(sampled_indices, indices == 0) %>%
      select(SampledSessionTime = sampled_times)
    missing_samples <- merge(missing_sample_times, default)
    sampled_guesses <- bind_rows(missing_samples, sampled_guesses)
  }

  sampled_guesses
}
