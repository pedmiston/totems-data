data_sampled <- function() {
  Guesses <- data_guesses()
  SampledPerformance <- Guesses %>%
    sample_every(60)
}

sample_every <- function(frame, seconds) {
  frame %>%
    left_join(data_teams()[,c("TeamID", "SessionDuration")])


}

#' @import dplyr
get_closest_trials_to_times <- function(trials, times, time_col = "TeamTime") {
  lapply(times, get_closest_trial_to_time, trials = trials, time_col = time_col) %>%
    bind_rows()
}

#' @import magrittr
get_closest_trial_to_time <- function(time, trials, time_col = "TeamTime",
                                      sample_time_col = "SampledTime") {
  trials %<>% arrange_(.dots = time_col)
  trials_that_have_happened <- trials[[time_col]] <= time
  trial <- trials[trials_that_have_happened, ] %>% tail(1)
  if (nrow(trial) == 1) {
    trial[, sample_time_col] <- time
  } else if (time == 0) {
    first_trial <- trials %>%
      head(1) %>%
      select(PlayerID, TeamID, Strategy) %>%
      mutate(
        NumInnovations = 0,
        GuessNum = 0,
        TeamGuessNum = 0
      )
    trial %<>% bind_rows(first_trial)
  }
  trial
}
