session_duration_sec <- 25 * 60

#' Replace TrialTime (msec) with SessionTime (sec)
replace_trial_time <- function(frame) {
  frame %>%
    mutate(SessionTime = TrialTime/1000) %>%
    select(-TrialTime)
}

fix_bug_in_session_time <- function(frame) {
  session_id <- "S454"
  session_row <- frame$SessionID == session_id
  session_times <- frame[session_row, "SessionTime"]
  frame[session_row, "SessionTime"] <- session_times - min(session_times)
  frame
}

label_time <- function(frame) {
  frame %>%
    label_player_time() %>%
    label_team_time() %>%
    label_calendar_time() %>%
    label_guess_time()
}

label_player_time <- function(frame) {
  mutate(frame, PlayerTime = (SessionDuration*(SessionIX-1)) * 60 + SessionTime)
}

label_team_time <- function(frame) {
  mutate(frame, TeamTime = ifelse(Strategy == "Synchronic", SessionTime*PlayersPerSession,
                                  (SessionDuration*(Generation-1)) * 60 + SessionTime))
}

label_calendar_time <- function(frame) {
  mutate(frame, CalendarTime = ifelse(Strategy == "Synchronic", SessionTime, TeamTime))
}

label_guess_time <- function(frame) {
  frame %>%
    group_by(SessionID) %>%
    mutate(GuessTime = SessionTime - lag(SessionTime, default = 0)) %>%
    ungroup()
}
