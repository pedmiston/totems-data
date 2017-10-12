session_duration_sec <- 25 * 60

#' Replace TrialTime (msec) with SessionTime (sec)
replace_trial_time <- function(frame) {
  frame %>%
    mutate(SessionTime = TrialTime/1000) %>%
    select(-TrialTime)
}

label_time <- function(frame) {
  frame %>%
    label_player_time() %>%
    label_team_time() %>%
    label_calendar_time()
}

label_player_time <- function(frame) {
  durations <- collect_tbl("Table_Group") %>%
    replace_id_group() %>%
    select(TeamID, DurationMin = BuildingTime)

  frame %>%
    left_join(durations) %>%
    mutate(PlayerTime = DurationMin*(SessionIX-1) + SessionTime)
}

label_team_time <- function(frame) {
  durations <- collect_tbl("Table_Group") %>%
    replace_id_group() %>%
    select(TeamID, DurationMin = BuildingTime)

  frame %>%
    left_join(durations) %>%
    label_team_size() %>%
    mutate(TeamTime = ifelse(Strategy == "Synchronic", SessionTime*TeamSize,
                             DurationMin*(Generation-1) + SessionTime))
}

label_calendar_time <- function(frame) {
  frame %>%
    mutate(CalendarTime = ifelse(Strategy == "Synchronic", SessionTime, TeamTime))
}
