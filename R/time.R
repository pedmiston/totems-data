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
  durations <- read_table("Table_Group") %>%
    replace_id_group() %>%
    select(TeamID, SessionDurationMin = BuildingTime)

  frame %>%
    left_join(durations) %>%
    mutate(PlayerTime = SessionDurationMin*(SessionIX-1) + SessionTime)
}

label_team_time <- function(frame) {
  durations <- read_table("Table_Group") %>%
    replace_id_group() %>%
    select(TeamID, SessionDurationMin = BuildingTime)

  frame %>%
    left_join(durations) %>%
    mutate(TeamTime = ifelse(Strategy == "Synchronic", SessionTime*NumPlayers,
                             SessionDurationMin*(Generation-1) + SessionTime))
}

label_calendar_time <- function(frame) {
  frame %>%
    mutate(CalendarTime = ifelse(Strategy == "Synchronic", SessionTime, TeamTime))
}
