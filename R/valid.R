#' Label session status.
#'
#' V: Valid session.
#' T: Test session.
#' no_guesses: Session has no guesses associated with it.
#' unknown_session: Session is not recorded
#'
#' **NOT IMPLEMENTED**
#' E: Error in experiment during session.
#' B: Behavioral issues during session.
label_session_status <- function(frame) {
  sessions <- read_table("Table_Player") %>%
    replace_id_group() %>%
    label_session_duration() %>%
    label_strategy() %>%
    replace_id_player() %>%
    replace_ancestor()

  sessions$SessionStatus <- ifelse(sessions$SessionDuration %in% c(25, 50), "V", "T")

  # Label sessions with no guesses
  sessions_with_no_guesses <- c("S157", "S158", "S160", "S162", "S202", "S475", "S314", "S315", "S340", "S391", "S599", "S600", "S601", "S168")
  sessions <- sessions %>%
    mutate(SessionStatus = ifelse(SessionID %in% sessions_with_no_guesses, "no_guesses", SessionStatus))

  invalid_sessions_with_guesses <- "S448"
  sessions <- sessions %>%
    mutate(SessionStatus = ifelse(SessionID %in% invalid_sessions_with_guesses, "unknown_session", SessionStatus))

  valid_sessions_map <- select(sessions, SessionID, SessionStatus)

  if(missing(frame)) return(valid_sessions_map)
  left_join(frame, valid_sessions_map)
}

#' Label team status.
#'
#' V: Valid session.
#' I: Incomplete team.
#' no_guesses: Team has no guesses associated with it.
label_team_status <- function(frame) {
  sessions <- read_table("Table_Player") %>%
    replace_id_group() %>%
    label_session_duration() %>%
    label_strategy() %>%
    label_players_per_session() %>%
    replace_id_player() %>%
    replace_ancestor() %>%
    label_player_conditions() %>%
    label_session_status()

  isolated <- sessions %>%
    dplyr::filter(Strategy == "Isolated") %>%
    group_by(Exp, TeamID, SessionsPerPlayer) %>%
    summarize(
      CompletedSessions = n(),
      AllValidSessions = all(SessionStatus == "V")
    ) %>%
    ungroup() %>%
    mutate(TeamStatus = ifelse(SessionsPerPlayer == CompletedSessions & AllValidSessions, "V", "I")) %>%
    select(Exp, TeamID, TeamStatus)

  diachronic <- sessions %>%
    dplyr::filter(Strategy == "Diachronic") %>%
    group_by(Exp, TeamID, NumPlayers) %>%
    summarize(
      CompletedPlayers = n(),
      AllValidSessions = all(SessionStatus == "V")
    ) %>%
    ungroup() %>%
    mutate(TeamStatus = ifelse(CompletedPlayers == NumPlayers & AllValidSessions, "V", "I")) %>%
    select(Exp, TeamID, TeamStatus)

  # This shouldn't do anything because all synchronic players
  # are required in order to start the experiment.
  synchronic <- sessions %>%
    dplyr::filter(Strategy == "Synchronic") %>%
    group_by(Exp, TeamID, NumPlayers) %>%
    summarize(
      CompletedPlayers = n(),
      AllValidSessions = all(SessionStatus == "V")
    ) %>%
    ungroup() %>%
    mutate(TeamStatus = ifelse(CompletedPlayers == NumPlayers & AllValidSessions, "V", "I")) %>%
    select(Exp, TeamID, TeamStatus)

  team_statuses <- bind_rows(isolated, diachronic, synchronic)

  teams_with_no_guesses <- c("G138", "G153", "G75")
  team_statuses <- team_statuses %>%
    mutate(TeamStatus = ifelse(TeamID %in% teams_with_no_guesses, "no_guesses", TeamStatus))

  if(missing(frame)) return(team_statuses)
  left_join(frame, team_statuses)
}
