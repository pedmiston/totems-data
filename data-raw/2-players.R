Players <- read_table("Table_Player") %>%
  replace_id_group() %>%
  label_session_duration() %>%
  label_strategy() %>%
  label_players_per_session() %>%
  replace_id_player() %>%
  replace_ancestor() %>%
  label_player_conditions() %>%
  label_session_status() %>%
  label_team_status() %>%
  select(
    Exp,
    PlayerID,
    PlayerIX,
    SessionID,
    SessionIX,
    TeamID,
    Strategy,
    NumPlayers,
    Generation,
    SessionDuration,
    SessionStatus,
    TeamStatus
  ) %>%
  arrange(TeamID, desc(Exp), PlayerIX, SessionIX, Strategy)

ValidSessions <- select(Players, Exp, PlayerID, SessionID, SessionStatus, TeamStatus)

devtools::use_data(Players, ValidSessions, overwrite = TRUE)
