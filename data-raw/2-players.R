Players <- read_table("Table_Player") %>%
  replace_id_group() %>%
  label_session_duration() %>%
  label_strategy() %>%
  label_players_per_session() %>%
  replace_id_player() %>%
  replace_ancestor() %>%
  label_player_conditions() %>%
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
    SessionDuration
  ) %>%
  arrange(TeamID, desc(Exp), PlayerIX, SessionIX, Strategy)

devtools::use_data(Players, overwrite = TRUE)
