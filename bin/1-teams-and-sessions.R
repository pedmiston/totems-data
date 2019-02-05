Teams <- read_table("Table_Group") %>%
  rename(Strategy = Treatment, SessionDuration = BuildingTime) %>%
  replace_id_group() %>%
  label_players_per_session() %>%
  select(
    TeamID,
    Strategy,
    PlayersPerSession,
    SessionDuration
  ) %>%
  arrange(TeamID)

Sessions <- read_table("Table_Player") %>%
  replace_id_group() %>%
  replace_id_player() %>%
  label_strategy() %>%
  label_session_duration() %>%
  replace_ancestor() %>%
  select(
    PlayerID,
    PlayerIX,
    SessionID,
    SessionIX,
    SessionDuration,
    TeamID,
    Strategy,
    Generation
  ) %>%
  arrange(TeamID, PlayerIX, SessionIX)

devtools::use_data(Teams, Sessions, overwrite = TRUE)
