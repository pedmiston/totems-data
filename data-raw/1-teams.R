Teams <- read_table("Table_Group") %>%
  rename(Strategy = Treatment, SessionDuration = BuildingTime) %>%
  replace_id_group() %>%
  label_players_per_session() %>%
  label_team_conditions() %>%
  label_team_status() %>%
  select(
    Exp,
    TeamID,
    Strategy,
    NumPlayers,
    SessionsPerPlayer,
    PlayersPerSession,
    SessionDuration,
    TeamStatus
  ) %>%
  arrange(TeamID, desc(Exp))

devtools::use_data(Teams, overwrite = TRUE)
