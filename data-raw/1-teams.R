Teams <- read_table("Table_Group") %>%
  rename(Strategy = Treatment, SessionDuration = BuildingTime) %>%
  replace_id_group() %>%
  label_players_per_session() %>%
  label_team_conditions() %>%
  select(
    Exp,
    TeamID,
    Strategy,
    NumPlayers,
    SessionsPerPlayer,
    PlayersPerSession,
    SessionDuration
  ) %>%
  arrange(TeamID, desc(Exp))

