label_valid_players <- function(frame) {
  teams <- collect_tbl("Table_Group") %>%
    rename(Strategy = Treatment, Duration = BuildingTime) %>%
    replace_id_group() %>%
    label_experiment() %>%
    replace_size() %>%
    select(TeamID, Strategy, TeamSize)

  players <- collect_tbl("Table_Player") %>%
    mutate(ID_Player = as.numeric(ID_Player)) %>%
    replace_id_group() %>%
    label_strategy() %>%
    replace_id_player() %>%
    replace_ancestor() %>%
    label_experiment() %>%
    left_join(teams)

  diachronic <- dplyr::filter(players, Strategy == "Diachronic") %>%
    group_by(Exp, TeamID) %>%
    mutate(ActualTeamSize = n()) %>%
    ungroup() %>%
    select(Exp, TeamID, PlayerID, Generation, ActualTeamSize)

  synchronic <- dplyr::filter(players, Strategy == "Synchronic")
  isolated <- dplyr::filter(players, Strategy == "Isolated")

  valid_player_map <- players %>%
    mutate(IsValidPlayer = TRUE) %>%
    select(Exp, TeamID, PlayerID, Session, IsValidPlayer)

  if(missing(frame)) return(valid_player_map)
  left_join(frame, valid_player_map)
}

label_valid_teams <- function(frame) {
  valid_team_map <- label_valid_players() %>%
    group_by(Exp, TeamID) %>%
    summarize(IsValidTeam = all(IsValidPlayer))

  if(missing(frame)) return(valid_team_map)
  left_join(frme, valid_team_map)
}
