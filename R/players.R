
join_players <- function(frame) left_join(frame, data_players())

#' Replace ID_Player with PlayerID, SessionID, and SessionIX
#'
#' For Diachronic and Synchronic players:
#' PlayerID = "P" + ID_Player
#' SessionID = "S" + ID_Player
#' SessionIX = 1
#'
#' For Isolated players, who are assigned multiple values for ID_Player:
#' SessionID = "S" + ID_Player
#' SessionIX = 1:4?
#' PlayerID = "P" + max(ID_Player)
replace_id_player <- function(frame) {
  players <- read_table("Table_Player") %>%
    replace_id_group() %>%
    label_strategy() %>%
    select(ID_Player, TeamID, Strategy)

  # Treat team players and isolated players separately.
  team_players <- dplyr::filter(players, Strategy != "Isolated") %>%
    mutate(
      PlayerID = paste0("P", ID_Player),
      SessionID = paste0("S", ID_Player),
      SessionIX = 1
    )
  isolated_players <- dplyr::filter(players, Strategy == "Isolated") %>%
    mutate(ID_PlayerInt = as.numeric(ID_Player)) %>%
    arrange(ID_PlayerInt) %>%
    group_by(TeamID) %>%
    mutate(
      PlayerID = paste0("P", max(ID_PlayerInt)),
      SessionID = paste0("S", ID_Player),
      SessionIX = 1:n()
    ) %>%
    select(-ID_PlayerInt)
  player_id_map <- bind_rows(team_players, isolated_players) %>%
    select(ID_Player, PlayerID, SessionID, SessionIX)

  left_join(frame, player_id_map) %>%
    select(-ID_Player)
}

#' Create Generation from Strategy and Ancestor.
#'
#' For Synchronic teams, Generation = 1.
#' For Diachronic and Isolated teams, Generation = Ancestor.
replace_ancestor <- function(frame) {
  frame %>%
    mutate(Generation = ifelse(Strategy == "Synchronic", 1, Ancestor)) %>%
    select(-Ancestor)
}

label_generation <- function(frame) {
  map <- read_table("Table_Player") %>%
    replace_id_player() %>%
    label_team_id() %>%
    label_strategy() %>%
    replace_ancestor() %>%
    select(PlayerID, SessionIX, Generation)
  if(missing(frame)) return(map)
  left_join(frame, map)
}

label_team_id <- function(frame) {
  map <- read_table("Table_Player") %>%
    replace_id_group() %>%
    replace_id_player() %>%
    select(PlayerID, TeamID) %>%
    unique()
  if(missing(frame)) return(map)
  left_join(frame, map)
}


label_valid_players <- function(frame) {
  teams <- read_table("Table_Group") %>%
    rename(Strategy = Treatment, Duration = BuildingTime) %>%
    replace_id_group() %>%
    label_experiment() %>%
    replace_size() %>%
    select(TeamID, Strategy, TeamSize)

  players <- read_table("Table_Player") %>%
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
