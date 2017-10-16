#' Replace ID_Player with PlayerID, PlayerIX, SessionID, and SessionIX.
#'
#' For Diachronic players:
#' PlayerID = "P" + ID_Player
#' PlayerIX = 1:4?
#' SessionID = "S" + ID_Player
#' SessionIX = 1
#'
#' For Synchronic players, who participate in the same session:
#' PlayerID = "P" + ID_Player
#' PlayerIX = 1:4?
#' SessionID = "S" + min(ID_Player)
#' SessionIX = 1
#'
#' For Isolated players, who are assigned multiple values for ID_Player:
#' PlayerID = "P" + max(ID_Player)
#' PlayerIX = 1
#' SessionID = "S" + ID_Player
#' SessionIX = 1:4?
replace_id_player <- function(frame) {
  players <- read_table("Table_Player") %>%
    replace_id_group() %>%
    label_strategy() %>%
    select(ID_Player, TeamID, Strategy)

  # Treat each strategy separately

  diachronic_players <- dplyr::filter(players, Strategy == "Diachronic") %>%
    mutate(
      PlayerID = paste0("P", ID_Player),
      SessionID = paste0("S", ID_Player),
      SessionIX = 1
    ) %>%
    arrange(ID_Player) %>%
    group_by(TeamID) %>%
    mutate(PlayerIX = 1:n()) %>%
    ungroup()

  synchronic_players <- dplyr::filter(players, Strategy == "Synchronic") %>%
    mutate(
      PlayerID = paste0("P", ID_Player),
      SessionID = paste0("S", min(ID_Player)),
      SessionIX = 1
    ) %>%
    arrange(ID_Player) %>%
    group_by(TeamID) %>%
    mutate(PlayerIX = 1:n()) %>%
    ungroup()

  isolated_players <- dplyr::filter(players, Strategy == "Isolated") %>%
    arrange(ID_Player) %>%
    group_by(TeamID) %>%
    mutate(
      PlayerID = paste0("P", max(ID_Player)),
      PlayerIX = 1,
      SessionID = paste0("S", ID_Player),
      SessionIX = 1:n()
    )

  player_id_map <- bind_rows(
      diachronic_players,
      synchronic_players,
      isolated_players
    ) %>%
    select(ID_Player, PlayerID, PlayerIX, SessionID, SessionIX)

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
  players <- read_table("Table_Player") %>%
    replace_id_group() %>%
    label_strategy() %>%
    replace_id_player() %>%
    replace_ancestor() %>%
    label_player_experiment()

  team_players_map <- dplyr::filter(players, Strategy != "Isolated") %>%
    group_by(Exp, TeamID) %>%
    mutate(IsPlayerValid = (n() == TeamSize)) %>%
    ungroup()

  isolated_players_map <- dplyr::filter(players, Strategy == "Isolated") %>%
    group_by(Exp, Duration, PlayerID) %>%
    mutate(IsPlayerValid = (n() == SessionSize))

  valid_players_map <- bind_rows(team_players_map, isolated_players_map) %>%
    select(Exp, TeamID, PlayerID, SessionID, SessionIX, IsPlayerValid)

  if(missing(frame)) return(valid_players_map)
  left_join(frame, valid_players_map)
}

join_players <- function(frame) left_join(frame, data_players())
