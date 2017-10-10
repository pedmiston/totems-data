#' Get Player data.
data_players <- function() {
  teams <- collect_tbl("Table_Group") %>%
    replace_id_group() %>%
    select(TeamID, Strategy = Treatment, Duration = BuildingTime)

  players <- collect_tbl("Table_Player") %>%
    replace_id_group() %>%
    left_join(teams) %>%
    replace_id_player() %>%
    replace_ancestor() %>%
    label_experiment() %>%
    label_valid_players() %>%
    select(
      Exp,
      PlayerID,
      TeamID,
      Strategy,
      Session,
      Generation,
      Duration
    )
   players %>% arrange(desc(Exp), TeamID, Generation, Session)
}

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
  players <- collect_tbl("Table_Player") %>%
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
    )
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
  collect_tbl("Table_Player") %>%
    recode_player_id() %>%
    replace_ancestor() %>%
    select(PlayerID, SessionIX, Generation)
}

label_team_id <- function(frame) {
  map <- collect_tbl("Table_Player") %>%
    replace_group_id() %>%
    replace_player_id() %>%
    select(PlayerID, TeamID)
  if(missing(frame)) return(map)
  left_join(frame, map)
}
