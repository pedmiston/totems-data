#' Create a map of ID_Player to PlayerID, PlayerIX, SessionID, and SessionIX.
#'
#' For Diachronic and Synchronic players:
#' PlayerID = "P" + ID_Player
#' PlayerIX = 1:4?
#' SessionID = "S" + ID_Player
#' SessionIX = 1
#'
#' For Isolated players, who are assigned multiple values for ID_Player:
#' PlayerID = "P" + max(ID_Player)
#' PlayerIX = 1
#' SessionID = "S" + ID_Player
#' SessionIX = 1:4?
create_player_id_map <- function() {
  players <- read_table("Table_Player") %>%
    replace_id_group() %>%
    label_strategy() %>%
    select(ID_Player, TeamID, Strategy)

  team_players <- dplyr::filter(players, Strategy != "Isolated") %>%
    mutate(
      PlayerID = paste0("P", ID_Player),
      SessionID = paste0("S", ID_Player),
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
    team_players,
    isolated_players
  ) %>%
    select(ID_Player, PlayerID, PlayerIX, SessionID, SessionIX)

  player_id_map
}

#' Replace ID_Player with PlayerID, PlayerIX, SessionID, and SessionIX.
replace_id_player <- function(frame) {
  player_id_map <- create_player_id_map()
  left_join(frame, player_id_map) %>%
    select(-ID_Player)
}

#' Label PlayerID, PlayerIX, and SessionIX from SessionID
label_session_player <- function(frame) {
  player_id_map <- create_player_id_map() %>%
    select(-ID_Player)
  left_join(frame, player_id_map)
}

#' Create Generation from Strategy and Ancestor.
#'
#' For Synchronic teams, Generation = 1.
#' For Diachronic and Isolated teams, Generation = Ancestor.
replace_ancestor <- function(frame) {
  frame %>%
    mutate(Generation = ifelse(Strategy == "Synchronic", 1, Ancestor),
           # Fix bug in Isolated 50 Minute players where Ancestor is mistakenly coded as 0.
           # This could be fixed in the DB!
           Generation = ifelse(Strategy == "Isolated" & SessionDuration == 50, 1, Generation)) %>%
    select(-Ancestor)
}

#' Label Generation based on Strategy, PlayerIX, and SessionIX.
label_generation <- function(frame) {
  map <- player_conditions() %>%
    select(Strategy, PlayerIX, SessionIX, Generation) %>%
    unique()
  if(missing(frame)) return(map)
  left_join(frame, map)
}

#' Label TeamID from PlayerID.
label_team_id <- function(frame) {
  map <- read_table("Table_Player") %>%
    replace_id_group() %>%
    replace_id_player() %>%
    select(PlayerID, TeamID) %>%
    unique()
  if(missing(frame)) return(map)
  left_join(frame, map)
}
