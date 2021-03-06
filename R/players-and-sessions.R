#' Replace ID_Player with PlayerID, PlayerIX, SessionID, and SessionIX.
#'
#' For all players:
#' SessionID = "S" + ID_Player
#'
#' For Diachronic and Synchronic players:
#' PlayerID = "P" + ID_Player
#' PlayerIX = 1:4?
#' SessionIX = 1
#'
#' For Isolated players, who are assigned multiple values for ID_Player:
#' PlayerID = "P" + max(ID_Player)
#' PlayerIX = 1
#' SessionIX = 1:4?
replace_id_player <- function(frame) {
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
    group_by(TeamID) %>%
    arrange(ID_Player) %>%
    mutate(PlayerIX = 1:n()) %>%
    ungroup()

  isolated_players <- dplyr::filter(players, Strategy == "Isolated") %>%
    group_by(TeamID) %>%
    arrange(ID_Player) %>%
    mutate(
      PlayerID = paste0("P", max(ID_Player)),
      PlayerIX = 1,
      SessionID = paste0("S", ID_Player),
      SessionIX = 1:n()
    )

  player_id_map <- bind_rows(team_players, isolated_players) %>%
    select(ID_Player, PlayerID, PlayerIX, SessionID, SessionIX)

  if(missing(frame)) return(player_id_map)
  left_join(frame, player_id_map) %>%
    select(-ID_Player)
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
  diachronic <- data_frame(
    Strategy = "Diachronic",
    PlayerIX = 1:4,
    SessionIX = 1,
    Generation = 1:4
  )

  isolated <- data_frame(
    Strategy = "Isolated",
    PlayerIX = 1,
    SessionIX = 1:4,
    Generation = 1:4
  )

  synchronic <- data_frame(
    Strategy = "Synchronic",
    PlayerIX = 1:4,
    SessionIX = 1,
    Generation = 1
  )

  map <- bind_rows(
    diachronic,
    isolated,
    synchronic
  )
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

#' Label PlayerID, PlayerIX, and SessionIX from SessionID
label_session_player <- function(frame) {
  player_id_map <- replace_id_player() %>%
    select(-ID_Player)
  if(missing(frame)) return(player_id_map)
  left_join(frame, player_id_map)
}

