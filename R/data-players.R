#' Get player data from the database.
#' @import dplyr
#' @import RMySQL
#' @export
data_players <- function() {
  Players <- collect_tbl("Table_Player") %>%
    mutate(ID_Player = as.numeric(ID_Player)) %>%
    replace_id_group() %>%
    join_teams() %>%
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
  Players %>%
    arrange(desc(Exp), TeamID, Generation, Session)
}

#' Replace ID_Player with PlayerID and Session.
#'
#' For Diachronic and Synchronic players, PlayerID is
#' just the letter P plus ID_Player, and Session is 1.
#'
#' For Isolated players, ID_Player is more analogous
#' to Session because Isolated players who return for
#' multiple sessions are given multiple ID_Player values.
#'
replace_id_player <- function(frame) {
  players <- collect_tbl("Table_Player") %>%
    mutate(ID_Player = as.integer(ID_Player)) %>%
    replace_id_group() %>%
    select(ID_Player, TeamID) %>%
    label_strategy()

  # Treat team players and isolated players separately.
  team_players <- dplyr::filter(players, Strategy != "Isolated") %>%
    mutate(PlayerID = paste0("P", ID_Player), Session = 1)
  isolated_players <- dplyr::filter(players, Strategy == "Isolated") %>%
    group_by(TeamID) %>%
    mutate(
      PlayerID = paste0("P", min(ID_Player)),
      Session = 1:n()
    )
  player_id_map <- bind_rows(team_players, isolated_players) %>%
    select(ID_Player, PlayerID, Session)

  left_join(frame, player_id_map) %>%
    select(-ID_Player)
}

#' Create Generation from Ancestor by Strategy.
replace_ancestor <- function(frame) {
  frame %>%
    mutate(Generation = ifelse(Strategy == "Synchronic", 1, Ancestor)) %>%
    select(-Ancestor)
}

#' Merge with Players data
join_players <- function(frame) {
  players <- data_players()
  left_join(frame, players)
}
