#' Get player data from the database.
#' @import dplyr
#' @import RMySQL
#' @export
data_players <- function() {
  Players <- collect_tbl("Table_Player") %>%
    replace_id_group() %>%
    replace_id_player() %>%
    join_teams() %>%
    create_generation() %>%
    assign_player_ix() %>%
    arrange(Strategy, TeamID, Generation) %>%
    select(
      PlayerID,
      PlayerIX,
      Strategy,
      TeamID,
      Generation,
      SessionDuration
    )
  Players
}

#' Replace ID_Player with PlayerID
replace_id_player <- function(frame) {
  recode_id_player(frame) %>% select(-ID_Player)
}

#' Recode ID_Player (int) as PlayerID (char)
recode_id_player <- function(frame) {
  player_id_levels <- collect_tbl("Table_Player") %>%
    arrange(ID_Player) %>%
    .$ID_Player
  player_id_labels <- paste0("P", seq_along(player_id_levels))
  player_id_map <- data_frame(
    ID_Player = player_id_levels,
    PlayerID = player_id_labels
  )
  left_join(frame, player_id_map)
}

#' Create Generation from Ancestor by Strategy
create_generation <- function(frame) {
  mutate(frame, Generation = ifelse(Strategy != "Diachronic", 1, Ancestor))
}

#' Assign PlayerIX, which enumerates players in teams
assign_player_ix <- function(frame) {
  frame %>%
    group_by(TeamID, Generation) %>%
    mutate(PlayerIX = paste0("P", 1:n())) %>%
    ungroup()
}

#' Merge with Players data
join_players <- function(frame) {
  players <- data_players()
  left_join(frame, players)
}
