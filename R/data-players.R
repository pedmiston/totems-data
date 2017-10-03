#' Get player data from the database.
#' @param con DBIConnection class. See \code{\link{connect_db}}.
#' @import dplyr
#' @export
data_players <- function(con) {
  Players <- tbl(con, "Table_Player") %>%
    rename_player_id(con) %>%
    rename_group_id(con) %>%
    create_generation() %>%
    assign_player_ix() %>%
    select(PlayerID, TeamID, Strategy = Treatment, Generation, PlayerIX, SessionDuration = BuildingTime) %>%
    arrange(Strategy, TeamID, Generation)
  collect(Players)
}

#' Recode ID_Player (int) as PlayerID (char)
recode_player_id <- function(frame, con) {
  player_id_levels <- tbl(con, "Table_Player") %>%
    arrange(ID_Player) %>%
    collect() %>%
    .$ID_Player
  player_id_labels <- paste0("P", seq_along(player_id_levels))
  player_id_map <- data_frame(
    ID_Player = player_id_levels,
    PlayerID = player_id_labels
  )
  left_join(frame, player_id_map)
}

#' Replace ID_Player with PlayerID
rename_player_id <- function(frame, con) {
  recode_player_id(frame, con) %>% select(-ID_Player)
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
