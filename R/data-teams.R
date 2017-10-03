#' Get team data from the database.
#' @import dplyr
data_teams <- function(con) {
  Teams <- tbl(con, "Table_Group") %>%
    rename_group_id(con) %>%
    rename(Strategy = Treatment, TeamSize = Size) %>%
    select(TeamID, Strategy, TeamSize)
  collect(Teams)
}

#' Replace ID_Group with TeamID.
#' @import dplyr
rename_group_id <- function(frame, con) {
  recode_group_id(frame, con) %>% select(-ID_Group)
}

#' Recode ID_Group as TeamID without datetime information.
#' @import dplyr
recode_group_id <- function(frame, con) {
  team_id_levels <- tbl(con, "Table_Group") %>%
    arrange(ID_Group) %>%
    collect() %>%
    .$ID_Group
  team_id_labels <- paste0("G", seq_along(team_id_levels))
  team_id_map <- data_frame(
    ID_Group = team_id_levels,
    TeamID = team_id_labels
  )
  left_join(frame, team_id_map)
}
