#' Get Teams data.
#' @import dplyr
#' @import RMySQL
#' @export
data_teams <- function() {
  Teams <- collect_tbl("Table_Group") %>%
    replace_id_group() %>%
    mutate(
      TeamSize = ifelse(Treatment == "Isolated", 1, Size)
    ) %>%
    select(
      TeamID,
      Strategy = Treatment,
      TeamSize,
      SessionDuration = BuildingTime
    )
  Teams
}

#' Replace ID_Group with TeamID.
#'
#' This function deidentifies ID_Group by removing
#' datetime information from it.
replace_id_group <- function(frame) {
  recode_id_group(frame) %>% select(-ID_Group)
}

#' Recode ID_Group as TeamID.
recode_id_group <- function(frame) {
  team_id_levels <- collect_tbl("Table_Group") %>%
    arrange(ID_Group) %>%
    .$ID_Group
  team_id_labels <- paste0("G", seq_along(team_id_levels))
  team_id_map <- data_frame(
    ID_Group = team_id_levels,
    TeamID = team_id_labels
  )
  left_join(frame, team_id_map)
}

#' Left join the given frame with the teams data.
join_teams <- function(frame) {
  teams <- data_teams()
  left_join(frame, teams)
}
