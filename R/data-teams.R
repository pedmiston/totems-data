#' Get Teams data.
#' @import dplyr
#' @export
data_teams <- function() {
  Teams <- collect_tbl("Table_Group") %>%
    rename(Strategy = Treatment, SessionDuration = BuildingTime) %>%
    replace_id_group() %>%
    mutate(
      TeamSize = ifelse(Strategy == "Isolated", 1, Size)
    ) %>%
    select(
      TeamID,
      Strategy,
      TeamSize,
      SessionDuration
    )
  Teams
}

#' Replace ID_Group with TeamID.
replace_id_group <- function(frame) {
  recode_id_group(frame) %>% select(-ID_Group)
}

#' Recode ID_Group as TeamID.
#'
#' TeamID is a deidentified version of ID_Group
#' with datetime information removed.
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

#' Merge with Teams data.
join_teams <- function(frame) {
  teams <- data_teams()
  left_join(frame, teams)
}
