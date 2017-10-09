#' Get Teams data.
#'
#' @import dplyr
#' @export
data_teams <- function() {
  Teams <- collect_tbl("Table_Group") %>%
    rename(Strategy = Treatment, Duration = BuildingTime) %>%
    replace_id_group() %>%
    label_experiment() %>%
    replace_size() %>%
    label_valid_teams() %>%
    select(
      TeamID,
      Strategy,
      Duration,
      Exp,
      TeamSize
    )
  Teams
}

#' Replace ID_Group with TeamID.
#'
#' TeamID is a deidentified version of ID_Group
#' with datetime information removed.
replace_id_group <- function(frame) {
  team_id_levels <- collect_tbl("Table_Group") %>%
    arrange(ID_Group) %>%
    .$ID_Group
  team_id_labels <- paste0("G", seq_along(team_id_levels))
  team_id_map <- data_frame(
    ID_Group = team_id_levels,
    TeamID = team_id_labels
  )
  left_join(frame, team_id_map) %>%
    select(-ID_Group)
}

#' Determine TeamSize from Strategy and Exp.
replace_size <- function(frame) {
  team_size_map <- data_frame(
    Strategy = rep(c("Isolated", "Diachronic", "Synchronic"), each = 2),
    Exp = rep(c("50LaborMinutes", "100LaborMinutes"), times = 3),
    TeamSize = c(1, 1, 2, 4, 2, 4)
  )
  left_join(frame, team_size_map) %>%
    select(-Size)
}

#' Label the strategy of the teams by TeamID.
label_strategy <- function(frame) {
  strategies <- collect_tbl("Table_Group") %>%
    replace_id_group() %>%
    select(TeamID, Strategy = Treatment)
  left_join(frame, strategies)
}

#' Join the given frame with the Teams data.
join_teams <- function(frame) {
  teams <- data_teams()
  left_join(frame, teams)
}
