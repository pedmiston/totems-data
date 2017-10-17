#' Replace ID_Group with TeamID.
#'
#' TeamID is a deidentified version of ID_Group
#' with datetime information removed.
replace_id_group <- function(frame) {
  id_groups <- read_table("Table_Group")$ID_Group %>% sort()
  team_ids <- paste0("G", seq_along(id_groups))
  map <- data_frame(ID_Group = id_groups, TeamID = team_ids)
  if(missing(frame)) return(map)
  left_join(frame, map) %>% select(-ID_Group)
}

#' Label the Strategy of the teams by TeamID.
label_strategy <- function(frame) {
  map <- read_table("Table_Group") %>%
    replace_id_group() %>%
    select(TeamID, Strategy = Treatment)
  if(missing(frame)) return(map)
  left_join(frame, map)
}

#' Label the number of current players
label_current_players <- function(frame) {
  frame %>% mutate(
    NumCurrentPlayers = ifelse(Strategy == "Diachronic", 1, NumPlayers)
  )
}

label_valid_teams <- function(frame) {
  valid_team_map <- label_valid_players() %>%
    group_by(Exp, TeamID) %>%
    summarize(IsTeamValid = all(IsPlayerValid)) %>%
    ungroup()

  if(missing(frame)) return(valid_team_map)
  left_join(frame, valid_team_map)
}
