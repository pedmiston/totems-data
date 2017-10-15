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

#' Determine TeamSize from Strategy and Exp.
#'
#' TeamSize for all Isolated teams is 1.
#' Diachronic and Synchronic teams are size 2 or 4
#' depending on the experiment.
label_team_size <- function(frame) {
  map <- data_frame(
    Strategy = rep(c("Isolated", "Diachronic", "Synchronic"), each = 2),
    Exp = rep(c("50LaborMinutes", "100LaborMinutes"), times = 3),
    TeamSize = c(1, 1, 2, 4, 2, 4)
  )
  if(missing(frame)) return(map)
  left_join(frame, map)
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
    NumCurrentPlayers = ifelse(Strategy == "Diachronic", 1, TeamSize)
  )
}
