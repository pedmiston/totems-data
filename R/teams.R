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

#' Label the number of current players based on Strategy and NumPlayers.
#'
#' For Synchronic players, PlayersPerSession == Size.
#' For Diachronic and Isolated players, PlayersPerSession == 1.
label_players_per_session <- function(frame) {
  players_per_session_map <- read_table("Table_Group") %>%
    replace_id_group() %>%
    rename(Strategy = Treatment) %>%
    mutate(PlayersPerSession = ifelse(Strategy == "Synchronic", Size, 1)) %>%
    select(TeamID, PlayersPerSession)
  if(missing(frame)) return(players_per_session_map)
  left_join(frame, players_per_session_map)
}

#' Label the duration of the session for players by TeamID.
label_session_duration <- function(frame) {
  session_durations <- read_table("Table_Group") %>%
    replace_id_group() %>%
    select(TeamID, SessionDuration = BuildingTime)
  if(missing(frame)) return(session_durations)
  left_join(frame, session_durations)
}

label_valid_teams <- function(frame) {
  valid_team_map <- label_valid_players() %>%
    group_by(Exp, TeamID) %>%
    summarize(IsTeamValid = all(IsPlayerValid)) %>%
    ungroup()

  if(missing(frame)) return(valid_team_map)
  left_join(frame, valid_team_map)
}
