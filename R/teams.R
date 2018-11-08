#' Replace ID_Group with TeamID.
#'
#' TeamID is a deidentified version of ID_Group
#' with datetime information removed.
replace_id_group <- function(frame) {
  id_groups <- read_table("Table_Group") %>%
    mutate(Datetime = parse_date_time_from_id_group(ID_Group)) %>%
    arrange(Datetime) %>%
    .$ID_Group
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

#' Label the number of players in a session based on Strategy and Size.
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

#' Extract datetime from ID_Group
parse_date_time_from_id_group <- function(id_group) {
  id_group <- stringr::str_replace(id_group, "^G_", "") %>%
    stringr::str_replace("-\\d*$", "")
  lubridate::parse_date_time(id_group, "m/d/y H:M:S Op!*", tz = "America/Chicago")
}
