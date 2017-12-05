#' Filter observations for the diachronic comparison.
#' @export
filter_diachronic_exp <- function(frame) {
  frame %>% dplyr::filter(Strategy == "Diachronic", SessionStatus == "valid")
}

#' Filter observations for the 50 labor minute comparison.
#' @export
filter_50min_exp <- function(frame) {
  frame %>%
    dplyr::filter(
      SessionStatus == "valid",
      (Strategy == "Diachronic" & Generation <= 2),
      (Strategy == "Synchronic" & PlayersPerSession == 2),
      (Strategy == "Isolated" & SessionDuration == 50)
    )
}

#' Filter observations for the inheritance experiment.
#' @export
filter_inheritance_exp <- function(frame) {
  frame %>%
    dplyr::filter(
      SessionStatus == "valid",
      Strategy != "Synchronic",
      SessionDuration == 25
    )
}

#' Filter observations for the 100 labor minute experiment.
#' @export
filter_100min_exp <- function(frame) {
  frame %>%
    dplyr::filter(
      SessionStatus == "valid",
      (Strategy == "Synchronic" & PlayersPerSession == 4),
      (Strategy == "Isolated" & SessionDuration == 25)
    )
}

player_conditions <- function() {
  # 50 Labor Minutes
  diachronic_50 <- data_frame(
    Strategy = "Diachronic",
    Exp = "50LaborMinutes",
    NumPlayers = 2,
    SessionsPerPlayer = 1,
    PlayersPerSession = 1,
    SessionDuration = 25,
    PlayerIX = 1:2,
    SessionIX = 1,
    Generation = 1:2
  )

  synchronic_50 <- data_frame(
    Strategy = "Synchronic",
    Exp = "50LaborMinutes",
    NumPlayers = 2,
    SessionsPerPlayer = 1,
    PlayersPerSession = 2,
    SessionDuration = 25,
    PlayerIX = 1:2,
    SessionIX = 1,
    Generation = 1
  )

  isolated_50 <- data_frame(
    Strategy = "Isolated",
    Exp = "50LaborMinutes",
    NumPlayers = 1,
    SessionsPerPlayer = 1,
    PlayersPerSession = 1,
    SessionDuration = 50,
    PlayerIX = 1,
    SessionIX = 1,
    Generation = 1
  )

  diachronic_100 <- data_frame(
    Strategy = "Diachronic",
    Exp = "InheritanceType",
    NumPlayers = 4,
    SessionsPerPlayer = 1,
    PlayersPerSession = 1,
    SessionDuration = 25,
    PlayerIX = 1:4,
    SessionIX = 1,
    Generation = 1:4
  )

  isolated_100 <- data_frame(
    Strategy = "Isolated",
    Exp = "InheritanceType",
    NumPlayers = 1,
    SessionsPerPlayer = 4,
    PlayersPerSession = 1,
    SessionDuration = 25,
    PlayerIX = 1,
    SessionIX = 1:4,
    Generation = 1:4
  )

  # 100 Labor Minutes
  synchronic_100 <- data_frame(
    Strategy = "Synchronic",
    Exp = "100LaborMinutes",
    NumPlayers = 4,
    SessionsPerPlayer = 1,
    PlayersPerSession = 4,
    SessionDuration = 25,
    PlayerIX = 1:4,
    SessionIX = 1,
    Generation = 1
  )

  diachronic_100 <- data_frame(
    Strategy = "Diachronic",
    Exp = "100LaborMinutes",
    NumPlayers = 4,
    SessionsPerPlayer = 1,
    PlayersPerSession = 1,
    SessionDuration = 25,
    PlayerIX = 1:4,
    SessionIX = 1,
    Generation = 1:4
  )

  isolated_100 <- data_frame(
    Strategy = "Isolated",
    Exp = "100LaborMinutes",
    NumPlayers = 1,
    SessionsPerPlayer = 4,
    PlayersPerSession = 1,
    SessionDuration = 25,
    PlayerIX = 1,
    SessionIX = 1:4,
    Generation = 1:4
  )

  bind_rows(
    diachronic_50,
    synchronic_50,
    isolated_50,
    diachronic_100,
    synchronic_100,
    isolated_100
  ) %>%
    select(Strategy, Exp, NumPlayers, SessionsPerPlayer, PlayersPerSession, SessionDuration,
           PlayerIX, SessionIX, Generation)
}

#' Label the experimental condiitions of the observations in frame.
#'
#' Data for Generation = 1 and Generation = 2 Diachronic players is
#' duplicated for the 50/100 labor minute comparisons.
#' Data for SessionIX = 1 and SessionIX = 2 Isolated players
#' with SessionDuration = 25 is similarly duplicated.
#'
#' **This means statistics cannot be collapsed across experiments.**
#'
#' @param frame Expects columns Strategy, PlayerIX, SessionIX, and Generation.
#' @return The same data as in frame, but with new columns for experimental
#'         conditions. See \code{\link{player_conditions}}.
label_player_conditions <- function(frame) {
  inner_join(frame, player_conditions())
}

#' Label the team conditions.
team_conditions <- function() {
  player_conditions() %>%
    select(-(PlayerIX:Generation)) %>%
    unique()
}

label_team_conditions <- function(frame) {
  inner_join(frame, team_conditions())
}

#' Recode Exp as ExpLabel, an ordered factor with pretty labels.
#' @import dplyr
#' @export
recode_experiment <- function(frame) {
  levels <- c("50LaborMinutes", "100LaborMinutes")
  labels <- c("50 Labor Minutes", "100 Labor Minutes")
  map <- data_frame(
    Exp = levels,
    ExpLabel = factor(levels, levels = levels, labels = labels)
  )
  if(missing(frame)) return(map)
  left_join(frame, map)
}
