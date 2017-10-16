#' Label the experiment of the observations in frame.
#'
#' Data for Generation = 1 and Generation = 2 Diachronic players is
#' duplicated: once for the 50 labor minutes comparison,
#' and once for the 100 labor minutes comparison.
#'
#' Data for SessionIX = 1 and SessionIX = 2 Isolated players
#' with Duration = 25 is similarly duplicated.
#'
#' **This means statistics cannot be collapsed across experiments.**
#'
#' @param frame Requires columns TeamID, PlayerID, SessionIX, and Generation.
#' @return The same data as in frame, but with a new column Exp.
label_player_experiment <- function(frame) {
  diachronic_50 <- data_frame(
    Strategy = "Diachronic",
    Exp = "50LaborMinutes",
    TeamSize = 2,
    SessionSize = 1,
    Duration = 25,
    SessionIX = 1,
    PlayerIX = 1:2,
    Generation = 1:2
  )

  diachronic_100 <- data_frame(
    Strategy = "Diachronic",
    Exp = "100LaborMinutes",
    TeamSize = 4,
    SessionSize = 1,
    Duration = 25,
    SessionIX = 1,
    PlayerIX = 1:4,
    Generation = 1:4
  )

  synchronic_50 <- data_frame(
    Strategy = "Synchronic",
    Exp = "50LaborMinutes",
    TeamSize = 2,
    SessionSize = 1,
    Duration = 25,
    SessionIX = 1,
    PlayerIX = 1:2,
    Generation = 1
  )

  synchronic_100 <- data_frame(
    Strategy = "Synchronic",
    Exp = "100LaborMinutes",
    TeamSize = 4,
    SessionSize = 1,
    Duration = 25,
    SessionIX = 1,
    PlayerIX = 1:4,
    Generation = 1
  )

  isolated_50 <- data_frame(
    Strategy = "Isolated",
    Exp = "50LaborMinutes",
    TeamSize = 1,
    SessionSize = c(1, 2, 2),
    Duration = c(50, 25, 25),
    SessionIX = c(1, 1, 2),
    PlayerIX = 1,
    Generation = c(1, 1, 2)
  )

  isolated_100 <- data_frame(
    Strategy = "Isolated",
    Exp = "100LaborMinutes",
    TeamSize = 1,
    SessionSize = 4,
    Duration = 25,
    SessionIX = 1:4,
    PlayerIX = 1,
    Generation = 1:4
  )

  experiment_map <- bind_rows(
    diachronic_50,
    diachronic_100,
    synchronic_50,
    synchronic_100,
    isolated_50,
    isolated_100
  )

  if(missing(frame)) return(experiment_map)
  inner_join(frame, experiment_map)
}

label_team_experiment <- function(frame) {
  experiment_map <- label_player_experiment() %>%
    select(Strategy, Duration, Exp, TeamSize, SessionSize) %>%
    unique()
  if(missing(frame)) return(experiment_map)
  inner_join(frame, experiment_map)
}
