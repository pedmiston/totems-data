#' Label the experiment of the observations in frame.
#'
#' Gets an authoritative source of which teams
#' are valid from the Status column of the Group table.
#'
#' Data for Gen1 and Gen2 Diachronic players is
#' duplicated: once for the 50 labor minutes comparison,
#' and once for the 100 labor minutes comparison.
#' This means **do not** collapse any statistics
#' across Experiment.
#'
#' @param frame Requires columns TeamID and PlayerID.
#' @return The same data as in frame, but with a new column Exp.
label_experiment <- function(frame) {
  players <- data_players() %>%
    join_teams()

  diachronic <- filter(players, Strategy == "Diachronic")
  diachronic2 <- filter(diachronic, Generation <= 2) %>%
    mutate(TeamSize = 2, Exp = "50LaborMinutes")
  diachronic4 <- diachronic %>%
    mutate(TeamSize = 4, Exp = "100LaborMinutes")

  synchronic_exp_map <- data_frame(
    TeamSize = c(2, 4),
    Exp = c("50LaborMinutes", "100LaborMinutes")
  )
  synchronic <- filter(frame, Strategy == "Synchronic") %>%
    left_join(synchronic_exp_map)

  isolated_map <- data_players() %>%
    filter(Strategy == "Isolated") %>%
    mutate(Exp = ifelse(SessionDuration == 25, "50LaborMinutes", "100LaborMinutes")) %>%
    select(PlayerID, Exp)
  isolated <- filter(frame, Strategy == "Isolated") %>%
    left_join(isolated_map)

  experiment_map <- bind_rows(
    diachronic2,
    diachronic4,
    synchronic,
    isolated
  ) %>%
    select(TeamID, PlayerID, Exp)

  left_join(frame, experiment_map)
}
