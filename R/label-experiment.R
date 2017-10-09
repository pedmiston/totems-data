#' Label the experiment of the observations in frame.
#'
#' Data for Gen1 and Gen2 Diachronic players is
#' duplicated: once for the 50 labor minutes comparison,
#' and once for the 100 labor minutes comparison.
#'
#' @import dplyr
#' @param frame Requires columns TeamID and PlayerID.
#' @return The same data as in frame, but with new columns Exp and TeamSize
label_experiment <- function(frame) {
  teams <- collect_tbl("Table_Group") %>%
    replace_id_group() %>%
    select(TeamID, Strategy = Treatment, Duration = BuildingTime, TeamSize = Size)

  players <- collect_tbl("Table_Player") %>%
    mutate(ID_Player = as.integer(ID_Player)) %>%
    replace_id_group() %>%
    replace_id_player() %>%
    left_join(teams) %>%
    replace_ancestor() %>%
    select(PlayerID, TeamID, Session, Strategy, Generation, TeamSize, Duration)

  diachronic <- dplyr::filter(players, Strategy == "Diachronic")
  diachronic2 <- dplyr::filter(diachronic, Generation <= 2) %>%
    mutate(TeamSize = 2, Exp = "50LaborMinutes")
  diachronic4 <- mutate(diachronic, TeamSize = 4, Exp = "100LaborMinutes")

  synchronic_exp_map <- data_frame(
    TeamSize = c(2, 4),
    Exp = c("50LaborMinutes", "100LaborMinutes")
  )
  synchronic <- dplyr::filter(players, Strategy == "Synchronic") %>%
    left_join(synchronic_exp_map)

  isolated <- dplyr::filter(players, Strategy == "Isolated")
  isolated2 <- bind_rows(
    dplyr::filter(isolated, Duration == 50),
    dplyr::filter(isolated, Duration == 25, Session <= 2)
  ) %>% mutate(Exp = "50LaborMinutes")
  isolated4 <- dplyr::filter(isolated, Duration == 25) %>%
    mutate(Exp = "100LaborMinutes")

  experiment_map <- bind_rows(
    diachronic2,
    diachronic4,
    synchronic,
    isolated2,
    isolated4
  ) %>%
    select(TeamID, PlayerID, Session, Exp)

  if(!("PlayerID" %in% colnames(frame))) {
    experiment_map <- experiment_map %>%
      select(TeamID, Exp) %>%
      unique()
  }

  merge(frame, experiment_map)
}
