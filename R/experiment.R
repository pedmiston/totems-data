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
label_experiment <- function(frame, map) {
  if(missing(map)) map <- make_experiment_map()
  in_common <- intersect(names(frame), names(map))
  map <- unique(map[,c(in_common, "Exp")])
  inner_join(frame, map)
}

make_experiment_map <- function() {
  teams <- read_table("Table_Group") %>%
    replace_id_group() %>%
    select(TeamID, Strategy = Treatment, Duration = BuildingTime, TeamSize = Size)

  players <- read_table("Table_Player") %>%
    replace_id_group() %>%
    replace_id_player() %>%
    left_join(teams) %>%
    replace_ancestor() %>%
    select(PlayerID, TeamID, SessionID, SessionIX, Strategy, Generation, TeamSize, Duration)

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
    dplyr::filter(isolated, Duration == 25, SessionIX <= 2)
  ) %>% mutate(Exp = "50LaborMinutes")
  isolated4 <- dplyr::filter(isolated, Duration == 25) %>%
    mutate(Exp = "100LaborMinutes")

  bind_rows(
    diachronic2,
    diachronic4,
    synchronic,
    isolated2,
    isolated4
  ) %>%
    select(TeamID, PlayerID, SessionID, SessionIX, Generation, Exp)
}
