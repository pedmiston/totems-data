context("Label experiment")

test_that("labeling experiment duplicates diachronic and isolated players", {
  experiment_map <- label_experiment() %>%
    label_strategy()

  full_diachronic_team <- experiment_map %>%
    dplyr::filter(Strategy == "Diachronic", Exp == "100LaborMinutes") %>%
    group_by(TeamID) %>%
    summarize(TeamSize = n()) %>%
    dplyr::filter(TeamSize == 4) %>%
    head(n = 1) %>%
    .$TeamID
  diachronic_player <- dplyr::filter(experiment_map, TeamID == full_diachronic_team) %>%
    dplyr::filter(Exp == "100LaborMinutes") %>%
    select(TeamID, PlayerID, SessionIX, Generation)
  expect_equal(nrow(diachronic_player), 4)
  diachronic_result <- label_experiment(diachronic_player)
  expect_equal(nrow(diachronic_result), 6)

  player_with_all_isolated_sessions <- experiment_map %>%
    dplyr::filter(Strategy == "Isolated", Exp == "100LaborMinutes") %>%
    group_by(PlayerID) %>%
    summarize(NumSessions = n()) %>%
    dplyr::filter(NumSessions == 4) %>%
    head(n = 1) %>%
    .$PlayerID
  isolated_player <- dplyr::filter(experiment_map, PlayerID == player_with_all_isolated_sessions) %>%
    dplyr::filter(Exp == "100LaborMinutes") %>%
    select(TeamID, PlayerID, SessionIX, Generation)
  expect_equal(nrow(isolated_player), 4)
  isolated_result <- label_experiment(isolated_player)
  expect_equal(nrow(isolated_result), 6)
})
