data_team_performance <- function() {
  Guesses <- data_guesses()
  TeamPerformance <- Guesses %>%
    label_experiment() %>%
    filter_valid_players() %>%
    group_by(Exp, TeamID) %>%
    summarize(
      TeamScore = sum(TeamScore),
      TeamGuesses = max(TeamGuessNum),
      UniqueTeamGuesses = max(NumUniqueTeamGuesses),
      UniqueTeamItems = max(NumUniqueTeamItems)
    ) %>%
    join_teams()
  TeamPerformance
}

data_player_performance <- function() {
  Guesses <- data_guesses()
  PlayerPerformance <- Guesses %>%
    label_experiment() %>%
    filter_valid_players() %>%
    group_by(Exp, PlayerID) %>%
    summarize(
      Score = sum(Score),
      NumGuesses = max(GuessNum),
      UniqueGuesses = max(NumUniqueGuesses),
      UniqueItems = max(NumUniqueItems)
    ) %>%
    join_players() %>%
    join_teams()
}

label_experiment <- function(frame) {
  diachronic <- filter(frame, Strategy == "Diachronic")
  diachronic2 <- filter(diachronic, Generation <= 2) %>%
    mutate(
      TeamSize = 2,
      Exp = "50LaborMinutes"
    )
  diachronic4 <- diachronic %>%
    mutate(
      TeamSize = 4,
      Exp = "100LaborMinutes"
    )

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

  bind_rows(
    diachronic2,
    diachronic4,
    synchronic,
    isolated
  )
}

filter_valid_players <- function(frame) {
  frame
}
