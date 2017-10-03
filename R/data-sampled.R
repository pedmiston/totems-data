data_sampled <- function(con) {
  SampledPerformance <- Guesses %>%
    left_join(PlayerInfo) %>%
    calculate_rolling_team_performance() %>%
    calculate_rolling_player_performance() %>%
    group_by(PlayerID) %>%
    # Sample closest trial every 60 seconds
    do({ get_closest_trials_to_times(., times = seq(0, 50 * 60, by = 60)) }) %>%
    ungroup() %>%
    # Prevent Synchronic teams from being sampled outside their range.
    filter(!(Strategy == "Synchronic" & SampledTime > 25*60)) %>%
    # Calculate number of guesses in each time bin
    group_by(PlayerID) %>%
    mutate(
      NewGuesses = GuessNum - lag(GuessNum),
      NewTeamGuesses = TeamGuessNum - lag(TeamGuessNum)
    ) %>%
    ungroup() %>%
    group_by(PlayerID, SampledTime) %>%
    summarize(
      # Take max() of cumsum variables, not mean()
      NumInnovations = max(NumInnovations),
      NumTeamInnovations = max(NumTeamInnovations),
      Score = max(Score),
      TeamScore = max(TeamScore),
      # Since time step is 60 seconds,
      GuessesPerMinute = max(NewGuesses),
      TeamGuessesPerMinute = max(NewTeamGuesses)
    ) %>%
    ungroup() %>%
    left_join(PlayerInfo) %>%
    mutate(
      SampledPlayerTime = ifelse(Strategy != "Diachronic", SampledTime,
                                 SampledTime - (Generation - 1) * (25 * 60))
    ) %>%
    select(
      PlayerID, SampledTime, SampledPlayerTime,
      NumInnovations, NumTeamInnovations,
      Score, TeamScore,
      GuessesPerMinute, TeamGuessesPerMinute
    )
}

calculate_rolling_team_performance <- . %>%
  group_by(TeamID) %>%
  arrange(TeamTime) %>%
  mutate(
    NumTeamInnovations = cumsum(TeamUniqueItem),
    TeamScore = cumsum(Score)
  ) %>%
  ungroup()

calculate_rolling_player_performance <- . %>%
  group_by(PlayerID) %>%
  arrange(TeamTime) %>%
  mutate(
    NumInnovations = cumsum(UniqueItem),
    Score = cumsum(Score)
  ) %>%
  ungroup()
