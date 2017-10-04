data_team_performance <- function() {
  Guesses <- data_guesses()
  TeamPerformance <- Guesses %>%
    filter_complete_teams() %>%
    label_team_size() %>%
    group_by(TeamID, TeamSize) %>%
    summarize(
      Score = sum(Score),
      NumInnovations = sum(TeamUniqueItem),
      NumGuesses = max(TeamGuessNum),
      NumUniqueGuesses = sum(TeamUniqueGuess)
    )
  TeamPerformance
}

label_team_size <- function(guesses) {
  diachronic <- filter(guesses, Strategy == "Diachronic") %>%
    clone_diachronic_teams()

  synchronic <- guesses %>%
    left_join(data_teams()[,c("TeamID", "TeamSize")]) %>%
    filter(Strategy == "Synchronic")

  isolated <- guesses %>%
    left_join(data_teams()[,c("TeamID", "SessionDuration")]) %>%
    filter(Strategy == "Isolated") %>%
    mutate(TeamSize = ifelse(SessionDuration == 50, 2, 4)) %>%
    select(-SessionDuration)

  bind_rows(
    diachronic,
    synchronic,
    isolated
  )
}

#' Duplicate guesses to allow summarizing diachronic team
#' performance at TeamSize == 2 and TeamSize == 4.
clone_diachronic_teams <- function(diachronic) {
  diachronic2 <- filter(diachronic, Generation <= 2) %>%
    mutate(TeamSize = 2)
  diachronic4 <- diachronic %>%
    mutate(TeamSize = 4)
  bind_rows(diachronic2, diachronic4)
}

data_player_performance <- function() {
  Guesses <- data_guesses()
  PlayerPerformance <- Guesses %>%
    group_by(PlayerID) %>%
    summarize(
      NumInnovations = sum(UniqueItem),
      NumGuesses = max(GuessNum),
      NumUniqueGuesses = sum(UniqueGuess),
      NumTeamUniqueGuesses = sum(TeamUniqueGuess)
    ) %>%
    # Add in Score from Player table
    left_join(select(Player, PlayerID, Score))
}

filter_complete_teams <- function(frame) {
  frame
}
