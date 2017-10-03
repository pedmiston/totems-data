data_guesses <- function(con) {
  Guesses <- WorkshopAnalyzed %>%
    rename(Guess = WorkShopString, Result = WorkShopResult) %>%
    left_join(PlayerInfo) %>%
    count_guesses("PlayerID", "GuessNum") %>%
    count_guesses("TeamID", "TeamGuessNum") %>%
    select(
      PlayerID,
      PlayerTime, TeamTime,
      GuessNum, TeamGuessNum,
      Guess, Result,
      Score, TeamScore,
      UniqueGuess, TeamUniqueGuess,
      UniqueItem, TeamUniqueItem
    )
}
