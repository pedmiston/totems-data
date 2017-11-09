context("Verify guess counts")

data("Guesses")
data("PlayerPerformance")

filter_valid <- . %>% dplyr::filter(TeamStatus == "V")
Guesses %<>% filter_valid()
PlayerPerformance %<>% filter_valid()

test_that("session guesses are counted correctly", {
  session_guesses <- Guesses %>%
    group_by(Exp, SessionID) %>%
    summarize(
      NumGuesses = n(),
      MaxNumSessionGuesses = max(NumSessionGuess)
    ) %>%
    ungroup()

  expect_equal(session_guesses$NumGuesses, session_guesses$MaxNumSessionGuesses)

  session_guesses %<>% left_join(
    select(PlayerPerformance, Exp, SessionID, NumPlayerGuesses = NumGuesses)
  )

  expect_equal(session_guesses$NumGuesses, session_guesses$NumPlayerGuesses)
})
