context("Verify isolated data")

test_that("isolated guesses are not duplicated", {
  data("Guesses")
  n_first_guesses <- Guesses %>%
    filter_selfother() %>%
    dplyr::filter(PlayerID == "P818", SessionID == "S818", NumSessionGuess == 1) %>%
    nrow()
  expect_equal(n_first_guesses, 1)
})
