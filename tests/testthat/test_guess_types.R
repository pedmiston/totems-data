context("Verify guess types")

data("Guesses")

test_that("there are no redundant guesses that produce unique results", {
  ImpossibleSessionGuesses <- Guesses %>%
    dplyr::filter(UniqueSessionGuess == 0, UniqueSessionResult == 1)
  expect_equal(nrow(ImpossibleSessionGuesses), 0)

  ImpossibleTeamGuesses <- Guesses %>%
    dplyr::filter(UniqueTeamGuess == 0, UniqueTeamResult == 1)
  expect_equal(nrow(ImpossibleTeamGuesses), 0)
})

test_that("all unique items result from unique guesses", {
  UniqueSessionItems <- Guesses %>%
    dplyr::filter(UniqueSessionResult == 1)
  expect_true(all(UniqueSessionItems$UniqueSessionGuess == 1))

  UniqueTeamItems <- Guesses %>%
    dplyr::filter(UniqueTeamResult == 1)
  expect_true(all(UniqueTeamItems$UniqueTeamGuess == 1))
})
