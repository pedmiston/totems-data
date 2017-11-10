context("Verify guess types")

filter_valid_teams <- . %>% dplyr::filter(TeamStatus == "V")

data("Guesses")
Guesses %<>% filter_valid_teams()

test_that("there are no redundant guesses that produce unique results", {
  ImpossibleSessionGuesses <- Guesses %>%
    dplyr::filter(UniqueSessionGuess == 0, UniqueSessionResult == 1)
  expect_equal(nrow(ImpossibleSessionGuesses), 0)

  ImpossiblePlayerGuesses <- Guesses %>%
    dplyr::filter(UniquePlayerGuess == 0, UniquePlayerResult == 1)
  expect_equal(nrow(ImpossiblePlayerGuesses), 0)

  ImpossibleTeamGuesses <- Guesses %>%
    dplyr::filter(UniqueTeamGuess == 0, UniqueTeamResult == 1)
  expect_equal(nrow(ImpossibleTeamGuesses), 0)
})

test_that("all unique items result from unique guesses", {
  UniqueSessionItems <- Guesses %>%
    dplyr::filter(UniqueSessionResult == 1)
  expect_true(all(UniqueSessionItems$UniqueSessionGuess == 1))

  UniquePlayerItems <- Guesses %>%
    dplyr::filter(UniquePlayerResult == 1)
  expect_true(all(UniquePlayerItems$UniquePlayerGuess == 1))

  UniqueTeamItems <- Guesses %>%
    dplyr::filter(UniqueTeamResult == 1)
  expect_true(all(UniqueTeamItems$UniqueTeamGuess == 1))
})

data("TeamPerformance")
TeamPerformance %<>% filter_valid_teams()

data("PlayerPerformance")
PlayerPerformance %<>% filter_valid_teams()

test_that("the sum of the four guess types equals num total guesses", {
  TeamPerformance <- TeamPerformance %>%
    mutate(NumGuesses2 = NumRedundantGuesses + NumRepeatedItems + NumUniqueGuesses + NumInnovations)

  expect_equal(TeamPerformance$NumGuesses, TeamPerformance$NumGuesses2)

  PlayerPerformance <- PlayerPerformance %>%
    mutate(NumGuesses2 = NumRedundantGuesses + NumRepeatedItems + NumUniqueGuesses + NumInnovations)

  expect_equal(PlayerPerformance$NumGuesses, PlayerPerformance$NumGuesses2)
})
