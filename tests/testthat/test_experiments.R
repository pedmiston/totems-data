context("Label conditions")

test_that("labeling player conditions duplicates diachronic players", {
  frame <- data_frame(
    Strategy = "Diachronic",
    SessionDuration = 25,
    Generation = 1,
    PlayerIX = 1,
    SessionIX = 1
  )
  result <- label_player_conditions(frame)
  expect_equal(nrow(result), 2)
})

test_that("labeling player conditions does not duplicate synchronic players", {
  frame <- data_frame(
    Strategy = "Synchronic",
    PlayerIX = 1,
    SessionIX = 1,
    PlayersPerSession = 2
  )
  result <- label_player_conditions(frame)
  expect_equal(nrow(result), 1)
})

test_that("labeling player conditions duplicates guesses correctly", {
  guesses <- data_frame(
    Guess = 1,
    Result = 1,
    Strategy = c("Synchronic", "Diachronic", "Isolated"),
    PlayersPerSession = c(2, 1, 1),
    SessionDuration = c(25, 25, 50),
    Generation = c(1, 1, 1),
    PlayerIX = 1,
    SessionIX = 1
  )
  results <- label_player_conditions(guesses)
  expect_equal(nrow(results), nrow(guesses) + 1)
  expect_equal(results$Exp, c("50LaborMinutes", "50LaborMinutes", "100LaborMinutes", "50LaborMinutes"))
})
