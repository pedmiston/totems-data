context("Calculate time")

test_that("synchronic team time is calculated based on players per session", {
  Guesses <- data_frame(
    Strategy = "Synchronic",
    SessionTime = c(1, 5, 10),
    PlayersPerSession = c(1, 2, 4)
  )

  result <- label_team_time(Guesses)
  expect_equal(result$TeamTime, c(1, 10, 40))
})

test_that("isolated player time is calculated based on session ix", {
  Guesses <- data_frame(
    Strategy = "Isolated",
    SessionDuration = 25,
    SessionIX = c(1, 2),
    SessionTime = c(1, 1)
  )

  result <- label_player_time(Guesses)
  expect_equal(result$PlayerTime, c(1, 26))
})

test_that("diachronic/isolated team time is calculated based on generation", {
  Guesses <- data_frame(
    Strategy = c("Diachronic", "Isolated"),
    Generation = 2,
    SessionDuration = 25,
    SessionTime = c(1, 1)
  )

  result <- label_team_time(Guesses)
  expect_equal(result$TeamTime, c(26, 26))
})

