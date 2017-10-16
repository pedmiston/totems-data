context("Label experiment")

test_that("experiment map duplicates diachronic players", {
  frame <- data_frame(
    Strategy = "Diachronic",
    Duration = 25,
    Generation = 1,
    PlayerIX = 1,
    SessionIX = 1
  )
  result <- label_player_experiment(frame)
  expect_equal(nrow(result), 2)
})
