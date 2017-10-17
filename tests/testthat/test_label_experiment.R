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
    SessionIX = 1
  )
  result <- label_player_conditions(frame)
  expect_equal(nrow(result), 1)
})
