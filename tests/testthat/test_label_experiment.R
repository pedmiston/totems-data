context("Label experiment")

test_that("experiment map duplicates diachronic players", {
  mock_experiment_map <- data_frame(
    PlayerID = 1,
    Exp = c("Exp1", "Exp2")
  )
  frame <- data_frame(PlayerID = 1, Trial = 1)
  result <- label_experiment(frame, mock_experiment_map)
  expect_equal(nrow(result), 2)
})
