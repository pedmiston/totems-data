context("Label generation")

test_that("labeling generation doesn't add rows", {
  guesses <- data_frame(
    Strategy = c("Diachronic", "Synchronic"),
    PlayerIX = 2,
    SessionIX = 1
  )
  result <- label_generation(guesses)
  expect_equal(nrow(result), 2)
  expect_equal(result$Generation, c(2, 1))
})
