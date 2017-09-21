library(tidyverse)

context("Guess generation from time variables")

test_that("generation is guessed correctly for diachronic teams", {
  times <- data_frame(Time = c(0, 25*60, 25*60+1), Strategy = "Diachronic")
  result <- guess_generation(times, time_col = "Time")
  expect_equal(result$Generation, c(1, 1, 2))
})
