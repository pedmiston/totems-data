context("Accumulate vars")

test_that("accumulate char items", {
  given <- c("a", "b", "c")
  expected <- list(NA, "a", c("a", "b"))
  result <- accumulate(given)
  expect_equal(result, expected)
})

test_that("accumulate unique char items", {
  given <- c("a", "b", "b", "c")
  expected <- list(NA, "a", c("a", "b"), c("a", "b"))
  result <- accumulate(given)
  expect_equal(result, expected)
})

test_that("accumulate int items", {
  given <- 1:4
  expected <- list(NA, 1, 1:2, 1:3)
  result <- accumulate(given)
  expect_equal(result, expected)
})

test_that("accumulate unique int items", {
  given <- c(1, 2, 2, 3)
  expected <- list(NA, 1, 1:2, 1:2)
  result <- accumulate(given)
  expect_equal(result, expected)
})

test_that("accumulate with default int items", {
  default <- 1:4
  given <- c(5, 5, 6)
  expected <- list(1:4, 1:5, 1:5)
  result <- accumulate(given, default = default)
  expect_equal(result, expected)
})

context("Accumulate session")

Guesses <- data_frame(
  SessionID = 1,
  SessionTime = 1:2,
  Guess = 1:2,
  Result = 7:8
)

test_that("results incorporate default items", {
  expected <- list(1:6, 1:7, 1:8, 1:6, 1:7)
  result <- accumulate_session(Guesses)
  expect_equal(result$PrevSessionResults, expected)
})
