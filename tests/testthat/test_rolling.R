context("Rolling vars")

test_that("rolling accumulates char items", {
  given <- c("a", "b", "c")
  expected <- list(NA, "a", c("a", "b"))
  result <- rolling(given)
  expect_equal(result, expected)
})

test_that("rolling accumulates unique char items", {
  given <- c("a", "b", "b", "c")
  expected <- list(NA, "a", c("a", "b"), c("a", "b"))
  result <- rolling(given)
  expect_equal(result, expected)
})

test_that("rolling accumulates int items", {
  given <- 1:4
  expected <- list(NA, 1, 1:2, 1:3)
  result <- rolling(given)
  expect_equal(result, expected)
})

test_that("rolling accumulates unique int items", {
  given <- c(1, 2, 2, 3)
  expected <- list(NA, 1, 1:2, 1:2)
  result <- rolling(given)
  expect_equal(result, expected)
})

context("Rolling guesses")

Guesses <- data_frame(
  GuessNum = c(1, 2, 3, 1, 2),
  PlayerID = c("P1", "P1", "P1", "P2", "P2"),
  Guess = c("a", "b", "c", "a", "a")
)

test_that("guesses can be rolled by player", {
  expected <- list(NA, "a", c("a", "b"), NA, "a")
  result <- rolling_player_guesses(Guesses)
  expect_equal(result$PrevGuesses, expected)
})

test_that("guess uniqueness is labeled", {
  expected <- c(T, T, T, T, F)
  result <- label_unique_guesses(Guesses)
  expect_equal(result$UniqueGuess, expected)
})
