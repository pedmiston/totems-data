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

test_that("rolling accumulates with default int items", {
  default <- 1:4
  given <- c(5, 5, 6)
  expected <- list(1:4, 1:5, 1:5)
  result <- rolling(given, default = default)
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

context("Rolling inventory")

Inventories <- data_frame(
  Result = c(7, 8, 9, 7, 7),
  PlayerID = c("P1", "P1", "P1", "P2", "P2"),
  GuessNum = c(1, 2, 3, 1, 2)
)

test_that("inventories incorporate default", {
  expected <- list(1:6, 1:7, 1:8, 1:6, 1:7)
  result <- rolling_player_inventory(Inventories)
  expect_equal(result$PrevInventory, expected)
})

test_that("item uniqueness is labeled", {
  expected <- c(T, T, T, T, F)
  result <- label_unique_items(Inventories)
  expect_equal(result$UniqueItem, expected)
})
