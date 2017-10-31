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

test_that("accumulate ignores special items", {
  default <- 1
  given <- c(2, 0, 3)
  expected <- list(1, 1:2, 1:2)
  result <- accumulate(given, default = default, ignore = 0)
  expect_equal(result, expected)
})

context("Assign ids")

test_that("assign_ids works for results of accumulate", {
  given <- 2:4
  accumulated <- accumulate(given, default = 1)
  ids <- assign_ids(accumulated)
  expect_equal(ids, c("1", "1-2", "1-2-3"))
})

test_that("assign_ids assigns the empty string for NA inventories", {
  expect_equal(assign_ids(accumulate(NA)), "")
})

context("Assign hashes")

test_that("assign_hashes is vectorized", {
  given <- 2:4
  accumulated <- accumulate(given, default = 1)
  hashes <- assign_hashes(accumulated)
  expected <- assign_ids(accumulated) %>%
    purrr::map(function(id) digest::digest(id)) %>%
    unlist()
  expect_equal(hashes, expected)
})

context("Accumulate session")

Guesses <- data_frame(
  SessionID = 1,
  SessionTime = 1:2,
  Guess = 1:2,
  Result = 7:8
)

test_that("results incorporate default items", {
  expected <- list(1:6, 1:7)
  result <- accumulate_session(Guesses)
  expect_equal(result$PrevSessionInventory, expected)
})
