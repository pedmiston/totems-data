context("Sample trials")

starting_inventory <- data_frame(
  NumGuesses = 0,
  InventoryID = "1-2-3-4-5-6",
  InventorySize = 6,
  GuessesHash = digest::digest(""),
  NumUniqueGuesses = 0
)

test_that("sample trials works", {
  times <- c(5, 10)
  rows <- findInterval(c(6, 7, 11, 12), times)
  expect_equal(rows, c(1, 1, 2, 2))
})

test_that("sample session works", {
  session <- data_frame(
    SessionID = "S1",
    SessionDuration = 5,
    SessionTime = seq(0, 5, length.out = 100),
    NumSessionGuess = 1:100,
    PrevSessionInventoryID = "x",
    SessionInventorySize = 10,
    PrevSessionGuessesHash = "y",
    NumUniqueSessionGuesses = 10
  )
  result <- sample_session(session, default = starting_inventory)
  expect_equal(nrow(result), 6)
})

test_that("sample session handles missing trials", {
  session <- data_frame(
    SessionID = "S1",
    SessionDuration = 5,
    SessionTime = seq(2, 5, length.out = 100),
    NumSessionGuess = 1:100,
    PrevSessionInventoryID = "x",
    SessionInventorySize = 10,
    PrevSessionGuessesHash = "y",
    NumUniqueSessionGuesses = 10
  )

  starting_inventory <- data_frame(
    NumGuesses = 0,
    InventoryID = "1-2-3-4-5-6",
    InventorySize = 6,
    GuessesHash = digest::digest(""),
    NumUniqueGuesses = 0
  )

  result <- sample_session(session, default = starting_inventory)
  expect_equal(nrow(result), 6)
})

context("Verify sampled inventory sizes")

data("Sampled")

test_that("sampled guesses are a single row per sampled time", {
  samples_per_time <- count(Sampled, SessionID, SessionTime)
  expect_true(all(samples_per_time$n == 1))
})
