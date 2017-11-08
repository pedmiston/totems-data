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
    SessionTime = seq(1, 5, length.out = 100),
    NumSessionGuess = 1:100,
    PrevSessionInventoryID = "x",
    SessionInventorySize = 10,
    PrevSessionGuessesHash = "y",
    NumUniqueSessionGuesses = 10
  )
  result <- sample_session(session)
  expect_equal(nrow(result), 5)
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
  expect_equal(nrow(result), 5)
})

context("Verify sampled inventory sizes")

data("Sampled")

Sampled <- Sampled %>%
  dplyr::filter(SessionStatus == "V")

test_that("sampled guesses are a single row per sampled time", {
  samples_per_time <- count(Sampled, Exp, SessionID, SessionTime)
  expect_true(all(samples_per_time$n == 1))
})

test_that("last sampled inventory size equals player performance", {
  example_team <- "G110"

  data("PlayerPerformance")
  G110Players <- PlayerPerformance %>%
    dplyr::filter(Exp == "100LaborMinutes", TeamID == example_team) %>%
    select(PlayerID, NumExpectedInnovations = NumInnovations)

  G110Sampled <- Sampled %>%
    dplyr::filter(Exp == "100LaborMinutes", TeamID == example_team) %>%
    group_by(PlayerID) %>%
    summarize(NumSampledInnovations = max(InventorySize) - 6)

  TestG110 <- left_join(G110Players, G110Sampled)

  expect_equal(TestG110$NumExpectedInnovations, TestG110$NumSampledInnovations)
})
