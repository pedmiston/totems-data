context("Sample trials")

test_that("sample trials works", {
  times <- c(5, 10)
  rows <- findInterval(c(6, 7, 11, 12), times)
  expect_equal(rows, c(1, 1, 2, 2))
})

test_that("sample session works", {
  session <- data_frame(
    SessionID = "S1",
    SessionDuration = 5,
    SessionTime = seq(1, 5, length.out = 100)
  )
  result <- sample_session(session)
  expect_true("SampledSessionTime" %in% colnames(result))
  expect_equal(nrow(result), 5)
})

test_that("sample session handles missing trials", {
  session <- data_frame(
    SessionID = "S1",
    SessionDuration = 5,
    SessionTime = seq(2, 5, length.out = 102)
  )
  result <- sample_session(session)
  expect_equal(nrow(result), 5)
})

test_that("sample session retains additional columns", {
  session <- data_frame(
    SessionID = "S1",
    SessionDuration = 5,
    SessionTime = seq(1, 5, length.out = 102),
    MyExtraVar = "a"
  )
  result <- sample_session(session)
  expect_equal(session$MyExtraVar[[1]], "a")
})
