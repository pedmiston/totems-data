context("Sample trials")

test_that("sample trials works", {
  times <- c(5, 10)
  rows <- findInterval(c(6, 7, 11, 12), times)
  expect_equal(rows, c(1, 1, 2, 2))
})

test_that("sample session works", {
  session <- data_frame(
    SessionDuration = 5,
    SessionTime = seq(1, 5, by = 0.5)
  )
  result <- sample_session(session)
  expect_true("SampledSessionTime" %in% colnames(result))
})

test_that("sample session handles missing trials", {
  session <- data_frame(
    SessionDuration = 5,
    SessionTime = seq(2, 5, by = 0.5)
  )
  result <- sample_session(session)
  expect_equal(nrow(result), 4)
})
