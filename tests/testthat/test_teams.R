context("Teams")

test_that("Correctly parse datetime out of ID_Group", {
  id_group <- "G_1/25/2017 1:32:16 PM"
  expected <- lubridate::ymd_hms("2017-01-25 13:32:16", tz = "America/Chicago")
  result <- parse_date_time_from_id_group(id_group)
  expect_equal(result, expected)
})

test_that("Parsing date time from ID_Group is vectorized", {
  id_group <- "G_1/25/2017 1:32:16 PM"
  expected <- lubridate::ymd_hms("2017-01-25 13:32:16", tz = "America/Chicago")
  result <- parse_date_time_from_id_group(rep(id_group, 2))
  expect_equal(result, rep(expected, 2))
})

test_that("Parse date time from TOT ID_Group with appended value", {
  id_group <- "G_1/25/2017 1:32:16 PM-100"
  expected <- lubridate::ymd_hms("2017-01-25 13:32:16", tz = "America/Chicago")
  result <- parse_date_time_from_id_group(id_group)
  expect_equal(result, expected)
})
