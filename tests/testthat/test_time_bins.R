library(tidyverse)
library(lubridate)
library(magrittr)

context("Get closest trial to a particular time")

test_that("correct trial is selected", {
  trials <- data_frame(TrialID = c(0, 1), TeamTime = c(0.5, 1.1))
  trial <- get_closest_trial_to_time(1, trials)
  expect_equal(trial$TrialID, 0)
})

test_that("missing trial returns without blowing up", {
  trials <- data_frame(TrialID = c(0, 1), TeamTime = c(0.5, 1.1))
  trial <- get_closest_trial_to_time(0.4, trials)
  expect_equal(trial$TrialID, numeric(0))
})

test_that("sample times are returned in the result", {
  trials <- data_frame(TrialID = c(0, 1), TeamTime = c(0.5, 1.1))
  trial <- get_closest_trial_to_time(1.09, trials)
  expect_equal(trial$SampledTime, 1.09)
})

test_that("trials are sampled for multiple times", {
  trials <- data_frame(TrialID = c(0, 1, 2), TeamTime = c(0.5, 1.1, 1.5))
  trial <- get_closest_trials_to_times(trials, times = c(1, 1.2, 1.6))
  expect_equal(trial$TrialID, c(0, 1, 2))
})
