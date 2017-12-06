context("Verify DiachronicPerformance data")

data("DiachronicPerformance")

test_that("Gen 1 Diachronic players have equal player and team guesses", {
  # Test: Gen 1 Diachronic players have equal player and team guesses
  gen1_guess_type_count_uniqueness <- DiachronicPerformance %>%
    dplyr::filter(Generation == 1) %>%
    arrange(SessionID, GuessType, GuessesRel) %>%
    select(SessionID, GuessType, GuessesRel, NumGuesses) %>%
    group_by(SessionID, GuessType) %>%
    summarize(NumUniqueNumbers = length(unique(NumGuesses))) %>%
    .$NumUniqueNumbers
  expect_true(all(gen1_guess_type_count_uniqueness == 1))
})

data("DiachronicPerformanceByStage")

test_that("Gen 1 Diachronic players have no time in the learning stage", {
  gen1_learning_stage <- DiachronicPerformanceByStage %>%
    dplyr::filter(Generation == 1, Stage == "learning")
  expect_equal(nrow(gen1_learning_stage), 0)
})
