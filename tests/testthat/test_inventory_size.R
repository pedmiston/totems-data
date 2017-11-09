context("Verify inventory size")

data("Guesses")

Guesses <- Guesses %>%
  dplyr::filter(SessionStatus == "V")

test_that("sum of unique session items always equals inventory size", {
  num_unique_session_results <- Guesses %>%
    group_by(Exp, SessionID) %>%
    summarize(SumUniqueSessionResults = sum(UniqueSessionResult))

  inventory_sizes <- Guesses %>%
    group_by(Exp, SessionID) %>%
    summarize(MaxSessionInventorySize = max(SessionInventorySize))

  result <- merge(num_unique_session_results, inventory_sizes)
  expect_equal(result$SumUniqueSessionResults,
               result$MaxSessionInventorySize - 6)
})


