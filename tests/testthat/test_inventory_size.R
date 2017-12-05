context("Verify inventory size")

data("Guesses")

Guesses <- Guesses %>%
  dplyr::filter(SessionStatus == "valid")

test_that("sum of unique session items always equals inventory size", {
  num_unique_session_results <- Guesses %>%
    group_by(SessionID) %>%
    summarize(SumUniqueSessionResults = sum(UniqueSessionResult))

  inventory_sizes <- Guesses %>%
    group_by(SessionID) %>%
    summarize(MaxSessionInventorySize = max(SessionInventorySize))

  result <- merge(num_unique_session_results, inventory_sizes)
  expect_equal(result$SumUniqueSessionResults,
               result$MaxSessionInventorySize - 6)
})


