context("Verify sampled inventory sizes")

data("Sampled")

Sampled <- Sampled %>%
  dplyr::filter(SessionStatus == "V")

test_that("no diff between mean and max inventory size for single players", {
  single_player_sessions <- Sampled %>%
    dplyr::filter(Strategy != "Synchronic")

  inventory_sizes <- single_player_sessions %>%
    group_by(Exp, SessionID, SessionTime) %>%
    summarize(
      MeanSessionInventorySize = mean(SessionInventorySize),
      MaxSessionInventorySize = max(SessionInventorySize)
    )

  expect_equal(
    inventory_sizes$MeanSessionInventorySize,
    inventory_sizes$MaxSessionInventorySize
  )
})
