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
