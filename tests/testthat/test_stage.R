context("Label stage")

guesses <- data_frame(SessionID = "S",
                      SessionTime = 1:3,
                      PrevTeamInventoryID = "1",
                      PrevSessionInventoryID = c("", "1", "1-2"))

test_that("label diachronic stage", {
  r <- guesses %>% mutate(Strategy = "Diachronic") %>% label_stage()
  expect_equal(r$Stage, c("learning", "playing", "playing"))
})

test_that("label synchronic stage", {
  r <- guesses %>% mutate(Strategy = "Synchronic") %>% label_stage()
  expect_equal(r$Stage, c("learning", "playing", "playing"))
})

context("Verify stage")

test_that("I50 players are only in stage 'playing'", {
  data("Guesses")
  I50 <- dplyr::filter(Guesses, Strategy == "Isolated", SessionDuration == 50)
  expect_true(all(I50$Stage == "playing"))
})

context("Label stage time")

test_that("stage time is centered on stage ix", {
  result <- guesses %>% label_stage() %>% label_stage_time()
  expect_equal(result$StageTime, c(0, 1, 2))
})
