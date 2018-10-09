context("Players")

test_that("Isolated 50 minute participants have Generation == 1", {
  e <- new.env()
  data("Sessions", envir = e)
  I50 <- e$Sessions %>%
    dplyr::filter(Strategy == "Isolated", SessionDuration == 50)

  expect_true(all(I50$Generation == 1))
})

test_that("TTP participants have been relabeled as Diachronic", {
  e <- new.env()
  data("Sessions", envir = e)
  ttp_subj <- e$Sessions %>%
    filter(SessionID == "S854")
  expect_equal(ttp_subj$Strategy, "Diachronic")
})
