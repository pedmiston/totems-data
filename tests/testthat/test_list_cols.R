context("Save and load tibbles with list columns")

test_that("saving a tibble with a list column and reloading it works", {
  contents <- list("a", "b", "c")
  orig <- dplyr::tibble(a = 1:3) # list col can't be created in constructor
  orig$b <- contents
  dst <- tempfile(fileext = ".rda")
  save(orig, file = dst)
  rm(orig)
  load(dst)
  expect_equal(orig$b, contents)
})
