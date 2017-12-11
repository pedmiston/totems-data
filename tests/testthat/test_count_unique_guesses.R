context("Verify math for counting possible guesses")

test_that("counting guesses with replacement is correct", {
  r <- count_unique_guesses_with_replacement(1)
  expect_equal(r, 4)

  # 1, 1-1, 1-1-1, 1-1-1-1
  # 2, 1-2, 2-2, 1-1-2, 1-2-2, 2-2-2, ...
  r <- count_unique_guesses_with_replacement(2)
  expect_equal(r, 30)
})

test_that("counting guesses is correct", {
  r <- count_unique_guesses(1)
  expect_equal(r, 1)

  r <- count_unique_guesses(2)
  # 1, 2, 1-2
  expect_equal(r, 3)
})
