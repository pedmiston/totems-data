context("Verify math for counting possible guesses")

test_that("counting permutations with replacement is correct", {
  r <- count_unique_permutations(1)
  expect_equal(r, 4)

  # 1, 1-1, 1-1-1, 1-1-1-1
  # 2, 1-2, 2-2, 1-1-2, 1-2-2, 2-2-2, ...
  r <- count_unique_permutations(2)
  expect_equal(r, 30)
})

test_that("counting guesses is correct", {
  r <- count_unique_combinations(1)
  expect_equal(r, 4)

  r <- count_unique_combinations(2)
  # 1, 1-1, 1-1-1, 1-1-1-1
  # 2, 2-2, 2-2-2, 2-2-2-2
  # 1-2, 1-1-2, 1-1-1-2
  # 1-2-2, 1-2-2-2, 1-1-2-2
  expect_equal(r, 14)
})
