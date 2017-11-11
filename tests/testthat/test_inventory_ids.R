context("Manipulate inventory ids")

test_that("unique results are appended and sorted", {
  result <- calculate_final_inventory_id("2", 1)
  expect_equal(result, "1-2")
})

test_that("duplicate results are ignored", {
  result <- calculate_final_inventory_id("1", 1)
  expect_equal(result, "1")
})
