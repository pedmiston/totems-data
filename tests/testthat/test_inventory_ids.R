context("Manipulate inventory ids")

test_that("unique results are appended and sorted", {
  result <- calculate_final_inventory_id("2", 1)
  expect_equal(result, "1-2")
})

test_that("duplicate results are ignored", {
  result <- calculate_final_inventory_id("1", 1)
  expect_equal(result, "1")
})

test_that("incorrect results are ignored", {
  result <- calculate_final_inventory_id("1", 0)
  expect_equal(result, "1")
})

context("Verify inventory ids")

data("PlayerPerformance")

test_that("0 doesn't appear in inventory ids", {
  inventory_ids <- PlayerPerformance$FinalInventoryID
  expect_true(!any(grepl("^0-", inventory_ids)))
})
