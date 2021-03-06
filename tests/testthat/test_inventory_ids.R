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

test_that("I50 players have all inventory types equal", {
  data("Guesses")
  I50 <- dplyr::filter(Guesses, Strategy == "Isolated", SessionDuration == 50)
  expect_true(all(
    with(I50,
          (PrevSessionInventoryID == PrevTeamInventoryID)
         )
  ))
})
