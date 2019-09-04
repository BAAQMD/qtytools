context("locf")

test_that("locf (numeric)", {

  x <- c(0, NA, 1, 2.3, NA, NA)
  expected <- c(0, 0, 1, 2.3, 2.3, 2.3)
  expect_equal(locf(x), expected)

})

#'-----------------------------------------------------------------------------

test_that("locf (units)", {

  x <- as_units(c(0, NA, 1, 2.3, NA, NA), "lb")
  expected <- as_units(c(0, 0, 1, 2.3, 2.3, 2.3), "lb")
  expect_equal(locf(x), expected)

})
