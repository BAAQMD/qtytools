context("convert_quantities")

test_that("unit masses", {

  expect_equal(convert_quantities(1, "ton", "lb"), 2000)
  expect_equal(convert_quantities(1, "kg", "lb"), 2.2046, tol = 0.0001)

})

test_that("unit durations", {

  expect_equal(convert_quantities(1, "yr", "day"), 365.24, tol = 0.0001)

})

test_that("examples", {

  expect_equal(convert_quantities(1, "ton", "lbs"), 2000)
  expect_equal(convert_quantities(100, "tons/day", "lbs/yr"), 73048440)
  expect_equal(convert_quantities(1, "yard^3", "ft^3"), 27)
  expect_equal(convert_quantities(212, "degF", "degC"), 100)

})

test_that("random mass rates", {

  x <- runif(10, 0, 1000)

  expect_equal(
    convert_quantities(x, "kg/hr", "ton/yr"),
    x * 9.663, tol = 0.0001)

})

