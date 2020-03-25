context("locf")

test_that("locf (Date)", {

  x <- as.Date(c("2001-02-03", NA, "2006-07-09", "1993-09-05", NA, NA))
  expected <- as.Date(c("2001-02-03", "2001-02-03", "2006-07-09", "1993-09-05", "1993-09-05", "1993-09-05"))
  expect_equal(locf(x), expected)

})

test_that("locf (character)", {

  x <- c("foo", NA, "bar", "baz", NA, NA)
  expected <- c("foo", "foo", "bar", "baz", "baz", "baz")
  expect_equal(locf(x), expected)

})

test_that("locf (numeric)", {

  x <- c(0, NA, 1, 2.3, NA, NA)
  expected <- c(0, 0, 1, 2.3, 2.3, 2.3)
  expect_equal(locf(x), expected)

})

test_that("locf (units)", {

  x <- as_units(c(0, NA, 1, 2.3, NA, NA), "lb")
  expected <- as_units(c(0, 0, 1, 2.3, 2.3, 2.3), "lb")
  expect_equal(locf(x), expected)

})

test_that("locf vs carry_forward", {

  x <- c(1, NA, 2, 3, NA, 5, 6, NA)

  expect_identical(
    expect_warning(carry_forward(x), "Deprecated"),
    locf(x))

  x <- c(NA, NA, NA, 1, NA, 2, 3, NA, 5, 6, NA)

  expect_identical(
    expect_warning(carry_forward(x), "Deprecated"),
    locf(x))

})
