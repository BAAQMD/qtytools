context("total")

test_that("numeric", {
  expect_equal(
    total(1:10), 55)
})

test_that("numeric with NAs", {
  expect_equal(
    total(c(1:3, NA, 4:10)),
    55)
})

test_that("digits", {
  expect_equal(total(c(889.234, 534.678), digits = 2), 1423.91)
})

test_that("signif", {
  expect_equal(total(c(889.234, 534.678), signif = 2), 1400)
})

test_that("character", {
  expect_error(
    total(c("foo", "bar")),
    "must be numeric")
})

test_that("overflow", {
  x <- rep(.Machine$integer.max, 2)
  expect_message(
    total(x, verbose = TRUE),
    "WARNING")
})
