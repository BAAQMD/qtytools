context("is_int")

test_that("truths", {

  x <- c(1L, -99L)
  expect_true(is_int(x))
  expect_true(is_int(as.integer(x)))

})

test_that("with NAs and Infs", {

  x <- c(1L, -99L, NA, Inf, -Inf)
  expect_true(is_int(x))

})

test_that("falsehoods", {

  expect_false(is_int(0.5))
  expect_false(is_int(0.9999999))
  expect_false(is_int("5"))

})
