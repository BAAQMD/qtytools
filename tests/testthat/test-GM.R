context("GM")

test_that("atomic", {
  x <- runif(1, min = 2, max = 3)
  expect_equal(GM(x), x)
})

test_that("NA", {
  x <- c(1, 4, 1/32, NA)
  expect_identical(GM(x, na.rm = FALSE), NaN)
  expect_identical(GM(x, na.rm = TRUE), 1/2)
})

test_that("NULL", {
  x <- NULL
  expect_identical(GM(x), NULL)
})

test_that("zero(s)", {
  x <- c(0, 1, 2, 3)
  expect_identical(GM(x), NaN)
})

test_that("known", {
  x <- c(4, 1, 1/32)
  expect_equal(GM(x), 1/2)
})
