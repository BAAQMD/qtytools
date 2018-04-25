context("int")

test_that("range", {
  expect_identical(
    int(1:5),
    c(1L, 2L, 3L, 4L, 5L))
})

test_that("mixed args", {
  expect_identical(
    int(1, 3:5),
    c(1L, 3L, 4L, 5L))
})

test_that("factor", {
  expect_identical(
    int(as.factor(c(foo = 2, bar = 99))),
    c(foo = 2L, bar = 99L))
})

test_that("named args", {
  expect_identical(
    int(ALA = 1, CC = 2),
    c(ALA = 1L, CC = 2L))
})
