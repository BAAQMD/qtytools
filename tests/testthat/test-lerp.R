context("lerp")

test_that("lerp (numeric y, missing x)", {

  expect_equal(
    lerp(sales),
    c(20, 30, 40, 50, 60))

})

test_that("lerp (numeric y, numeric x)", {

  result <-
    lerp(sales, year, nm = TRUE)

  expected <-
    set_names(
      c(20, 25, 40, 45, 60),
      as.character(year))

  expect_equal(
    result,
    expected)

})

test_that("lerp (numeric y, numeric x, nm = FALSE)", {

  result <-
    lerp(sales, year, nm = FALSE)

  expected <-
    c(20, 25, 40, 45, 60)

  expect_equal(
    result,
    expected)

})

test_that("lerp (numeric y, numeric x, numeric xout)", {

  xout <- 1991:1999

  result <-
    lerp(sales, year, xout = xout, nm = TRUE)

  expected <-
    set_names(
      c(20, 25, 30, 35, 40, 45, 50, 55, 60),
      as.character(xout))

  expect_equal(
    result,
    expected)

})

test_that("lerp (numeric y, numeric x, numeric xout, extrapolate = TRUE)", {

  xout <- 1990:2002

  result <-
    lerp(sales, year, xout = xout, nm = TRUE, extrapolate = TRUE)

  expected <-
    set_names(
      c(20, 20, 25, 30, 35, 40, 45, 50, 55, 60, 60, 60, 60),
      as.character(xout))

  expect_equal(
    result,
    expected)

})


test_that("lerp (formula, without explicit data)", {

  result <-
    lerp(
      sales ~ year,
      nm = TRUE)

  expected <-
    set_names(
      c(20, 25, 40, 45, 60),
      as.character(year))

  expect_equal(
    result,
    expected)

})

test_that("lerp (formula, with explict data)", {

  result <-
    lerp(
      sales ~ year,
      data.frame(sales = sales, year = year),
      nm = TRUE)

  expected <-
    set_names(
      c(20, 25, 40, 45, 60),
      as.character(year))

  expect_equal(
    result,
    expected)

})
