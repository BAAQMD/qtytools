context("lerp")

test_that("lerp (numeric y, missing x)", {

  expect_equal(
    lerp(sales),
    c(20, 30, 40, 50, 60))

})

test_that("lerp (numeric y, numeric x)", {

  result <-
    lerp(sales, elide_year(year), nm = TRUE)

  expected <-
    set_names(
      c(20, 25, 40, 45, 60),
      as.character(elide_year(year)))

  expect_equal(
    result,
    expected,
    tol = 0.001)

})

test_that("lerp (numeric y, numeric x, nm = FALSE)", {

  result <-
    lerp(sales, elide_year(year), nm = FALSE)

  expected <-
    c(20, 25, 40, 45, 60)

  expect_equal(
    result,
    expected,
    tol = 0.001)

})

test_that("lerp (numeric y, numeric x, numeric xout)", {

  xout <- elide_year(CY(1991:1999))

  result <-
    lerp(sales, elide_year(year), xout = xout, nm = TRUE)

  expected <-
    set_names(
      c(20, 25, 30, 35, 40, 45, 50, 55, 60),
      as.character(xout))

  expect_equal(
    result,
    expected,
    tol = 0.001)

})

test_that("lerp (numeric y, numeric x, numeric xout, extrapolate = TRUE)", {

  xout <- elide_year(CY(1990:2002))

  result <-
    lerp(sales, elide_year(year), xout = xout, nm = TRUE, extrapolate = TRUE)

  expected <-
    set_names(
      c(20, 20, 25, 30, 35, 40, 45, 50, 55, 60, 60, 60, 60),
      as.character(xout))

  expect_equal(
    result,
    expected,
    tol = 0.001)

})


test_that("lerp (formula, without explicit data)", {

  result <-
    lerp(
      sales ~ elide_year(year),
      nm = TRUE)

  expected <-
    set_names(
      c(20, 25, 40, 45, 60),
      as.character(year))

  expect_equal(
    result,
    expected,
    tol = 0.001)

})

test_that("lerp (formula, with explict data)", {

  result <-
    lerp(
      sales ~ year,
      tibble(sales, year),
      nm = TRUE)

  expected <-
    set_names(
      c(20, 25, 40, 45, 60),
      as.character(year))

  expect_equal(
    result,
    expected,
    tol = 0.001)

})
