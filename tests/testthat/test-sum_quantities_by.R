context("sum_quantities_by")

test_that("sum quantities by year", {
  # See helper-quantities.R for definitions of `tput_only` and `tput_by_year`
  result <- sum_quantities_by(tput_only, year)
  expected <- tput_by_year
  expect_equal(result, expected)
})

test_that("sum quantities by pol_abbr", {
  # See helper-quantities.R for definitions of `ems_only` and `ems_by_pol`
  result <- sum_quantities_by(ems_only, pol_abbr)
  expected <- ems_by_pol
  expect_equal(result, expected)
})

test_that("signif = 2", {

  unrounded <-
    ems_only %>%
    sum_quantities_by(
      pol_abbr)

  rounded <-
    ems_only %>%
    sum_quantities_by(
      pol_abbr,
      signif = 2)

  # should not be equal
  expect_failure(
    expect_equal(
      unrounded,
      rounded))

  # manual rounding should generate the same result
  unrounded %>%
    mutate_at(
      vars(ems_qty),
      ~ base::signif(., digits = 2)) %>%
    expect_equal(
      rounded)

})

test_that("digits = -1", {

  unrounded <-
    ems_only %>%
    sum_quantities_by(
      pol_abbr)

  rounded <-
    ems_only %>%
    sum_quantities_by(
      pol_abbr,
      digits = -1)

  # should not be equal
  expect_failure(
    expect_equal(
      unrounded,
      rounded))

  # manual rounding should generate the same result
  unrounded %>%
    mutate_at(
      vars(ems_qty),
      ~ base::round(., digits = -1)) %>%
    expect_equal(
      rounded)

})

