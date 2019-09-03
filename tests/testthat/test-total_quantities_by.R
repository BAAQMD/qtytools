context("total_quantities_by")

test_that("total quantities by year", {
  # See helper-quantities.R for definitions of `tput_only` and `tput_by_year`
  result <- total_quantities_by(tput_only, year)
  expected <- tput_by_year
  expect_equal(result, expected)
})

test_that("total quantities by pol_abbr", {
  # See helper-quantities.R for definitions of `ems_only` and `ems_by_pol`
  result <- total_quantities_by(ems_only, pol_abbr)
  expected <- ems_by_pol
  expect_equal(result, expected)
})

test_that("signif = 2", {

  rounded <-
    ems_only %>%
    total_quantities_by(
      pol_abbr,
      signif = 2)

  # unrounded should fail
  expect_failure(
    expect_equal(
      ems_only,
      rounded))

  # rounded should succeed
  expect_equal(
    rounded,
    tribble(
      ~ pol_abbr, ~ ems_qty, ~ ems_unit,
      "PM", 210, "tons/yr",
      "TOG", 270, "tons/yr"))

})

