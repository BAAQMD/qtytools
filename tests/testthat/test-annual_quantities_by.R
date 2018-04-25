context("annual_quantities_by")

test_that("no groups", {

  expect_equal(
    ems_and_tput_by_year,
    annual_quantities_by(ems_and_tput))

  expect_equal(
    annual_quantities_by(ems_and_tput),
    total_quantities_by(ems_and_tput, year))

})

test_that("grouped by pol_abbr", {

  expect_equal(
    ems_and_tput_by_year_and_pol,
    annual_quantities_by(ems_and_tput, pol_abbr))

  expect_equal(
    annual_quantities_by(ems_and_tput, pol_abbr),
    total_quantities_by(ems_and_tput, year, pol_abbr))

})

