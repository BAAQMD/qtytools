context("annual_emissions_by")

test_that("ems_and_tput, pol_abbr", {
  result <- annual_emissions_by(ems_and_tput, pol_abbr)
  expected <- ems_by_year_and_pol
  expect_equal(result, expected)
})

test_that("including year, redundantly", {
  result <- annual_emissions_by(ems_and_tput, year, pol_abbr)
  expected <- ems_by_year_and_pol
  expect_equal(result, expected)
})

test_that("signif = 2", {

  rounded <-
    ems_and_tput %>%
    annual_emissions_by(
      year,
      pol_abbr,
      signif = 2)

  # unrounded should fail
  expect_failure(
    expect_equal(
      ems_by_year_and_pol,
      rounded))

})

