context("tabulate_emissions_by")

test_that("tabulate emissions by pol_abbr", {

  tabulated <- tabulate_emissions_by(ems_and_tput, pol_abbr)
  expected <- data_frame(ems_unit = "tons/yr", PM = 212, TOG = 270)
  expect_equal(tabulated, expected)

})
