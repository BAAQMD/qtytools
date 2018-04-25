context("total_emissions_by")

test_that("total throughputs by pol_abbr", {
  expect_equal_data(ems_by_pol, ems_and_tput %>% total_emissions_by(pol_abbr))
})

