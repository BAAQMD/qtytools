context("total_emissions_by")

test_that("total throughputs by pol_abbr", {
  input_data <- ems_and_tput %>% mutate(cf_qty = runif(nrow(.)))
  totaled <- total_emissions_by(input_data, pol_abbr)
  expect_equal(totaled, ems_by_pol)
})

