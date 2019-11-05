context("sum_emissions_by")

test_that("sum throughputs by pol_abbr", {

  input_data <-
    ems_and_tput %>%
    mutate(
      cf_qty = runif(nrow(.)))

  summed <-
    sum_emissions_by(
      input_data,
      pol_abbr)

  expect_equal(
    summed, ems_by_pol)

})

