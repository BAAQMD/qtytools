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

test_that("digits = -1", {

  unrounded <-
    ems_only %>%
    sum_emissions_by(
      pol_abbr)

  rounded <-
    ems_only %>%
    sum_emissions_by(
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

