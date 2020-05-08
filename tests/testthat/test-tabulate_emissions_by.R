context("tabulate_emissions_by")

test_that("tabulate emissions by pol_abbr", {

  tabulated <-
    tabulate_emissions_by(
      ems_and_tput,
      pol_abbr)

  expected <-
    ems_and_tput %>%
    sum_emissions_by(
      pol_abbr) %>%
    tidyr::spread(
      pol_abbr,
      ems_qty) %>%
    dplyr::select(
      PM,
      TOG,
      ems_unit)

  expect_equal(
    tabulated,
    expected)

})
