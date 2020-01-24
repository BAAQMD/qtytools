context("tabulate_throughputs_by")

test_that("tabulate throughputs by year", {

  tabulated <-
    tabulate_throughputs_by(
      ems_and_tput,
      year)

  expected <-
    ems_and_tput %>%
    sum_throughputs_by(
      year) %>%
    spread(
      year,
      tput_qty) %>%
    dplyr::select(
      `1991`,
      `1992`,
      tput_unit)

  expect_equal(
    tabulated,
    expected)

})

