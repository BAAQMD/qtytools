context("total_throughputs_by")

test_that("total throughputs by year", {

  expect_equal(
    tput_by_year,
    total_throughputs_by(ems_and_tput, year))

})
