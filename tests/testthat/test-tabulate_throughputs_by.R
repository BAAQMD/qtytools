context("tabulate_throughputs_by")

test_that("tabulate throughputs by year", {

  tabulated <- tabulate_throughputs_by(ems_and_tput, year)
  expected <- data_frame(tput_unit = "MMscf", `1991` = 230, `1992` = 112)
  expect_equal(tabulated, expected)

})

