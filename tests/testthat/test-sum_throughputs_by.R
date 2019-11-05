context("sum_throughputs_by")

test_that("sum throughputs by year", {

  input_data <- ems_and_tput %>% mutate(cf_qty = runif(nrow(.)))
  summed <- sum_throughputs_by(input_data, year)
  expect_equal(summed, tput_by_year)

})
