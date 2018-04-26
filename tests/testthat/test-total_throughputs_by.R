context("total_throughputs_by")

test_that("total throughputs by year", {

  input_data <- ems_and_tput %>% mutate(cf_qty = runif(nrow(.)))
  totaled <- total_throughputs_by(input_data, year)
  expect_equal(totaled, tput_by_year)

})
