context("total_quantities_by")

test_that("total quantities by year", {
  # See helper-quantities.R for definitions of `tput_only` and `tput_by_year`
  result <- total_quantities_by(tput_only, year)
  expected <- tput_by_year
  expect_equal(result, expected)
})

test_that("total quantities by pol_abbr", {
  # See helper-quantities.R for definitions of `ems_only` and `ems_by_pol`
  result <- total_quantities_by(ems_only, pol_abbr)
  expected <- ems_by_pol
  expect_equal(result, expected)
})
