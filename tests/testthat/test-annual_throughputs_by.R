context("annual_throughputs_by")

test_that("tput_only, no groups", {
  result <- annual_throughputs_by(tput_only, verbose = TRUE)
  expected <- tput_by_year
  expect_equal(result, expected)
})

test_that("including year, redundantly", {
  result <- annual_throughputs_by(tput_only, year, verbose = TRUE)
  expected <- tput_by_year
  expect_equal(result, expected)
})
