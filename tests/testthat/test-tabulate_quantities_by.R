context("tabulate_quantities_by")

test_that("tabulate_quantities_by (no grouping)", {

  tabulated <-
    tput_only %>%
    tabulate_quantities_by(
      year)

  expected <-
    tput_only %>%
    sum_quantities_by(
      year) %>%
    spread(
      year,
      tput_qty) %>%
    select(
      `1991`,
      `1992`,
      tput_unit)

  expect_equal(
    tabulated,
    expected)

})

test_that("S3 classes", {

  tabulated <-
    tput_only %>%
    tabulate_quantities_by(year)

  expect_s3_class(
    tabulated, class(tput_only))

  expect_s3_class(
    tabulated, "tabulation")

})
