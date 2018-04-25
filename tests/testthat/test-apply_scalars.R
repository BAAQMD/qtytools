context("apply_scalars")

county_estimates <-
  test_category_totals %>%
  scale_emissions_by(test_population_fractions, key_col = "cnty_abbr")

test_that("simple case", {

  expect_equal_to_reference(
    county_estimates,
    here::here("tests", "testthat", "expected-apply_scalars-county_estimates.Rds"))

})

test_that("with key_col", {

  projected_emissions <-
    county_estimates %>%
    filter(year == "CY2000") %>%
    rename(base_year = year) %>%
    scale_emissions_by(test_population_growth, key_col = "year")

  expect_equal_to_reference(
    projected_emissions,
    here::here("tests", "testthat", "expected-apply_scalars-projected_emissions.Rds"))

})


