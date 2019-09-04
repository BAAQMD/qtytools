context("MMTCO2eq")

test_that("MMTCO2eq", {

  library(units)

  x <- as_units(1, "MMTCO2eq")

  expect_equal(
    set_units(x, "TCO2eq"),
    as_units(1e6, "TCO2eq"))

})
