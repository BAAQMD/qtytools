context("carry_forward")

test_that("carry_forward is deprecated", {

  expect_warning(
    expect_identical(
      carry_forward(c(1, NA, 2, 3, NA, 5, 6, NA)),
      c(1, 1, 2, 3, 3, 5, 6, 6)),
    "Deprecated")

  expect_warning(
    expect_identical(
      carry_forward(c(NA, NA, NA, 1, NA, 2, 3, NA, 5, 6, NA)),
      c(NA, NA, NA, 1, 1, 2, 3, 3, 5, 6, 6)),
    "Deprecated")

})
