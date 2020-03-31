context("percent-class")

x <- c(0.25, 0.5, 0.75)

test_that("constructor works", {
  expect_silent(
    percent(x))
})

test_that("arithmetic works", {

  pct_obj <- percent(x)
  expect_equal(
    pct_obj + pct_obj,
    percent(x + x))

})

test_that("can't combine with character", {
  expect_error(vec_c(1, "a"), class = "vctrs_error_incompatible_type")
})
