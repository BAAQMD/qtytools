context("top_q")

test_that("mtcars", {

  top_mpg <- top_q(mtcars, q = 0.9, wt = mpg)
  expect_equal(pull(top_mpg, mpg), c(32.4, 30.4, 33.9, 30.4))

})
