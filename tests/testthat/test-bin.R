context("bin")

test_that("bin (numeric)", {

  expect_equal(
    bin(0:8, by = 3),
    c(0, 0, 0, 3, 3, 3, 6, 6, 6))

})
