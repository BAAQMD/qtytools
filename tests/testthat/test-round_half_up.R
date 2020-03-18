test_that("0.5 rounds to 1", {
  expect_equal(round_half_up(0.5), 1)
})

test_that("1.5 rounds to 2", {
  expect_equal(round_half_up(1.5), 2)
})

test_that("2.5 rounds to 3", {
  expect_equal(round_half_up(2.5), 3)
})

test_that("0.15 rounds to 0.2", {
  expect_equal(round_half_up(0.15, digits = 1), 0.2)
})

test_that("0.25 rounds to 0.3", {
  expect_equal(round_half_up(0.25, digits = 1), 0.3)
})

test_that("pi (various digits)", {
  expect_equal(round_half_up(pi, digits = 0), 3)
  expect_equal(round_half_up(pi, digits = 1), 3.1)
  expect_equal(round_half_up(pi, digits = 2), 3.14)
  expect_equal(round_half_up(pi, digits = 3), 3.142)
  expect_equal(round_half_up(pi, digits = 4), 3.1416)
})
