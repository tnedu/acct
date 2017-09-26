context("round5")

test_that("Round works", {
  expect_equal(round5(.5 + 0:4), 1:5)
})

test_that("Round diff works", {
  expect_equal(round5(67.8 - 67.8/4, 1), 50.9)
})