context("amo_target")

test_that("Less Than 30 returns NA", {
    expect_true(all(is.na(amo_target(0:29, runif(30)))))
})

test_that("Double AMO target works two ways", {
    expect_equal(
        amo_reduction(n = 50, pct = 20.7, double = TRUE),
        amo_reduction_double(n = 50, pct = 20.7)
    )
})

test_that("Target does not exceed 100 or go below 0", {
    expect_equal(amo_reduction(n = 50, pct = 0), 0)
    expect_equal(amo_reduction_double(n = 50, pct = 0), 0)
    expect_equal(amo_target(n = 50, pct = 100), 100)
})
