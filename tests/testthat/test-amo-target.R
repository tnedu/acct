context("amo_target")

test_that("Less Than 30 returns NA", {
    expect_true(all(is.na(amo_target(0:29, runif(30)))))
})
