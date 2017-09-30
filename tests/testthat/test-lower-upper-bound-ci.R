context("lower_upper_bound_ci")

test_that("Warn when pct is not a percentage", {
    expect_warning(ci_lower_bound(n = 100, pct = 1000))
    expect_warning(ci_upper_bound(n = 100, pct = 1000))
})
