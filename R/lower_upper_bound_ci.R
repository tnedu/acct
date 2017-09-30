#' Calculate Upper/Lower Bound Confidence Intervals for Accountability
#'
#' @param n A count (valid tests, enrolled, etc.).
#' @param pct A percentage (Percent On Track/Mastered, Chronic Absenteeism, etc.).
#'
#' @export
#'
#' @examples
#'
#' ci_upper_bound(n = 10, pct = 50)
#' ci_lower_bound(n = 10, pct = 50)
#'
ci_lower_bound <- function(n, pct) {
    pct <- pct/100

    round5(100 * n/(n + qnorm(0.975)^2) * (pct + (qnorm(0.975)^2/(2 * n)) -
        qnorm(0.975) * sqrt((pct * (1 - pct))/n + qnorm(0.975)^2/(4 * n^2))), 1)
}

#' @export
ci_upper_bound <- function(n, pct) {
    pct <- pct/100

    round5(100 * n/(n + qnorm(0.975)^2) * (pct + (qnorm(0.975)^2/(2 * n)) +
        qnorm(0.975) * sqrt((pct * (1 - pct))/n + qnorm(0.975)^2/(4 * n^2))), 1)
}
