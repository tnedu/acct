#' Calculate AMO Targets
#'
#' Family of functions for calcuating AMO targets. Includes functions for
#' AMO reduction targets and double AMO targets.
#'
#' @param n A count (valid tests, enrolled, etc.).
#' @param pct A percentage (Percent On Track/Mastered, Chronic Absenteeism, etc.).
#' @param double Calculate double AMO target? Defaults to FALSE.
#'
#' @export
#'
#' @examples
#'
amo_target <- function(n, pct, double = FALSE) {
    if (!double) {
        denom <- 16
    } else {
        denom <- 8
    }

    ifelse(n >= 30, round(pct + (100 - pct)/denom, 1), NA_real_)
}

amo_reduction <- function(n, pct, double = FALSE) {
    if (!double) {
        denom <- 16
    } else {
        denom <- 8
    }

    ifelse(n >= 30, round5(pct - pct/denom, 1), NA_real_)
}

amo_reduction_double <- function(n, pct, double = FALSE) {
    if (!double) {
        denom <- 8
    } else {
        denom <- 4
    }

    ifelse(n >= 30, round5(pct - pct/denom, 1), NA_real_)
}
