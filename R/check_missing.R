#' Functions to check (non-)missingness.
#'
#' @param x A vector.
#' @param df A data frame.
#'
#' @importFrom purrr map_int
#' @export
#'

not_na <- function(x) !is.na(x)

#' @export
count_na <- function(x) {
    if (is.vector(x)) {
        sum(is.na(x))
    } else if (is.data.frame(x)) {
        purrr::map_int(x, ~ sum(is.na(.)))
    }
}
