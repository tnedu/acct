#' Round Numbers
#'
#' Does not handle rounding of .5 in the usual R way.
#'
#' @param x A vector of numeric values.
#' @param digits Number of decimal places. Defaults to 0 (round to the whole number).
#'
#' @export
#'
#' @examples
#'
#' round(.5 + -2:4) # -2  0  0  2  2  4  4
#' round5(.5 + -2:4) # -2  -1  1  2  3  4  5
#'
round5 = function(x, digits = 0) {
    round(x + 1e-10, digits)
}
