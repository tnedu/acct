#' Count Observations in a Data Frame Meeting a Condition
#'
#' @param df A data frame.
#' @param ... A logical condition. Multiple conditions are combined with &.
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' count_if(iris, Species == "setosa")

count_if <- function(df, ...) {
    dplyr::filter(df, ...) %>%
        nrow()
}
