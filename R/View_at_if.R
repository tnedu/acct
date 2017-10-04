#' View a subset of a data frame.
#'
#' @param df A data frame.
#' @param ... An unquoted set of variables, passed to dplyr::select.
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' View_at(iris, Species, Sepal.Length, Petal.Length)
#'

View_at <- function(df, ...) {
    dplyr::select(df, ...) %>%
        View()
}

#' View a subset of rows from a data frame.
#'
#' @param df A data frame.
#' @param ... A logical condition, passed to dplyr::filter. Multiple conditions are combined with &.
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' View_if(iris, Species == "setosa")

View_if <- function(df, ...) {
    dplyr::filter(df, ...) %>%
        View()
}
