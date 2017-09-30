#' View a subset of a data frame.
#'
#' @param df A data frame.
#' @param ... An unquoted set of variables.
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#'     x = sample(LETTERS, 1000, replace = TRUE),
#'     y = rnorm(1000),
#'     z = rnorm(1000)
#' )
#'
#' View_at(df, x, z)
#'
View_at <- function(df, ...) {
    dplyr::select(df, ...) %>%
        View()
}

#' View a subset of rows from a data frame.
#'
#' @param df A data frame.
#' @param ... A logical condition.
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#'     x = sample(LETTERS, 1000, replace = TRUE),
#'     y = rnorm(1000),
#'     z = rnorm(1000)
#' )
#'
#' View_if(df, x == "A")
#'

View_if <- function(df, ...) {
    dplyr::filter(df, ...) %>%
        View()
}
