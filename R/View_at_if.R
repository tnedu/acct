#' View a subset of a data frame.
#'
#' @param df A data frame.
#' @param ... A logical condition (View_if) or an unquoted set of variables (View_at).
#'
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
#' View_at(df, x, z)
#'
View_at <- function(df, ...) {
    dplyr::select(df, ...) %>%
        View()
}

View_if <- function(df, ...) {
    dplyr::filter(df, ...) %>%
        View()
}
