#' Count distinct combinations of a set of variables
#'
#' @param df A data frame.
#' @param ... An unquoted set of variables, passed to dplyr::select.
#'
#' @importFrom dplyr select distinct
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' count_distinct(iris, Species)

count_distinct <- function(df, ...) {
    df %>%
        dplyr::select(...) %>%
        dplyr::distinct() %>%
        nrow()
}
