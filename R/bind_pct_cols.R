#' Calculates Percentages and Adds them to the end of the data frame
#'
#' This function takes a dataframe and a list of columns to find a percentage for, along with a single column to use
#' in the denominator. It returns a dataframe
#' @param df Dataframe. Used first to be friendly to the \%>\% operator.
#' @param numerator_cols A character vector of columns to be used in the numerator.
#' @param denom_col A single string of the column name to be used in the denominator.
#' @return Returns a dataframe with the pct columns at the end. The columns will be named
#' the original column name with a 'pct_' prefix
#' @keywords percent, percentage, calculate
#' @export
#' @examples
#' bind_pct_cols(system_level_df, c('on_track', 'mastered'), 'valid_tests')

bind_pct_cols <- function(df, numerator_cols, denom_col){
    out_df <- df %>%
        mutate_at(
            .vars = numerator_cols,
            .funs = list(pct = ~ round(. / (!!as.name(denom_col)) * 100 + 1e-10, 1))
        ) %>%
        # pct is added as a suffix. Renaming columns so that pct is a prefix
        rename_at( vars( contains( "_pct") ), list( ~paste("pct", gsub("_pct|n_", "", .), sep = "_") ) )
    return(out_df)
}
