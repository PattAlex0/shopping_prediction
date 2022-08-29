#' Identify the mode of a vector
#' Code take from: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
#'
#' @param df The vector
#' @examples
#' var_mode(shoppers$Administrative)

var_mode <- function(df) {

    ## Identify all unique values
    unq <- unique(df)

    ## Match to values
    mat <- match(df, unq)

    ## Tabulate the values
    tab <- tabulate(mat)

    ## Identify the maximum value
    mod <- which.max(tab)

    ## Return mode
    return(df[mod])

}
