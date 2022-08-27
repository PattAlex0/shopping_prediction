#' Returns a cumulative proportion table rounded to two digits
#'
#' @param df The dataframe
#' @examples
#' tab_cum(shoppers$Administrative)

tab_cum <- function(df) {

    ## Create table
    tab <- table(df)

    ## Convert to proportional table
    tab_p <- prop.table(tab)

    ## Round to 2 digits
    tab_p <- round(tab_p, 2)

    ## Convery to cumulative table
    tab_c <- cumsum(tab_p)

    ## Return Table
    return(tab_c)
}
