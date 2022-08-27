#' Returns a cumulative percentage proportion table rounded to two digits
#'
#' @param df The dataframe
#' @examples
#' tab_cum(shoppers$Administrative)

tab_cum <- function(df) {

    ## Create table
    tab <- table(df)

    ## Convert to proportional table
    tab_p <- prop.table(tab)

    ## Convert to cumulative table
    tab_c <- cumsum(tab_p)

    ## Multiply by 100
    tab_c <- tab_c * 100

    ## Round to two digits
    tab_c <- round(tab_c, 2)

    ## Return Table
    return(tab_c)
}
