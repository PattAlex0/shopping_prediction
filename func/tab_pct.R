#' Creates a table of percentages for a variable
#'
#' @param df The dataframe
#' @examples
#' tab_pct(shoppers$Revenue)

tab_pct <- function(df) {

    ## Create table
    tab <- table(df)

    ## Create Proportional Table
    tab <- prop.table(tab)

    ## Convert Proportions to Percentages
    tab <- tab * 100

    ## Round values to two digits
    tab <- round(tab, 2)

    ## Return final table
    return(tab)

}
