#' Creates a new binary variable based on the value of an existing variable
#'
#' @param df The data frame
#' @param var_old The exisiting variable
#' @param cond The condition/s of the exisitng variable to base the new variable on
#' @examples
#' shoppers$February <- var_binary(shoppers, "Month", "February")


var_binary <- function(df, var_old, cond) {

    ## Create column with all zeros
    df$var_new <- 0

    ## Assign 1 to all rows which meet condition/s
    for(i in cond) {
        df$var_new[df[,c(var_old)] == i] <- 1
    }
    
    ## Return new column
    return(df$var_new)

}
