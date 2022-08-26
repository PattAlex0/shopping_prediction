#' Create a barchart, grouped by Revenue and using the dodge position
#'
#' @param df The data
#' @param var The column
#' @param tit the Graph's title
#' @examples
#' viz_dodge(shoppers, Month, "Sessions by Month and Revenue")

viz_dodge <- function(df, var, tit) {
  
  ### Clean data
  viz <- df %>%
    count({{var}}, Revenue) %>%
    group_by(Revenue) %>%
    mutate(freq = n / sum(n)) %>%
    ggplot() +
    aes({{var}}, freq, fill = Revenue) +
    geom_col(position = "dodge") +
    labs(x = NULL, y = NULL, 
         title = tit,
         caption = "Data: Sakar, C.O., Polat, S.O., Katircioglu, M. et al. Neural Comput & Applic (2018).")


    ## Return data
    return(viz)
    
}
