#' Create a barchart, grouped by chosen column and using the fill position
#'
#' @param df The data
#' @param var The column
#' @param tit the Graph's title
#' @examples
#' viz_fill(shoppers, Month, "Revenue Percentage by Month")

viz_fill <- function(df, col, tit) {

    ## Create graph
    viz <- df %>%
        count({{col}}, Revenue) %>%
        group_by({{col}}) %>%
        mutate(freq = n / sum(n)) %>%
        ggplot() +
        aes({{col}}, freq, fill = Revenue) +
        geom_col(position = "fill") +
        labs(x = NULL, y = NULL,
             title = tit,
             caption = "Data: Sakar, C.O., Polat, S.O., Katircioglu, M. et al. Neural Comput & Applic (2018).")

    ## Return graph
    return(viz)
}
