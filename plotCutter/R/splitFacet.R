#' Split multi-facet plot into individual plots
#'
#' This function allows you to split a multi-facet plot into individual plots.
#'
#'The function is built along these steps :
#'(1) it goes through the structure of the object to get the names of the variables used for faceting.
#'(2) it overwrites the facet element of the plot object with the one from the empty ggplot object (so if we print it at this stage facets are gone).
#'(3) it extracts the data and split it along the variables identified in the 1st step.
#'(4) it overwrites the original data with each subset and store all outputs in a list.
#'
#' @param x A multi-facet plot made from ggplot or other R packages
#' @param ... Other arguments passed on to methods. Not currently used.
#' @keywords plot
#' @export
#' @examples
#' # Generate some sample data, then create a multi-facet plot
#' df <- data.frame(x=seq(1,24,1), y=seq(1,24,1), z=rep(seq(1,12),each=2))
#' myplot <- ggplot(df,aes(x=x, y=y))+geom_point()+facet_wrap(~z)
#' myplot
#'
#' # Use the 'splitFacet()' function to split into individual plots
#' new_plots <- splitFacet(myplot)
#'
#' length(new_plots)
#' new_plots[[3]]
#'
#' @export
splitFacet <- function(x){
  facet_vars <- names(x$facet$params$facets)
  x$facet    <- ggplot2::ggplot()$facet
  datasets   <- split(x$data, x$data[facet_vars])
  new_plots  <- lapply(datasets,function(new_data) {
    x$data <- new_data
    x})
}
