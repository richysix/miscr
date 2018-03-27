#' get_gg_legend
#'
#' \code{get_gg_legend} returns the legend of a ggplot object as a grob
#'
#' @param ggplot_obj a ggplot object
#'
#' @return the legend as a grob
#'
#' @examples
#' plot_legend <- get_gg_legend(ggplot_obj)
#'
#' @export
get_gg_legend <- function(ggplot_obj){
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(ggplot_obj))
  leg <- which(sapply(tmp$grobs, function(x){ x$name }) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
