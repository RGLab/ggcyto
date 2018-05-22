#' Add a flowJo biexponential scale to the x or y axes of a ggcyto plot.
#' 
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @param maxValue,widthBasis,pos,neg see 'help(flowJoTrans')
#' @param equal.space whether to display the breaks in equal.space format
#' @return ScaleContinuous object
#' @importFrom flowWorkspace flowJo_biexp_trans
#' @examples 
#' data(GvHD)
#' fr <- GvHD[[1]]
#' p <- ggcyto(fr, aes(x = `FL1-H`)) + geom_density()
#' #display at raw scale
#' p 
#' #display at transformed scale
#' p + scale_x_flowJo_biexp(maxValue = 1e4, widthBasis = 0)
#' @export
scale_x_flowJo_biexp <- function(..., maxValue = 262144, widthBasis = -10, pos = 4.5, neg = 0, equal.space = FALSE){
  myTrans <- flowJo_biexp_trans(maxValue = maxValue, widthBasis = widthBasis, pos = pos, neg = neg, equal.space = equal.space)
  scale_x_continuous(..., trans = myTrans)
}

#' @rdname scale_x_flowJo_biexp
#' @export
scale_y_flowJo_biexp <- function(..., maxValue = 262144, widthBasis = -10, pos = 4.5, neg = 0, equal.space = FALSE){
  myTrans <- flowJo_biexp_trans(maxValue = maxValue, widthBasis = widthBasis, pos = pos, neg = neg, equal.space = equal.space)
  scale_y_continuous(..., trans = myTrans)
}

