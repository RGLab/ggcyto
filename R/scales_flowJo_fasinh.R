#' Add a flowJo inverse hyperbolic sine scale to the x or y axes of a ggcyto plot.
#' 
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @param m,t see 'help(flowJo.fasinh')
#' @return ScaleContinuous object
#' @importFrom flowWorkspace flowJo_fasinh_trans
#' @examples 
#' data(GvHD)
#' fr <- GvHD[[1]]
#' p <- ggcyto(fr, aes(x = `FL1-H`)) + geom_density()
#' #display at raw scale
#' p 
#' #display at transformed scale
#' p + scale_x_flowJo_fasinh(t = 1e4)
#' @export
scale_x_flowJo_fasinh <- function(..., m = 4, t = 1200){
  myTrans <- flowJo_fasinh_trans(m = m, t = t)
  scale_x_continuous(..., trans = myTrans)

}

#' @rdname scale_x_flowJo_fasinh
#' @export
scale_y_flowJo_fasinh <- function(..., m = 4, t = 1200){
  myTrans <- flowJo_fasinh_trans(m = m, t = t)
  scale_y_continuous(..., trans = myTrans)

}
