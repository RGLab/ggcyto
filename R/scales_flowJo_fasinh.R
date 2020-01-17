#' Add a flowJo inverse hyperbolic sine scale to the x or y axes of a ggcyto plot.
#' 
#' @name scales_flowjo_fasinh
#' @aliases scale_x_flowjo_fasinh scale_y_flowjo_fasinh
#' scale_x_flowJo_fasinh scale_y_flowJo_fasinh
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @param m,t see 'help(flowjo_fasinh')
#' @return ScaleContinuous object
#' @importFrom flowWorkspace flowjo_fasinh_trans
#' @examples 
#' data(GvHD)
#' fr <- GvHD[[1]]
#' p <- ggcyto(fr, aes(x = `FL1-H`)) + geom_density()
#' #display at raw scale
#' p 
#' #display at transformed scale
#' p + scale_x_flowjo_fasinh(t = 1e4)
#' @export
scale_x_flowjo_fasinh <- function(..., m = 4, t = 1200){
  myTrans <- flowjo_fasinh_trans(m = m, t = t)
  scale_x_continuous(..., trans = myTrans)

}

#' @export
scale_x_flowJo_fasinh <- function(...){
  .Deprecated("scale_x_flowjo_fasinh")
  scale_x_flowjo_fasinh(...)
}

#' @rdname scales_flowjo_fasinh
#' @export
scale_y_flowjo_fasinh <- function(..., m = 4, t = 1200){
  myTrans <- flowjo_fasinh_trans(m = m, t = t)
  scale_y_continuous(..., trans = myTrans)

}

#' @export
scale_y_flowJo_fasinh <- function(...){
  .Deprecated("scale_x_flowjo_fasinh")
  scale_x_flowjo_fasinh(...)
}
