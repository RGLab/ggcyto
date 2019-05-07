#' Add a flowJo inverse hyperbolic sine scale to the x or y axes of a ggcyto plot.
#' 
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
#' @rdname scale_x_flowjo_fasinh
#' @export
scale_x_flowjo_fasinh <- function(..., m = 4, t = 1200){
  myTrans <- flowjo_fasinh_trans(m = m, t = t)
  scale_x_continuous(..., trans = myTrans)

}

#' @rdname scale_x_flowjo_fasinh
#' @export
scale_x_flowJo_fasinh <- function(...){
  .Deprecated("scale_x_flowjo_fasinh")
  scale_x_flowjo_fasinh(...)
}

#' @rdname scale_x_flowjo_fasinh
#' @export
scale_y_flowjo_fasinh <- function(..., m = 4, t = 1200){
  myTrans <- flowjo_fasinh_trans(m = m, t = t)
  scale_y_continuous(..., trans = myTrans)

}

#' @rdname scale_x_flowjo_fasinh
#' @export
scale_y_flowJo_fasinh <- function(...){
  .Deprecated("scale_x_flowjo_fasinh")
  scale_x_flowjo_fasinh(...)
}
