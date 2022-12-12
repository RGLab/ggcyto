#' Add a logicle scale to the x or y axes of a ggcyto plot.
#' 
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @param w,t,m,a see 'help(logicleTransform')
#' @importFrom flowWorkspace logicle_trans
#' @return ScaleContinuous object
#' @examples 
#' data(GvHD)
#' fr <- GvHD[[1]]
#' p <- ggcyto(fr, aes(x = `FL1-H`)) + geom_density()
#' #display at raw scale
#' p 
#' #display at transformed scale
#' p + scale_x_logicle(t = 1e4)
#' @export
scale_x_logicle <- function(..., w = 0.5, t = 262144, m = 4.5, a = 0){
  myTrans <- logicle_trans(w = w, t = t, m = m, a = a)
  #see https://github.com/RGLab/ggcyto/issues/88#issuecomment-1346957390
  myTrans$domain <- c(-1, 1) * t
  scale_x_continuous(..., trans = myTrans)
  
}

#' @rdname scale_x_logicle
#' @export
scale_y_logicle <- function(..., w = 0.5, t = 262144, m = 4.5, a = 0){
  myTrans <- logicle_trans(w = w, t = t, m = m, a = a)
  myTrans$domain <- c(-1, 1) * t
  scale_y_continuous(..., trans = myTrans)
}
