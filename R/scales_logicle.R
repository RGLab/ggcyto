#' flowJo inverse hyperbolic sine breaks (integer breaks on fasinh-transformed scales)
#' @param n desired number of breaks
#' @param ... parameters passed to flowJo.fasinh
#' @export
#' @importFrom flowCore logicleTransform inverseLogicleTransform
logicle_breaks <- function (n = 6, ...) 
{
  
  function(x) {
    trans.obj <- logicleTransform(...)
    transFunc <- trans.obj@.Data
    invFunc <- inverseLogicleTransform(trans = trans.obj)@.Data
    rng.raw <- range(x, na.rm = TRUE)
    
    #equal-spaced
    rng <- transFunc(rng.raw)
    min <- floor(rng[1])
    max <- ceiling(rng[2])
    if (max == min) 
      return(invFunc(min))
    by <- floor((max - min)/n) + 1
    myBreaks <- invFunc(seq(min, max, by = by))      
    round(myBreaks)
  }
}

#' logicle transformation.
#' 
#' @param ... arguments passed to logicleTransform.
#' @export
logicle_trans <- function(...){
  trans.obj <- logicleTransform(...)
  trans <- trans.obj@.Data
  inv <- inverseLogicleTransform(trans = trans.obj)@.Data
  brk <- logicle_breaks(...)
  #   debug(brk)
  trans_new("logicle", transform = trans, inverse = inv, breaks = brk)
  
}

#' flowJo inverse hyperbolic sine scale
#' 
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @param w,t,m,a see 'help(logicleTransform')
#' @export
#' @importFrom ggplot2 continuous_scale waiver
scale_x_logicle <- function(..., w = 0.5, t = 262144, m = 4.5, a = 0){
  myTrans <- logicle_trans(w = w, t = t, m = m, a = a)
  #   scale_x_continuous(..., trans = myTrans)
  continuous_scale(aesthetics = c("x"), "position_c", identity,trans = myTrans, ..., expand = waiver(), guide = "none")
}

#' @rdname scale_x_logicle
#' @export
scale_y_logicle <- function(..., w = 0.5, t = 262144, m = 4.5, a = 0){
  myTrans <- logicle_trans(w = w, t = t, m = m, a = a)
  #   scale_x_continuous(..., trans = myTrans)
  continuous_scale(aesthetics = c("y"), "position_c", identity,trans = myTrans, ..., expand = waiver(), guide = "none")
}