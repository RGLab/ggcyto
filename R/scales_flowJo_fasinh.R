###############################
#(we should eventually remove its copy from flowWorkspace)
########################

#'  hyperbolic sine/inverse hyperbolic sine (flowJo-version) transform function constructor
#' 
#' @rdname flowJo.fasinh
#' @param m numeric the full width of the transformed display in asymptotic decades
#' @param t numeric the maximum value of input data
#' @param a numeric Additional negative range to be included in the display in asymptotic decades
#' @param length numeric the maximum value of transformed data
#' @return fasinh/fsinh transform function
#' @export
flowJo.fasinh <- function (m = 4.0, t = 12000, a =  0.7, length = 256) 
{
  function(x){
    length * ((asinh(x * sinh(m * log(10)) / t) + a * log(10)) / ((m + a) * log(10)))
  }
}

#' @rdname flowJo.fasinh
#' @export
flowJo.fsinh <- function(m = 4.0, t = 12000, a =  0.7, length = 256){
  function(x){
    sinh(((m + a) * log(10)) * x/length - a * log(10)) * t / sinh(m * log(10)) 
  }
}

#' flowJo inverse hyperbolic sine breaks (integer breaks on fasinh-transformed scales)
#' @param n desired number of breaks
#' @param ... parameters passed to flowJo.fasinh
#' @return a function generates fasinh or fsinh space
#' @export
flowJo_fasinh_breaks <- function (n = 6, ...) 
{
  
  function(x) {
    transFunc <- flowJo.fasinh(...)
    invFunc <- flowJo.fsinh(...)
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
  
#' flowJo inverse hyperbolic sine transformation.
#' 
#' @inheritParams flowJo_fasinh_breaks
#' @return fasinh transformation object
#' @export
flowJo_fasinh_trans <- function(...){
  trans <- flowJo.fasinh(...)
  inv <- flowJo.fsinh(...)
  brk <- flowJo_fasinh_breaks(...)
  #   debug(brk)
  trans_new("flowJo_fasinh", transform = trans, inverse = inv, breaks = brk)

}
  
#' flowJo inverse hyperbolic sine scale
#' 
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @param m,t see 'help(flowJo.fasinh')
#' @export
scale_x_flowJo_fasinh <- function(..., m = 4, t = 1200){
  mytrans <- flowJo_fasinh_trans(m = m, t = t)
#   scale_x_continuous(..., trans = myTrans)
  continuous_scale(aesthetics = c("x"), "position_c", identity,trans = myTrans, ..., expand = waiver(), guide = "none")
}

#' @rdname scale_x_flowJo_fasinh
#' @export
scale_y_flowJo_fasinh <- function(..., m = 4, t = 1200){
  mytrans <- flowJo_fasinh_trans(m = m, t = t)
  #   scale_x_continuous(..., trans = myTrans)
  continuous_scale(aesthetics = c("y"), "position_c", identity,trans = myTrans, ..., expand = waiver(), guide = "none")
}