###############################
#(we should eventually remove its copy from flowWorkspace)
########################

#'  hyperbolic sine/inverse hyperbolic sine (flowJo-version) transform function constructor
#' 
#' @rdname flowJo.fasinh
#' @param M numeric the full width of the transformed display in asymptotic decades
#' @param T numeric the maximum value of input data
#' @param A numeric Additional negative range to be included in the display in asymptotic decades
#' @param length numeric the maximum value of transformed data
#' @export
flowJo.fasinh <- function (M = 4.0, T = 12000, A =  0.7, length = 256) 
{
  function(x){
    length * ((asinh(x * sinh(M * log(10)) / T) + A * log(10)) / ((M + A) * log(10)))
  }
}

#' @rdname flowJo.fasinh
#' @export
flowJo.fsinh <- function(M = 4.0, T = 12000, A =  0.7, length = 256){
  function(x){
    sinh(((M + A) * log(10)) * x/length - A * log(10)) * T / sinh(M * log(10)) 
  }
}

#' flowJo inverse hyperbolic sine breaks (integer breaks on fasinh-transformed scales)
#' @param n desired number of breaks
#' @param ... parameters passed to flowJo.fasinh
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
#' @param M, T see 'help(flowJo.fasinh')
#' @export
scale_x_flowJo_fasinh <- function(..., M = 4, T = 1200){
  myTrans <- flowJo_fasinh_trans(M = M, T = T)
#   scale_x_continuous(..., trans = myTrans)
  continuous_scale(aesthetics = c("x"), "position_c", identity,trans = myTrans, ..., expand = waiver(), guide = "none")
}

#' @rdname scale_x_flowJo_fasinh
#' @export
scale_y_flowJo_fasinh <- function(..., M = 4, T = 1200){
  myTrans <- flowJo_fasinh_trans(M = M, T = T)
  #   scale_x_continuous(..., trans = myTrans)
  continuous_scale(aesthetics = c("y"), "position_c", identity,trans = myTrans, ..., expand = waiver(), guide = "none")
}