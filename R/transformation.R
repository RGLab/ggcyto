# 
# #' logicle transformation.
# #' 
# #' @export
# #' @importFrom scales trans_new
# logicle_trans <- function(){
#   trans_new("logicle", )
#   
# }
# 
# #' logicle transformation.
# #' 
# #' @export
# #' @importFrom scales trans_new
# flowJo_fasinh_trans <- function(...){
#   trans <- flowWorkspace:::.fasinh
#   inv <- flowWorkspace:::.fsinh
#   brk <- log_breaks(base = base)
#   debug(brk)
#   trans_new("flowJo_fasinh", transform = trans, inverse = inv, breaks = brk)
#   
# }

#' flowJo biexponential breaks (integer breaks on biexp-transformed scales)
#' 
#' @export
#' @param n desired number of breaks
#' @param pretty if use log10-like breaks that is normally used in flowJo
#'        when FALSE, breaks are equally spaced instead.
#' @param ... parameters passed to flowWorkspace::flowJoTrans function
flowJo_biexp_breaks <- function (n = 6, pretty = FALSE, ...) 
{
  
  f1 <- function(x) {
    transFunc <- flowJoTrans(...)
    invFunc <- flowJoTrans(..., inverse = TRUE)
    rng.raw <- range(x, na.rm = TRUE)
      if(pretty){
      #log10 (e.g. 0, 10, 1000, ...)
      base10raw <- unlist(lapply(2:n,function(e)10^e))
      base10raw <- c(0,base10raw)
      myBreaks <- base10raw[base10raw>min(rng.raw)&base10raw<max(rng.raw)]
    }else{
      #equal-spaced
      rng <- transFunc(rng.raw)
      min <- floor(rng[1])
      max <- ceiling(rng[2])
      if (max == min) 
        return(invFunc(min))
      by <- floor((max - min)/n) + 1
      myBreaks <- invFunc(seq(min, max, by = by))      
    }
    round(myBreaks)
  }
  
}


#' logicle transformation. 
#' 
#' 
#' 
#' @export
#' @importFrom scales trans_new format_format
#' @importFrom flowWorkspace flowJoTrans
#' @param ... arguments passed to flowJoTrans
flowJo_biexp_trans <- function(..., pretty = FALSE){
#   browser()
  trans <- flowJoTrans(...)
  inv <- flowJoTrans(..., inverse = TRUE)
  brk <- flowJo_biexp_breaks(pretty = pretty, ...)
    if(pretty){
    fmt <- flowWorkspace:::pretty10exp  
    formals(fmt)[["drop.1"]] <- TRUE
  }else
    fmt <- format_format(digits = 0)
#   debug(brk)
  trans_new("flowJo_biexp", transform = trans, inverse = inv
            , breaks = brk
            , format = fmt
#               , domain = c(-Inf, 4096)
            )
  
}

scale_x_flowJo_biexp <- function(..., maxValue = 262144, widthBasis = -10, pretty = FALSE){
  myTrans <- flowJo_biexp_trans(maxValue = maxValue, widthBasis = widthBasis, pretty = pretty)
#   scale_x_continuous(..., trans = myTrans)
  continuous_scale(aesthetics = c("x"), "position_c", identity,trans = myTrans, ..., expand = waiver(), guide = "none")
}
