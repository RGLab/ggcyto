#' flowCore logicle breaks (integer breaks on logicle-transformed scales)
#' 
#' Used to construct \code{\link{logicle_trans}} object
#' 
#' @param n desired number of breaks
#' @param ... parameters passed to flowJo.fasinh
#' @return a function that generates logicle space
#' @examples 
#' 
#' data <- 1:1e3
#' brks.func <- logicle_breaks()
#' brks <- brks.func(data)
#' brks # logicle space displayed at raw data scale
#' 
#' #logicle transform it to verify it is equal-spaced at transformed scale
#' trans.obj <- logicleTransform()
#' trans.func <- slot(trans.obj, ".Data")
#' brks.trans <- trans.func(brks)
#' brks.trans 
#' @export
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
#' Used for logicle scale layer \code{\link{scale_x_logicle}}
#' 
#' @param ... arguments passed to logicleTransform.
#' @return a logicle transformation object
#' @examples 
#' logicle_trans()
#' @export
logicle_trans <- function(...){
  trans.obj <- logicleTransform(...)
  trans <- trans.obj@.Data
  inv <- inverseLogicleTransform(trans = trans.obj)@.Data
  brk <- logicle_breaks(...)
  trans_new("logicle", transform = trans, inverse = inv, breaks = brk)
  
}

#' flowJo inverse hyperbolic sine scale
#' 
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @param w,t,m,a see 'help(logicleTransform')
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
