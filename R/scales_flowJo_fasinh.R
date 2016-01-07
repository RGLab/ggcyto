#' flowJo inverse hyperbolic sine breaks (integer breaks on fasinh-transformed scales)
#' 
#' Used to construct \code{\link{flowJo_fasinh_trans}} object
#' 
#' @param n desired number of breaks
#' @param ... parameters passed to flowJo.fasinh
#' @return a function generates fasinh or fsinh space
#' @examples 
#' 
#' data <- 1:1e3
#' brks.func <- flowJo_fasinh_breaks()
#' brks <- brks.func(data)
#' brks # fasinh space displayed at raw data scale
#' 
#' #transform it to verify it is equal-spaced at transformed scale
#' trans.func <- flowJo.fasinh()
#' brks.trans <- trans.func(brks)
#' brks.trans 
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
#' Used for inverse hyperbolic sine scale layer \code{\link{scale_x_flowJo_fasinh}}
#' 
#' @inheritParams flowJo_fasinh_breaks
#' @return fasinh transformation object
#' @examples 
#' flowJo_fasinh_trans()
#' @export
flowJo_fasinh_trans <- function(...){
  trans <- flowJo.fasinh(...)
  inv <- flowJo.fsinh(...)
  brk <- flowJo_fasinh_breaks(...)
  
  trans_new("flowJo_fasinh", transform = trans, inverse = inv, breaks = brk)

}
  
#' flowJo inverse hyperbolic sine scale
#' 
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @param m,t see 'help(flowJo.fasinh')
#' @return ScaleContinuous object
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
