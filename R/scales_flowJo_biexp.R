#' flowJo biexponential breaks (integer breaks on biexp-transformed scales)
#' 
#' Used to construct \code{\link{flowJo_biexp_trans}} object
#' 
#' @export
#' @param n desired number of breaks
#' @param pretty if use log10-like breaks that is normally used in flowJo
#'        when FALSE, breaks are equally spaced instead.
#' @param ... parameters passed to \code{\link[flowWorkspace]{flowJoTrans}}
#' @return a function generates biexponential space
#' @examples 
#' 
#' data <- 1:1e3
#' brks.func <- flowJo_biexp_breaks()
#' brks <- brks.func(data)
#' brks # biexp space displayed at raw data scale
#' 
#' #transform it to verify it is equal-spaced at transformed scale
#' trans.func <- flowJoTrans()
#' brks.trans <- trans.func(brks)
#' brks.trans 
flowJo_biexp_breaks <- function (n = 6, pretty = FALSE, ...) 
{
  
  function(x) {
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


#' flowJo biexponential transformation. 
#' 
#' Used for biexponential scale layer \code{\link{scale_x_flowJo_biexp}}
#' 
#' @export
#' @importFrom scales trans_new format_format
#' @inheritParams flowJo_biexp_breaks
#' @examples 
#' flowJo_biexp_trans()
#' @return biexponential transformation object
flowJo_biexp_trans <- function(..., pretty = FALSE){

  trans <- flowJoTrans(...)
  inv <- flowJoTrans(..., inverse = TRUE)
  brk <- flowJo_biexp_breaks(pretty = pretty, ...)
    if(pretty){
    fmt <- flowWorkspace:::pretty10exp  
    formals(fmt)[["drop.1"]] <- TRUE
  }else
    fmt <- format_format(digits = 0)

  trans_new("flowJo_biexp", transform = trans, inverse = inv
            , breaks = brk
#             , format = fmt
#               , domain = c(-Inf, 4096)
            )
  
}

#' flowJo biexponential scale
#' 
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @param maxValue,widthBasis,pos,neg see 'help(flowJoTrans')
#' @param pretty whether to display the breaks in pretty format
#' @return ScaleContinuous object
#' @examples 
#' data(GvHD)
#' fr <- GvHD[[1]]
#' p <- ggcyto(fr, aes(x = `FL1-H`)) + geom_density()
#' #display at raw scale
#' p 
#' #display at transformed scale
#' p + scale_x_flowJo_biexp(maxValue = 1e4, widthBasis = 0)
#' @export
scale_x_flowJo_biexp <- function(..., maxValue = 262144, widthBasis = -10, pos = 4.5, neg = 0, pretty = FALSE){
  myTrans <- flowJo_biexp_trans(maxValue = maxValue, widthBasis = widthBasis, pos = pos, neg = neg, pretty = pretty)
  scale_x_continuous(..., trans = myTrans)
}

#' @rdname scale_x_flowJo_biexp
#' @export
scale_y_flowJo_biexp <- function(..., maxValue = 262144, widthBasis = -10, pos = 4.5, neg = 0, pretty = FALSE){
  myTrans <- flowJo_biexp_trans(maxValue = maxValue, widthBasis = widthBasis, pos = pos, neg = neg, pretty = pretty)
  scale_y_continuous(..., trans = myTrans)
}

