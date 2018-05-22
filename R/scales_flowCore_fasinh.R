#' Add a flowCore inverse hyperbolic sine scale to the x or y axes of a ggcyto plot.
#' 
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @param a,b,c see 'help(arcsinhTransform')
#' @return ScaleContinuous object
#' @examples 
#' data(GvHD)
#' fr <- GvHD[[1]]
#' p <- ggcyto(fr, aes(x = `FL1-H`)) + geom_density()
#' #display at raw scale
#' p 
#' #display at transformed scale
#' p + scale_x_flowCore_fasinh(a = 2)
#' @export
scale_x_flowCore_fasinh <- function(..., a=1, b=1, c=0){
  myTrans <- flowCore_asinht_trans(a = a, b = b, c = c)
  scale_x_continuous(..., trans = myTrans)

}

#' @rdname scale_x_flowCore_fasinh
#' @export
scale_y_flowCore_fasinh <- function(..., a=1, b=1, c=0){
  myTrans <- flowCore_asinht_trans(a = a, b = b, c = c)
  scale_y_continuous(..., trans = myTrans)

}

## Hyperbolic sin transformation constructor
sinhTransform <- function(transformationId="defaultsinhTransform",
                             a=1, b=1, c=0)
{
  t <- new("transform", .Data=function(x) (sinh(x-c)-a)/b)
  t@transformationId <- transformationId
  t
}


#' Inverse hyperbolic sine transformation(flowCore version).
#'
#' Used to construct inverse hyperbolic sine transform object.
#'
#' @param n desired number of breaks (the actual number will be different depending on the data range)
#' @param equal.space whether breaks at equal-spaced intervals
#' @param ... parameters passed to arcsinhTransform
#' @return asinht transformation object
#' @examples
#' trans.obj <- flowCore_asinht_trans(equal.space = TRUE)
#' data <- 1:1e3
#' brks.func <- trans.obj[["breaks"]]
#' brks <- brks.func(data)
#' brks # fasinh space displayed at raw data scale
#'
#' #transform it to verify it is equal-spaced at transformed scale
#' trans.func <- trans.obj[["transform"]]
#' brks.trans <- trans.func(brks)
#' brks.trans
#' @export
#' @importFrom flowCore arcsinhTransform
#' @importFrom flowWorkspace flow_trans
flowCore_asinht_trans <- function(..., n = 6, equal.space = FALSE){
  trans <- arcsinhTransform(...)
  inv <- sinhTransform(...)
  flow_trans(name = "asinht", trans.fun = trans, inverse.fun = inv, n = n, equal.space = equal.space)
}