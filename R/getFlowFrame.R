#' extract flowFrame data structure from the given R object
#' 
#' Mainly to get the channel and marker information.
#' 
#' @param x flowSet or GatingSet/GatingHierarchy
#' @return an empty flowFrame
#' @export
getFlowFrame <- function(x)UseMethod("getFlowFrame")

#' @rdname getFlowFrame
#' @export
getFlowFrame.flowSet <- function(x){
  x[[1, use.exprs = FALSE]]
}

#' @rdname getFlowFrame
#' @export
getFlowFrame.GatingSet <- function(x){
  getFlowFrame(getData(x))
}

#' @rdname getFlowFrame
#' @export
getFlowFrame.GatingHierarchy <- function(x){
  getData(x, use.exprs = FALSE)
}