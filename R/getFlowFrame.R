#' extract flowFrame data structure from the given R object
#' 
#' Mainly to get the channel and marker information.
#' 
#' @param x flowSet or GatingSet/GatingHierarchy
#' @return an empty flowFrame
getFlowFrame <- function(x)UseMethod("getFlowFrame")

#' @rdname getFlowFrame
getFlowFrame.flowSet <- function(x){
  x[[1, use.exprs = FALSE]]
}

#' @rdname getFlowFrame
getFlowFrame.GatingSet <- function(x){
  getFlowFrame(getData(x))
}

#' @rdname getFlowFrame
getFlowFrame.GatingHierarchy <- function(x){
  getData(x, use.exprs = FALSE)
}