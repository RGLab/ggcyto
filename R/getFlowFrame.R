#' extract flowFrame data structure from the given R object
#' 
#' Mainly to get the channel and marker information.
#' 
#' @param x flowSet or GatingSet/GatingHierarchy
#' @return a flowFrame. When x is a ncdfFlowSet or GatingSet that is associated with ncdfFlowSet, the raw event data
#' is not read and an empty flowFrame is returned. 
#' @export
#' @examples 
#' data(GvHD)
#' fs <- GvHD[1:2]
#' getFlowFrame(fs)# fs is a flowSet
#' 
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' getFlowFrame(gs)# gs is a GatingSet
getFlowFrame <- function(x)UseMethod("getFlowFrame")

#' @rdname getFlowFrame
#' @export
getFlowFrame.flowSet <- function(x){
  x[[1, use.exprs = FALSE]]
}

#' @rdname getFlowFrame
#' @export
getFlowFrame.ncdfFlowList <- function(x){
  getS3method("getFlowFrame", "flowSet")(x)
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
