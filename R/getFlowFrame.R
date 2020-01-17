#' extract flowFrame data structure from the given R object
#' 
#' Mainly to get the channel and marker information.
#' 
#' @name getFlowFrame
#' @aliases getFlowFrame.flowSet getFlowFrame.ncdfFlowList
#' getFlowFrame.GatingSetList getFlowFrame.GatingSet
#' getFlowFrame.GatingHierarchy
#' @param x flowSet, ncdfFlowList, GatingSet, GatingHierarchy, or GatingSetList
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

#' @export
getFlowFrame.flowSet <- function(x){
  x[[1, use.exprs = FALSE]]
}

#' @export
getFlowFrame.ncdfFlowList <- function(x){
  getS3method("getFlowFrame", "flowSet")(x)
}

#' @export
getFlowFrame.GatingSetList <- function(x){
  getS3method("getFlowFrame", "GatingSet")(x)
}

#' @export
getFlowFrame.GatingSet <- function(x){
  getFlowFrame(gs_pop_get_data(x))
}

#' @export
getFlowFrame.GatingHierarchy <- function(x){
  gh_pop_get_data(x, use.exprs = FALSE)
}
