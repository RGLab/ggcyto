#' Fortify a model into flowSet object
#'
#' The method provides a universe interface to convert a generic R object into a flowSet useful for ggcyto
#'
#' @param model flow object(flowFrame or GatingSet) to be converted to flowSet. when it is a GatingSet, it must contain the subset information stored as  'subset' attribute.
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @return a flowSet/ncdfFlowSet object
#' @examples 
#' \dontrun{
#' fortify_fs(fr)
#' attr(gs, "subset") <- "CD4"
#' fortify_fs(gs)
#' }
#' @export
fortify_fs <- function(model, data, ...) UseMethod("fortify_fs")

#' @rdname fortify_fs
#' @export
fortify_fs.flowSet <- function(model, data, ...) model

#' @rdname fortify_fs
#' @export
fortify_fs.default <- function(model, data, ...) {
  
  stop("ggcyto doesn't know how to deal with data of class ", class(model), call. = FALSE)
}




#' @export
#' @rdname fortify_fs
fortify_fs.flowFrame <- function(model, data, ...){
  sn <- identifier(model)
  fs <- as(model, "flowSet")
  sampleNames(fs) <- sn
  pData(fs)[["name"]] <- sn
  fs
}


#' @export
#' @rdname fortify_fs
fortify_fs.GatingSet <- function(model, data, ...){
  subset <- attr(model, "subset")
  if(is.null(subset))
    stop("subset must be supplied!")
  else if(subset == "_parent_")
    stop("'subset' must be instantiated by the actual node name!\nMake sure either 'subset' is specified or the 'geom_gate' layer is added. ")
  fs <- getData(model, subset)
  #copy dims attribute to fs
  attr(fs, "dims") <- attr(model, "dims")
  attr(fs, "filter") <- attr(model, "filter")
  fs
  
}