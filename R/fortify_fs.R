#' Fortify a model into flowSet object
#'
#' Method to convert a generic R object into a flowSet useful for ggcyto
#'
#' @param model model or other R object to convert to data frame
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @export
fortify_fs <- function(model, data, ...) UseMethod("fortify_fs")

#' @export
fortify_fs.flowSet <- function(model, data, ...) model

#' @export
fortify_fs.default <- function(model, data, ...) {
  
  stop("ggcyto doesn't know how to deal with data of class ", class(model), call. = FALSE)
}

#' coerce flowFrame to flowSet
#' The default coerce method does not perserve the sample name.
#' @export
fortify_fs.flowFrame <- function(model, data, ...){
  sn <- identifier(model)
  fs <- as(model, "flowSet")
  sampleNames(fs) <- sn
  pData(fs)[["name"]] <- sn
  fs
}

#' coerce a GatingSet node to flowSet
#' The default coerce method does not perserve the sample name.
#' @param model GatingSet object that has 'subset' character attribute that specifies the node name
#' @export
fortify_fs.GatingSet <- function(model, data, ...){
  subset <- attr(model, "subset")
  if(is.null(subset))
    stop("subset must be supplied!")
  else if(subset == "_parent_")
    stop("'subset' must be instantiated by the actual node name!\nMake sure either 'subset' is specified or the 'geom_gate' layer is added. ")
  fs <- getData(model, subset)
#   attr(fs, "gs") <- model #need to store gs to be used later for other layers
  fs
  
}