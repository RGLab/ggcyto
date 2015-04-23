#' coerce the flowFrame to a data.frame
#' 
#' It extracts the cell event matrix and convert it to a data.frame.
#' 
#' @param x flowFrame
#' @return data.frame
#' @export
#' @
as.data.frame.flowFrame <- function(x, ...){
  as.data.frame(exprs(x))
}

#' coerce the flowSet to a data.frame
#' 
#' It extract the cell event matrix from each flowFrame
#'  and combind them to a single data.frame.
#' 
#' @param x flowSet
#' @return data.frame
#' @export
as.data.frame.flowSet <- function(x, ...){
  df.list <- fsApply(x, as.data.frame, simplify = FALSE)
  df <- ldply(df.list)
  df  
}

#' coerce flowFrame to flowSet
#' The default coerce method does not perserve the sample name.
#' 
.flowFrame2flowSet <- function(fr){
  sn <- identifier(fr)
  fs <- as(fr, "flowSet")
  sampleNames(fs) <- sn
  pData(fs)[["name"]] <- sn
  fs
}

#' Convert a flowFrame to a ggplot-compatible data.frame
#' 
#' It actually converts the flowFrame to flowSet first and
#' then dispatch to the fority method for flowSet.
#' 
#' @param model flowFrame
#' @param data not used.
#' @param ... not used.
#' 
#' @export
#' @aliases fortify
fortify.flowFrame <- function(model, data, ...){
  #covert to flowSet
  fs <- .flowFrame2flowSet(model)
  #then dispatch to forityf method for flowSet
  fortify(fs, ...)
}

#' Convert a flowSet to a ggplot-compatible data.frame
#' 
#' It invokes as.data.frame.flowSet and append the pData
#' to it so that ggplot can use the pData for facetting.
#' 
#' @param model flowSet
#' @param data not used.
#' @param ... not used.
#' 
#' @importFrom plyr ldply
#' @export
#' @aliases fortify
fortify.flowSet <- function(model, data, ...){
  #convert to data.frame
  df <- as.data.frame(model)
  #merge with pData
  pd <- pData(model)
  df <- merge(pd, df, by.x = "name", by.y = ".id")
  df
}
