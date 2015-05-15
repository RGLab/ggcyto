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
  df <- ldply(df.list, .id = ".rownames")
  df  
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
fortify.flowFrame <- function(model, data, ...){
  #covert to flowSet
  fs <- fortify_fs(model)
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
  pd <- name_rows(pd)#add rownames to column
  df <- merge(pd, df, by = ".rownames")
  
  
#   #we have to attach the extra copy of pd to attribute as well
#   # in order for the ggcyo wrapper to copy it to the gate layer
#   attr(df, "pd") <- pd
#   attr(df, "gs") <- attr(model, "gs") #copy gs attribute over
  df
}

#' coerce a GatingSet node to data.frame
#' @param model GatingSet
#' @export
fortify.GatingSet <- function(model, ...){
  
  fs <- fortify_fs(model, ...)
  fortify(fs)
}

#' Convert a polygonGate to a data.frame useful for ggplot
#' 
#' It converts the boundaries slot into a data.frame
#' 
#' @param model polygonGate
#' @param data not used.
#' @param ... not used.
#' 
#' @export
fortify.polygonGate <- function(model, data, ...){
  as.data.frame(model@boundaries)
}

#' Convert a filterList to a data.frame useful for ggplot
#' 
#' It tries to merge with pData
#' 
#' @param model filterList
#' @param data pData of flowSet
#' @param ... not used.
#' 
#' @importFrom plyr name_rows
#' @export
fortify.filterList <- function(model, data, ...){
  
  # convert each filter to df
  df <- ldply(model, fortify, .id = ".rownames")
  pd <- attr(model,"pd")
  if(!is.null(pd)){
    # merge with pd
    pd <- name_rows(pd)
    df <- merge(df, pd)  
    attr(df, "annotated") <- TRUE
  }
    
  
  df
}

#' Convert a rectangleGate to a data.frame useful for ggplot
#' 
#' For 2d rectangelGate, it is converted to a geom_polygon format
#' for 1d, uses geom_vline/hline format.
#' 
#' @param model rectangleGate
#' @param data not used.
#' @param ... not used.
#' 
#' @export
fortify.rectangleGate <- function(model, data, ...){
  
  param <- parameters(model)
  nDim <- length(param)
  l.b <- model@min
  r.t <- model@max  
  if (nDim ==  2){
    
    l.t <- c(l.b[1], r.t[2])
    r.b <- c(r.t[1], l.b[2])
    
    as.data.frame(do.call(rbind, list(l.b, l.t, r.t, r.b)))
  }else if(nDim ==  1){
    coord <- c(l.b, r.t)
#     toRm <- is.infinite(coord)
#     if(any(toRm))
#       coord <- coord[!toRm] 
# browser()
    df <- data.frame(unname(coord), check.names = F)
    colnames(df) <- param
    df
  }else
    stop("rectangelGate with dimension ", dDim, "is not supported!")
#   browser()
}

