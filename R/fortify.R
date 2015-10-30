#' coerce the flowFrame to a data.table
#' 
#' It extracts the cell event matrix and convert it to a data.table.
#' 
#' @param x flowFrame
#' @return data.table
#' @import data.table
#' @export
#' @
.fr2dt <- function(x, ...){
  as.data.table(exprs(x))
}

#' coerce the flowSet to a data.table
#' 
#' It extract the cell event matrix from each flowFrame
#'  and combind them to a single data.table.
#' 
#' @param x flowSet
#' @return data.table
#' @export
.fs2dt <- function(x, ...){
  # subset by columns if applicable
  dims <- attr(x, "dims")
  if(!is.null(dims))
    x <- x[, dims[, name]]
  thisFilter <- attr(x, "filter")
  if(!is.null(thisFilter)){
    if(is.function(thisFilter)){
      thisFilter <- thisFilter(x, dims)
    }
    x <- Subset(x, thisFilter)
  }
    
  df.list <- .fsdply(x, .fr2dt, .id = ".rownames")
  
}


#' Convert a flowFrame to a ggplot-compatible data.table
#' 
#' It actually converts the flowFrame to flowSet first and
#' then dispatch to the fority method for flowSet.
#' 
#' @param model flowFrame
#' @param data not used.
#' @param ... not used.
#' 
# @import grid
# @import methods
#' @import flowCore
#' @import ncdfFlow
#' @import flowWorkspace
#' @import ggplot2
# @import Biobase 
# @import BiocGenerics
#' @export
fortify.flowFrame <- function(model, data, ...){
  #covert to flowSet
  fs <- fortify_fs(model)
  #then dispatch to forityf method for flowSet
  fortify(fs, ...)
}

#' convert pData to data.table
.pd2dt <- function(pd){
  pd <- as.data.table(pd, keep.rownames = TRUE)
  setnames(pd, "rn", ".rownames")
  pd
}
#' Convert a flowSet to a ggplot-compatible data.table
#' 
#' It invokes as.data.table.flowSet and append the pData
#' to it so that ggplot can use the pData for facetting.
#' 
#' @param model flowSet
#' @param data not used.
#' @param ... not used.
#' 
#' @export
#' @aliases fortify
fortify.flowSet <- function(model, data, ...){
  #convert to data.table
  df <- .fs2dt(model)

  #merge with pData
  pd <- .pd2dt(pData(model))
  
  merge(pd, df, by = ".rownames")

}

#' coerce a GatingSet node to data.table
#' @param model GatingSet
#' @param ... not used.
#' @export
fortify.GatingSet <- function(model, ...){
  
  fs <- fortify_fs(model, ...)
  fortify(fs)
}

#' Convert a polygonGate to a data.table useful for ggplot
#' 
#' It converts the boundaries slot into a data.table
#' 
#' @param model polygonGate
#' @param data not used.
#' @param bins the plot information collected from flow data and geom_hex used to interpolate polygon with more vertices. Interpolation is mainly 
#'                    for the purpose of plotting (so that it won't lose its shape from subsetting through 'limits').
#'                    But it is not necessary for other purposes like centroid calculation.
#' @param ... not used.
#' 
#' @export
fortify.polygonGate <- function(model, data
                                # , measure_range = NULL
                                , bins = NULL, ...){
  
  vertices <- model@boundaries
  chnls <- colnames(vertices)
  
  
  #measure_range currently not used
  if(is.null(bins)){
    
    new.vertices <- vertices
  }else
  {
    # browser()
    #normalize data first
    vertices <- scale(vertices)
    sd <- attr(vertices, "scaled:scale")
    mu <- attr(vertices, "scaled:center")

    measure_range <- diff(range(vertices))
    
    
    unit_length <- measure_range/bins
    
    nVert <- nrow(vertices)
    # compute distance between each pair of adjacent points
    edges <- sapply(1:nVert, function(i){
                     j <- ifelse(i < nVert, i + 1, 1)
                     dist(vertices[c(i, j), ])[[1]]
                  })
    
    #determine the number of points to be interpolated for each edge
    nEdge.points <- round(edges / unit_length)
    new.vertices <- .ldply(1:nVert, function(i){
      j <- ifelse(i < nVert, i + 1, 1)
      thisPair <- vertices[c(i, j),]
      
      xx <- thisPair[,1]
      yy <- thisPair[,2]
      
      nOut <- max(2, nEdge.points[i]) # at least 2 to preserve orginal points
      # interpolate more points to prevent it from losing its shape by xlim/ylim
      if(xx[1] == xx[2]){
        new.points <- list(x = rep(xx[1], nOut)
                          , y = seq(yy[1], yy[2], (yy[2] - yy[1])/(nOut-1))
                          )
      }else{
        
        new.points <- approx(x = xx, y = yy, n = nOut) 
        #approx tends to goes from left to right regardless of the order of original points
        #we try to reverse it when needed
        
        if(xx[1] > xx[2]){
          new.points[["x"]] <- rev(new.points[["x"]])
          new.points[["y"]] <- rev(new.points[["y"]])
        }  
      }
  #     plot(thisPair, xlim =range(vertices[,1]), ylim =range(vertices[,2]))
  #     text(new.points,labels = 1:nOut,  col = "red")
      as.data.table(new.points)
    })
    
  
    new.vertices <- sweep(new.vertices , 2L,  sd, "*")
    new.vertices <- sweep(new.vertices , 2L,  mu, "+")
    
#     plot(vertices, type = "l")
#     polygon(new.vertices, col = "red")
  }
  
  dt <- as.data.table(new.vertices)
  setnames(dt, chnls)
  dt
}

#' Convert a ellipsoidGate to a data.table useful for ggplot
#' 
#' It converts ellipsoidGate to polygongate before fortifying it.
#' 
#' @param model ellipsoidGate
#' @param data not used.
#' @param ... not used.
#' 
#' @export
fortify.ellipsoidGate <- function(model, data, ...){
  poly.g <- flowViz:::ell2Polygon(model)  
  fortify(poly.g, ...)
}

#' Convert a filterList to a data.table useful for ggplot
#' 
#' It tries to merge with pData
#' 
#' @param model filterList
#' @param data not used
#' @param bins used for interpolating polygonGates to prevent it from losing shape when truncated by axis limits
#' @param ... not used.
#' 
#' @importFrom plyr name_rows
#' @export
fortify.filterList <- function(model, data
                               # , measure_range = NULL
                               , bins = NULL, ...){
  
  # convert each filter to df
  df <- .ldply(model, fortify
               # , measure_range = measure_range
               , bins = bins, .id = ".rownames")
  
  pd <- attr(model,"pd")
  if(!is.null(pd)){
    # merge with pd
    
    if(!is(pd, "data.table"))
      pd <- .pd2dt(pd)
    df <- merge(df, pd, by = ".rownames")  
    attr(df, "annotated") <- TRUE
  }
    
  
  df
}

#' Convert a rectangleGate to a data.table useful for ggplot
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
  if (nDim ==  2){
    fortify(as(model, "polygonGate"), ...)
  }else if(nDim ==  1){
    l.b <- model@min
    r.t <- model@max  
    
    coord <- c(l.b, r.t)
    
    df <- data.table(unname(coord), check.names = F)
    setnames(df, "V1" , param)
    df
  }else
    stop("rectangelGate with dimension ", nDim, "is not supported!")
#   browser()
}

