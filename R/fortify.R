#' coerce the flowFrame to a data.table
#' 
#' It extracts the cell event matrix and convert it to a data.table.
#' 
#' @param x flowFrame
#' @return data.table
#' @import data.table
#' @export
#' @noRd 
.fr2dt <- function(x, mapping = NULL, ...){
  dt <- as.data.table(exprs(x))
  # sort values by aesthetic mapping
  if(any(c("size", "colour", "fill") %in% mapping$axis)) {
    cols <- c(
      "-1" ="size",     # -1 = descending order
      "1" = "colour",   # 1 = ascending order
      "1" = "fill"
    )
    cols <- cols[
      !is.na(
        match(
          cols,
          mapping$axis
        )
      )
    ]
    setorderv(
      dt, 
      cols = mapping$name[as.integer(names(cols))],
      order = as.integer(names(cols))
    )
  }
  return(dt)
}

#' coerce the flowSet to a data.table
#' 
#' It extract the cell event matrix from each flowFrame
#'  and combind them to a single data.table.
#' 
#' @param x flowSet
#' @return data.table
#' @export
#' @noRd 
.fs2dt <- function(x, ...){
  
  thisFilter <- attr(x, "filter")#must to get attr here before it is lost during subsetting
  
  # subset by columns if applicable
  dims <- attr(x, "dims")
  if(!is.null(dims))
    x <- x[, unique(dims[, name])]
  
  if(!is.null(thisFilter)){
    if(is.function(thisFilter)){
      thisFilter <- thisFilter(x, unique(dims[, name]))
    }
    x <- Subset(x, thisFilter)
  }
    
  df.list <- .fsdply(x, .fr2dt, mapping = dims, .id = ".rownames")
  
}

#' @export
#' @rdname fortify.flowSet
fortify.cytoframe <- function(model, ...){
  getS3method("fortify", "flowFrame")(model, ...)
}

#' @export
#' @return data.table
#' @rdname fortify.flowSet
fortify.flowFrame <- function(model, data, ...){
  #covert to flowSet
  fs <- fortify_fs(model)
  #then dispatch to forityf method for flowSet
  fortify(fs, ...)
}

#' convert pData to data.table
#' @noRd 
.pd2dt <- function(pd){
  pd <- as.data.table(pd, keep.rownames = TRUE)
  setnames(pd, "rn", ".rownames")
  pd
}
#' Convert a flowFrame/flowSet/GatingSet to a ggplot-compatible data.table
#' 
#' It extracts events matrices and appends the pData to it so that ggplot can use the pData for facetting.
#' 
#' @param model flowFrame, flowSet or GatingSet
#' @param data not used.
#' @param ... not used.
#' 
#' @export
#' @aliases fortify
#' @return data.table
#' @examples 
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' 
#' attr(gs, "subset") <- "CD4" #must attach subset information to GatingSet object before foritfying it
#' fortify(gs)
#' 
#' fs <- gs_pop_get_data(gs, "CD8")
#' fortify(fs)#fs is a flowSet/ncdfFlowSet
#' 
#' fr <- fs[[1]]
#' fortify(fr)#fr is a flowFrame
fortify.flowSet <- function(model, data, ...){
  #convert to data.table
  df <- .fs2dt(model)

  #merge with pData
  pd <- .pd2dt(pData(model))
  
  merge(pd, df, by = ".rownames")

}

#' @export
#' @rdname fortify.flowSet
fortify.cytoset <- function(model, ...){
  getS3method("fortify", "flowSet")(model, ...)
}
#' @export
#' @rdname fortify.flowSet
fortify.ncdfFlowList <- function(model, ...){
  getS3method("fortify", "flowSet")(model, ...)
}

#' @export
#' @rdname fortify.flowSet
fortify.GatingSetList <- function(model, ...){
  getS3method("fortify", "GatingSet")(model, ...)
}

#' @export
#' @return data.table
#' @rdname fortify.flowSet
fortify.GatingSet <- function(model, ...){
  
  fs <- fortify_fs(model, ...)
  fortify(fs)
}

#' Convert a polygonGate to a data.table useful for ggplot
#' 
#' It converts the boundaries slot into a data.table
#' 
#' 
#' @param model polygonGate
#' @param data data range used to reset off-bound gate coordinates to prevent interpolating on the extremely large space unnecessarily.
#' @param nPoints not used
#' @param ... not used.
#' 
#' @export
#' @return data.table
#' @examples 
#' sqrcut <- matrix(c(300,300,600,600,50,300,300,50),ncol=2,nrow=4)
#' colnames(sqrcut) <- c("FSC-H","SSC-H")
#' pg <- polygonGate(filterId="nonDebris", .gate= sqrcut)
#' fortify(pg) 
fortify.polygonGate <- function(model, data = NULL, nPoints = NULL, ...){
  vertices <- model@boundaries
  chnls <- colnames(vertices)
  new.vertices <- rbind(vertices, vertices[1,])#make sure geom_path will enclose the polygon by ending with the starting point
  dt <- as.data.table(new.vertices)
  setnames(dt, chnls)
  dt
}
#' Convert a multiRangeGate to a data.table useful for ggplot
#' 
#' It converts the boundaries slot into a data.table
#' 
#' 
#' @param model multiRangeGate
#' @param data Not used
#' @param nPoints not used
#' @param ... not used.
#' 
#' @export
#' @return data.table
#' @examples 
#' mrq = multiRangeGate(ranges = list(min=c(100, 350), max=c(250, 400)))
#' fortify(mrq)
fortify.multiRangeGate<- function(model, data = NULL, ...){
  vertices <- model@ranges
  # Convert to 1D vector
  channel = parameters(model)
  vertices =unlist(mapply(function(x, y)c(x, y),vertices[["min"]], vertices[["max"]], SIMPLIFY=FALSE))
  dt <- as.data.table(vertices)
  setnames(dt, channel)
  dt
}

#' Convert a ellipsoidGate to a data.table useful for ggplot
#' 
#' It interpolates the ellipsoidGate to polygongate before fortifying it.
#' 
#' @param model ellipsoidGate
#' @param data data range used for polygon interpolation.
#' @param ... not used.
#' 
#' @export
#' @return data.table
#' @examples 
#' ## Defining the gate
#' cov <- matrix(c(6879, 3612, 3612, 5215), ncol=2,
#'               dimnames=list(c("FSC-H", "SSC-H"), c("FSC-H", "SSC-H")))
#' mean <- c("FSC-H"=430, "SSC-H"=175)
#' eg <- ellipsoidGate(filterId= "myEllipsoidGate", .gate=cov, mean=mean)
#' fortify(eg)
fortify.ellipsoidGate <- function(model, data = NULL, ...){
  poly.g <- as(model, "polygonGate")  
  fortify(poly.g, data = data, ...)
}

#' Convert a filterList to a data.table useful for ggplot
#' 
#' It tries to merge with pData that is associated with filterList as attribute 'pd'
  
#' @param model filterList
#' @param data not used
#' @param nPoints not used
#' @param ... not used.
#' 
#' @importFrom plyr name_rows
#' @export
#' @return data.table
#' @examples 
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' gates <- gs_pop_get_gate(gs, "CD4")
#' gates <- as(gates, "filterList") #must convert list to filterList in order for the method to dispatch properly
#' fortify(gates)
fortify.filterList <- function(model, data = NULL, nPoints = NULL, ...){
      # convert each filter to df
      df <- .ldply(model, fortify
                      # , data = data
                      # , nPoints = nPoints
                   , .id = ".rownames")
    
      pd <- attr(model,"pd")
      if(!is.null(pd)){
          # merge with pd
            
        if(!is(pd, "data.table"))
            pd <- .pd2dt(pd)
        df <- merge(df, pd, by = ".rownames")  
        attr(df, "annotated") <- TRUE
      }
      # attr(df, "nPoints") <- nPoints
      
    df
}

#' Convert a rectangleGate to a data.table useful for ggplot
#' 
#' For 2d rectangelGate, it is converted to a polygonGate first and then dispatch to the fortify method for polygonGate.
#' for 1d, uses geom_vline/hline format.
#' 
#' @param model rectangleGate
#' @param data data range used for polygon interpolation.
#' @param ... not used.
#' 
#' @export
#' @examples 
#' #2d rectangleGate
#' rect.g <- rectangleGate(list("FSC-H" =  c(300,500), "SSC-H" = c(50,200)))
#' fortify(rect.g)
#' #1d gate
#' rg <- rectangleGate(list("FSC-H" =  c(300,500)))
#' fortify(rg)
#' 
#' @return data.table
fortify.rectangleGate <- function(model, data = NULL, ...){
  
  param <- parameters(model)
  nDim <- length(param)
  if (nDim ==  2){
    fortify(as(model, "polygonGate"), data = data, ...)
  }else if(nDim ==  1){
    l.b <- model@min
    r.t <- model@max  
    
    coord <- c(l.b, r.t)
    
    df <- data.table(unname(coord), check.names = FALSE)
    setnames(df, "V1" , param)
    df
  }else
    stop("rectangelGate with dimension ", nDim, "is not supported!")

}

