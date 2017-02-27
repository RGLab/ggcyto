#' coerce the flowFrame to a data.table
#' 
#' It extracts the cell event matrix and convert it to a data.table.
#' 
#' @param x flowFrame
#' @return data.table
#' @import data.table
#' @export
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
  
  thisFilter <- attr(x, "filter")#must to get attr here before it is lost during subsetting
  
  # subset by columns if applicable
  dims <- attr(x, "dims")
  if(!is.null(dims))
    x <- x[, dims[, name]]
  
  if(!is.null(thisFilter)){
    if(is.function(thisFilter)){
      thisFilter <- thisFilter(x, dims[, name])
    }
    x <- Subset(x, thisFilter)
  }
    
  df.list <- .fsdply(x, .fr2dt, .id = ".rownames")
  
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
#' fs <- getData(gs, "CD8")
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
#' When 'nPoints' is supplied, the method tries to interpolate the polygon with more verticies.
#' 
#' 
#' @param model polygonGate
#' @param data data range used to reset off-bound gate coordinates to prevent interpolating on the extremely large space unnecessarily.
#' @param nPoints total number of vertices of the polygon after interpolation. Default is NULL, which is no interpolation.
#'                 The actual number may be more or less based on the lengths of edges due to the maximun and minimum limits on each edge.
#'                   Interpolation is mainly for the purpose of plotting (so that it won't lose its shape from subsetting through 'limits').
#'                    But it is not necessary for other purposes like centroid calculation.
#' @param ... not used.
#' 
#' @export
#' @return data.table
#' @examples 
#' sqrcut <- matrix(c(300,300,600,600,50,300,300,50),ncol=2,nrow=4)
#' colnames(sqrcut) <- c("FSC-H","SSC-H")
#' pg <- polygonGate(filterId="nonDebris", .gate= sqrcut)
#' fortify(pg) #no interpolation
#' fortify(pg, nPoints = 30) # with interpolation
fortify.polygonGate <- function(model, data = NULL, nPoints = NULL, ...){
  
  vertices <- model@boundaries
  chnls <- colnames(vertices)
  
  
  
  #reset the boundaries based on the current data range
  #to prevent it from interpolating on too large space
  #(thus lose the point when display is still at the scale of measured range)
  #such situations are caused by  the infinity vetices from rectangle or the extended vertices during the gate parsing
  #or the extreme coordinates stored in flowJo xml
  #measurement range won't work , since the actual data range (meaningful data) could be beyond the measure_range
  if(!is.null(data)&&!is.null(nPoints)){
    for(chnl in chnls){
      thisVal <- vertices[, chnl] 
      if(!chnl%in%colnames(data))
        stop("gate dimension ", chnl, " not found in data!")
      thisRg <- data[, chnl]
      vertices[thisVal < thisRg[1], chnl] <- thisRg[1]
      vertices[thisVal > thisRg[2], chnl] <- thisRg[2]
    }  
  }
  
  
  if(is.null(nPoints)){
    
    new.vertices <- rbind(vertices, vertices[1,])#make sure geom_path will enclose the polygon by ending with the starting point
  }else
  {
    

    nVert <- nrow(vertices)
    # compute distance between each pair of adjacent points
    edges.lengths <- sapply(1:nVert, function(i){
                     j <- ifelse(i < nVert, i + 1, 1)
                     dist(vertices[c(i, j), ])[[1]]
                  })
    total.length <- sum(edges.lengths)
    #determine the number of points to be interpolated for each edge
    edges.lengths.norm <- edges.lengths/total.length
    
    if(nVert > 5){
      #set cap for each edge to prevent the over-interpolation on the extended long edges
      #The critieria to tell if it is extended gate is currently loose
      #just by looking at the number of vertices, main idea is to exclude
      #the rectangle gate
      edges.lengths.norm <- sapply(edges.lengths.norm, function(i)min(i, 0.4))
    }
    
    nEdge.points <- round(edges.lengths.norm * nPoints)
    #set the lower limit to prevent under-interpolation on the short edges
    nEdge.points <- sapply(nEdge.points, function(i)max(i,20))

    #normalize data first
#     vertices <- scale(vertices)
#     sd <- attr(vertices, "scaled:scale")
#     mu <- attr(vertices, "scaled:center")
    
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
    
  
#     new.vertices <- sweep(new.vertices , 2L,  sd, "*")
#     new.vertices <- sweep(new.vertices , 2L,  mu, "+")
  
#     plot(vertices, type = "l")
#     polygon(new.vertices, col = "red")
  }
  
  dt <- as.data.table(new.vertices)
  setnames(dt, chnls)
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
#' @param data data range used for polygon interpolation
#' @param nPoints used for interpolating polygonGates to prevent it from losing shape when truncated by axis limits
#' @param ... not used.
#' 
#' @importFrom plyr name_rows
#' @export
#' @return data.table
#' @examples 
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' gates <- getGate(gs, "CD4")
#' gates <- as(gates, "filterList") #must convert list to filterList in order for the method to dispatch properly
#' fortify(gates)
fortify.filterList <- function(model, data = NULL, nPoints = NULL, ...){
      # convert each filter to df
      df <- .ldply(model, fortify
                      , data = data
                      , nPoints = nPoints, .id = ".rownames")
    
      pd <- attr(model,"pd")
      if(!is.null(pd)){
          # merge with pd
            
        if(!is(pd, "data.table"))
            pd <- .pd2dt(pd)
        df <- merge(df, pd, by = ".rownames")  
        attr(df, "annotated") <- TRUE
      }
      attr(df, "nPoints") <- nPoints
      
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

